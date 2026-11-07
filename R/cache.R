# Cache layer ----------------------------------------------------------------
#
# The cache stores answers a provider has already given (geocode results and
# route-segment distances), so the package never spends API quota on the same
# answer twice. It has two layers that work together:
#
#   * memory: an environment inside the running R session;
#   * disk:   one file per provider and kind under the user cache folder.
#
# Lookup order is memory, then disk, then the provider. See design.md,
# "Component 8", for the full rules (when data moves between the layers, how
# entries expire, and which failures are stored).

.truckr_cache <- new.env(parent = emptyenv())

cache_state_init <- function(env) {
  env$mem <- new.env(parent = emptyenv())  # store id -> environment of entries
  env$stores <- list()                     # store id -> store description
  env$loaded <- character(0)               # store ids already read from disk
  env$dirty <- list()                      # store id -> keys not yet on disk
  env$warned <- FALSE                      # warned about a failed write?
  invisible(env)
}
cache_state_init(.truckr_cache)

cache_file_pattern <- "^[a-z][a-z0-9_]*\\.(geocode|route)(\\.[^.]+)?\\.rds$"


# Settings --------------------------------------------------------------------

#' The folder that holds the disk cache
#' @noRd
cache_dir <- function() {
  dir <- getOption("truckr.cache_dir")
  if (is.null(dir)) dir <- tools::R_user_dir("truckR", "cache")
  dir
}

#' Is the disk layer on? Returns "disk" or "session".
#' @noRd
cache_mode <- function() {
  mode <- getOption("truckr.cache", "disk")
  if (!is.character(mode) || length(mode) != 1 || is.na(mode) || !mode %in% c("disk", "session")) {
    stop("Option `truckr.cache` must be \"disk\" or \"session\".", call. = FALSE)
  }
  mode
}

#' Days after which a cached answer is refreshed
#' @noRd
cache_max_age_days <- function() {
  days <- getOption("truckr.cache_max_age_days", 120)
  if (!is.numeric(days) || length(days) != 1 || is.na(days) || days <= 0) {
    stop("Option `truckr.cache_max_age_days` must be a single positive number.", call. = FALSE)
  }
  days
}

#' How many new answers are collected before they are written to disk
#' @noRd
cache_flush_every <- function() {
  n <- getOption("truckr.cache_flush_every", 25L)
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1 || n != round(n)) {
    stop("Option `truckr.cache_flush_every` must be a single whole number of at least 1.", call. = FALSE)
  }
  n
}

#' Check every setting, so a bad option is reported at once and not only when
#' an entry happens to be looked at
#' @noRd
cache_check_settings <- function() {
  cache_mode()
  cache_max_age_days()
  cache_flush_every()
  invisible(TRUE)
}

#' The current time in seconds. Tests replace the clock with the option
#' `truckr.cache_clock` (a function returning a time) so that "120 days later"
#' does not require waiting.
#' @noRd
cache_now <- function() {
  clock <- getOption("truckr.cache_clock")
  as.numeric(if (is.function(clock)) clock() else Sys.time())
}


# Stores and keys ----------------------------------------------------------------

#' Describe one cache store
#'
#' A store holds one provider's answers of one kind: \code{"geocode"}, or
#' \code{"route"} for one routing profile. Each store is one file on disk.
#'
#' @param provider A provider name.
#' @param kind \code{"geocode"} or \code{"route"}.
#' @param profile The routing profile (route stores only). \code{"default"} when
#'   not given.
#'
#' @return A list with \code{id}, \code{provider}, \code{kind}, \code{profile}.
#' @noRd
cache_store <- function(provider, kind, profile = NULL) {
  validate_provider_name(provider)
  kind <- match.arg(kind, c("geocode", "route"))

  if (kind == "route") {
    if (is.null(profile) || length(profile) != 1 || is.na(profile) || !nzchar(profile)) {
      profile <- "default"
    }
    profile_part <- gsub("[^A-Za-z0-9_-]", "_", profile)
  } else {
    profile <- NA_character_
    profile_part <- NULL
  }

  list(
    id = paste(c(provider, kind, profile_part), collapse = "."),
    provider = provider,
    kind = kind,
    profile = profile
  )
}

cache_file <- function(store) {
  file.path(cache_dir(), paste0(store$id, ".rds"))
}

check_cache_key <- function(key) {
  if (!is.character(key) || length(key) != 1 || is.na(key) || !nzchar(key)) {
    stop("A cache key must be a single non-empty string.", call. = FALSE)
  }
  invisible(key)
}

#' Cache key for a route segment
#'
#' Route answers are keyed by the coordinates of the two ends (rounded to five
#' decimals, about a metre), not by the location text. If a place is later
#' geocoded to somewhat different coordinates, its routes are requested again
#' instead of being reused for a point that has moved.
#'
#' @param from,to Coordinates: a named numeric vector with \code{lon} and
#'   \code{lat}, or two unnamed numbers in the order longitude, latitude.
#'
#' @return A string.
#' @noRd
route_cache_key <- function(from, to) {
  point <- function(p, what) {
    if (!is.null(names(p)) && all(c("lon", "lat") %in% names(p))) p <- p[c("lon", "lat")]
    p <- suppressWarnings(as.numeric(unname(p)))
    if (length(p) != 2 || anyNA(p)) {
      stop(sprintf("`%s` must be two numbers: longitude and latitude.", what), call. = FALSE)
    }
    sprintf("%.5f,%.5f", p[1], p[2])
  }
  paste0(point(from, "from"), ">", point(to, "to"))
}


# Memory layer ----------------------------------------------------------------------

cache_mem <- function(store) {
  env <- get0(store$id, envir = .truckr_cache$mem, inherits = FALSE)
  if (is.null(env)) {
    env <- new.env(parent = emptyenv())
    assign(store$id, env, envir = .truckr_cache$mem)
    .truckr_cache$stores[[store$id]] <- store
  }
  env
}

cache_expired <- function(entry, now = cache_now()) {
  (now - entry[["obtained"]]) > cache_max_age_days() * 86400
}

valid_cache_entry <- function(entry) {
  is.list(entry) &&
    is.numeric(entry[["obtained"]]) && length(entry[["obtained"]]) == 1 && !is.na(entry[["obtained"]]) &&
    "value" %in% names(entry)
}


# Disk layer -----------------------------------------------------------------------------

#' Read a store's file
#'
#' @return The entries (a named list). An empty list when there is no file, or
#'   when the file cannot be used; in that case a message says so unless
#'   \code{quiet}.
#' @noRd
read_cache_file <- function(store, quiet = FALSE) {
  path <- cache_file(store)
  if (!file.exists(path)) {
    return(list())
  }

  problem <- NULL
  obj <- tryCatch(readRDS(path), error = function(e) {
    problem <<- conditionMessage(e)
    NULL
  })
  if (is.null(problem)) {
    if (!is.list(obj) || !identical(obj[["version"]], 1L)) {
      problem <- "it was not written by this version of the package"
    } else if (!is.list(obj[["entries"]]) ||
               (length(obj[["entries"]]) > 0 && is.null(names(obj[["entries"]])))) {
      problem <- "its contents are not in the expected form"
    }
  }
  if (!is.null(problem)) {
    if (!quiet) {
      message(sprintf(
        "The cache file %s could not be used (%s); it will be ignored and rebuilt.",
        path, problem
      ))
    }
    return(list())
  }

  Filter(valid_cache_entry, obj[["entries"]])
}

#' Write a store's file
#'
#' The data is written to a temporary file in the same folder and then renamed
#' over the real file, so a crash part-way through cannot leave a damaged file.
#' @noRd
write_cache_file <- function(store, entries) {
  dir <- cache_dir()
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(dir)) {
    stop("the cache folder could not be created", call. = FALSE)
  }

  obj <- list(
    version = 1L,
    provider = store$provider,
    kind = store$kind,
    profile = store$profile,
    entries = entries
  )
  path <- cache_file(store)
  tmp <- tempfile(pattern = paste0(store$id, "-"), tmpdir = dir, fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)

  saveRDS(obj, tmp)
  moved <- suppressWarnings(file.rename(tmp, path))
  if (!moved && !file.copy(tmp, path, overwrite = TRUE)) {
    stop("the cache file could not be replaced", call. = FALSE)
  }
  invisible(TRUE)
}

#' Load a store from disk into memory, once per session
#'
#' Entries that have already expired are not loaded.
#' @noRd
cache_ensure_loaded <- function(store) {
  if (cache_mode() != "disk" || store$id %in% .truckr_cache$loaded) {
    return(invisible(FALSE))
  }

  entries <- read_cache_file(store)
  env <- cache_mem(store)
  now <- cache_now()
  for (key in names(entries)) {
    entry <- entries[[key]]
    if (!cache_expired(entry, now) && !exists(key, envir = env, inherits = FALSE)) {
      assign(key, entry, envir = env)
    }
  }
  .truckr_cache$loaded <- c(.truckr_cache$loaded, store$id)
  invisible(TRUE)
}


# Lookups and saves ---------------------------------------------------------------------------

#' Look for a saved answer
#'
#' Looks in memory, then on disk (the file is read into memory the first time
#' it is needed). An answer older than the maximum age counts as missing.
#'
#' @param store A store from \code{cache_store()}.
#' @param key The answer's key.
#'
#' @return \code{NULL} when there is no usable answer. Otherwise a list with
#'   \code{value} (what the provider returned), \code{obtained} (seconds) and
#'   \code{failure} (\code{TRUE} for a remembered permanent failure).
#' @noRd
cache_get <- function(store, key) {
  check_cache_key(key)
  cache_check_settings()
  cache_ensure_loaded(store)

  entry <- get0(key, envir = cache_mem(store), inherits = FALSE)
  if (is.null(entry) || cache_expired(entry)) {
    return(NULL)
  }
  entry
}

#' Does a provider result describe a failure?
#' @noRd
result_failed <- function(value) {
  err <- value[["error"]]
  length(err) > 0 && any(!is.na(err) & nzchar(as.character(err)))
}

#' Remove the provider's API key from every piece of text in a result
#' @noRd
redact_cache_value <- function(x, provider) {
  if (is.character(x)) {
    return(redact_key(x, provider))
  }
  if (is.list(x)) {
    x[] <- lapply(x, redact_cache_value, provider = provider)
  }
  x
}

#' Save a provider's answer
#'
#' What is kept depends on the result (Requirement 14.13):
#' \itemize{
#'   \item a success is saved in memory and queued for the disk cache;
#'   \item a failure the provider marked \code{error_permanent = TRUE} is kept
#'     in memory only, for this session;
#'   \item any other failure (a network error, a timeout, a rate limit, a
#'     server error, exhausted quota, or a failure not marked permanent) is not
#'     stored anywhere.
#' }
#' Text in the result is passed through \code{redact_key()} first, so an API
#' key can never end up in the cache.
#'
#' New answers are written to disk in batches (see \code{cache_mark_dirty()}).
#'
#' @param store A store from \code{cache_store()}.
#' @param key The answer's key.
#' @param value The provider's result: a list. A failure has a non-empty
#'   \code{error}.
#'
#' @return Invisibly: \code{"stored"}, \code{"memory_only"} or
#'   \code{"not_stored"}.
#' @noRd
cache_put <- function(store, key, value) {
  check_cache_key(key)
  cache_check_settings()
  if (!is.list(value)) {
    stop("`value` must be a list (a provider result).", call. = FALSE)
  }

  failed <- result_failed(value)
  if (failed && !isTRUE(value[["error_permanent"]])) {
    return(invisible("not_stored"))
  }

  entry <- list(
    value = redact_cache_value(value, store$provider),
    obtained = cache_now(),
    failure = failed
  )
  assign(key, entry, envir = cache_mem(store))

  if (failed) {
    return(invisible("memory_only"))
  }
  if (cache_mode() == "disk") {
    cache_mark_dirty(store, key)
  }
  invisible("stored")
}

#' Queue a new answer for the disk cache, writing a batch when enough are waiting
#' @noRd
cache_mark_dirty <- function(store, key) {
  keys <- union(.truckr_cache$dirty[[store$id]], key)
  .truckr_cache$dirty[[store$id]] <- keys
  if (length(keys) >= cache_flush_every()) {
    cache_flush(store)
  }
  invisible(TRUE)
}

#' Write a store's waiting answers to disk
#'
#' The file is read again just before it is written, and the waiting answers
#' are added to what it holds (the newer answer wins). That way answers saved
#' by another R session are not lost. Answers older than the maximum age are
#' dropped. If the file cannot be written, a warning is given once per session,
#' the answers stay in memory, and the write is retried next time.
#'
#' @return Invisibly, \code{TRUE} if the file was written.
#' @noRd
cache_flush <- function(store) {
  keys <- .truckr_cache$dirty[[store$id]]
  if (length(keys) == 0 || cache_mode() != "disk") {
    return(invisible(FALSE))
  }
  env <- cache_mem(store)

  written <- tryCatch({
    entries <- read_cache_file(store, quiet = store$id %in% .truckr_cache$loaded)
    for (key in keys) {
      entry <- get0(key, envir = env, inherits = FALSE)
      if (is.null(entry) || isTRUE(entry[["failure"]])) next
      old <- entries[[key]]
      if (is.null(old) || old[["obtained"]] <= entry[["obtained"]]) {
        entries[[key]] <- entry
      }
    }
    now <- cache_now()
    still_good <- !vapply(entries, cache_expired, logical(1), now = now)
    write_cache_file(store, entries[still_good])
    TRUE
  }, error = function(e) {
    if (!isTRUE(.truckr_cache$warned)) {
      warning(
        sprintf(
          paste0(
            "Could not write the cache for provider `%s` (%s). ",
            "Answers are kept in memory for this session and the write will be retried."
          ),
          store$provider, conditionMessage(e)
        ),
        call. = FALSE
      )
      .truckr_cache$warned <- TRUE
    }
    FALSE
  })

  if (written) {
    .truckr_cache$dirty[[store$id]] <- character(0)
  }
  invisible(written)
}

#' Write every store's waiting answers to disk
#'
#' Call this when a run ends, including when it stops early; \code{route()}
#' does so with \code{on.exit()}.
#'
#' @return Invisibly, the number of stores written.
#' @noRd
cache_flush_all <- function() {
  n <- 0L
  for (id in names(.truckr_cache$dirty)) {
    if (length(.truckr_cache$dirty[[id]]) == 0) next
    store <- .truckr_cache$stores[[id]]
    if (!is.null(store) && isTRUE(cache_flush(store))) n <- n + 1L
  }
  invisible(n)
}

#' Forget everything held in memory
#'
#' With \code{flush = TRUE} (the default) waiting answers are written to disk
#' first. Used to start a "new session" in tests and by
#' \code{truckr_cache_clear(what = "memory")}.
#' @noRd
cache_reset_memory <- function(flush = TRUE) {
  if (flush) cache_flush_all()
  cache_state_init(.truckr_cache)
}


# For users ------------------------------------------------------------------------------------

#' See what is in the cache
#'
#' Reports the answers saved on disk, one row per provider and kind of answer.
#' It also prints the cache folder and whether the disk cache is on.
#'
#' The cache saves what providers have already answered (geocoding results and
#' route distances and durations), so repeated runs spend no API quota on them.
#' It never stores an API key. Answers older than 120 days are treated as
#' missing and fetched again.
#'
#' Four options change the behaviour:
#' \itemize{
#'   \item \code{truckr.cache}: \code{"disk"} (the default) saves answers in
#'     files that last between R sessions; \code{"session"} keeps them in
#'     memory only;
#'   \item \code{truckr.cache_max_age_days}: how old an answer may be before it
#'     is fetched again (default 120);
#'   \item \code{truckr.cache_dir}: the folder for the cache files (default
#'     \code{tools::R_user_dir("truckR", "cache")});
#'   \item \code{truckr.cache_flush_every}: how many new answers are collected
#'     before they are written to disk (default 25).
#' }
#' Waiting answers are also written whenever a run ends or stops early.
#'
#' @return A data frame with one row per saved set of answers and the columns
#'   \code{provider}, \code{kind} (\code{"geocode"} or \code{"route"}),
#'   \code{profile}, \code{entries} (answers in the file), \code{expired} (how
#'   many of them are older than the maximum age), \code{unsaved} (answers in
#'   memory not yet written), \code{oldest_days} and \code{size_bytes}. The
#'   cache folder is in the \code{"cache_dir"} attribute.
#' @seealso \code{\link{truckr_cache_clear}}
#' @export
#'
#' @examples
#' truckr_cache_info()
truckr_cache_info <- function() {
  dir <- cache_dir()
  now <- cache_now()
  limit <- cache_max_age_days() * 86400

  files <- if (dir.exists(dir)) {
    list.files(dir, pattern = cache_file_pattern, full.names = TRUE)
  } else {
    character(0)
  }
  ids <- sub("\\.rds$", "", basename(files))
  waiting <- names(Filter(function(k) length(k) > 0, .truckr_cache$dirty))
  memory_only <- setdiff(waiting, ids)

  describe <- function(id, path = NA_character_) {
    parts <- strsplit(id, ".", fixed = TRUE)[[1]]
    entries <- NULL
    if (!is.na(path)) {
      obj <- tryCatch(readRDS(path), error = function(e) NULL)
      if (is.list(obj) && identical(obj[["version"]], 1L) && is.list(obj[["entries"]])) {
        entries <- Filter(valid_cache_entry, obj[["entries"]])
      }
    }
    obtained <- vapply(entries, function(e) e[["obtained"]], numeric(1))
    dplyr::tibble(
      provider = parts[1],
      kind = parts[2],
      profile = if (length(parts) >= 3) paste(parts[-(1:2)], collapse = ".") else NA_character_,
      entries = if (is.null(entries)) (if (is.na(path)) 0L else NA_integer_) else length(entries),
      expired = if (is.null(entries)) (if (is.na(path)) 0L else NA_integer_) else sum((now - obtained) > limit),
      unsaved = length(.truckr_cache$dirty[[id]]),
      oldest_days = if (length(obtained) > 0) round((now - min(obtained)) / 86400, 1) else NA_real_,
      size_bytes = if (is.na(path)) NA_real_ else file.size(path)
    )
  }

  rows <- c(
    Map(describe, ids, files, USE.NAMES = FALSE),
    lapply(memory_only, describe)
  )
  out <- if (length(rows) == 0) {
    dplyr::tibble(
      provider = character(), kind = character(), profile = character(),
      entries = integer(), expired = integer(), unsaved = integer(),
      oldest_days = double(), size_bytes = double()
    )
  } else {
    dplyr::bind_rows(rows)
  }

  attr(out, "cache_dir") <- dir
  message(sprintf(
    "Cache folder: %s (disk cache is %s)",
    dir, if (cache_mode() == "disk") "on" else "off"
  ))
  out
}

#' Empty the cache
#'
#' Removes saved answers so they are fetched from the provider again. Only the
#' package's own cache files are touched; other files in the folder are left
#' alone. Deleting the cache folder by hand has the same effect as clearing the
#' disk.
#'
#' @param provider A provider name to clear only that provider's answers, or
#'   \code{NULL} (the default) for all providers.
#' @param what \code{"both"} (the default) clears memory and disk;
#'   \code{"memory"} forgets what this R session holds (answers waiting to be
#'   saved are written to disk first, so nothing is lost, and the files are
#'   read again when needed); \code{"disk"} deletes the cache files.
#'
#' @return Invisibly, a named integer vector: the number of answers dropped
#'   from memory (\code{memory_entries}) and files deleted (\code{disk_files}).
#' @seealso \code{\link{truckr_cache_info}}
#' @export
#'
#' @examples
#' \dontrun{
#' truckr_cache_clear()
#' truckr_cache_clear("ors", what = "disk")
#' }
truckr_cache_clear <- function(provider = NULL, what = c("both", "memory", "disk")) {
  what <- match.arg(what)
  if (!is.null(provider)) validate_provider_name(provider)

  memory_entries <- 0L
  disk_files <- 0L

  if (what == "memory") {
    cache_flush_all()
  }

  if (what %in% c("memory", "both")) {
    for (id in ls(.truckr_cache$mem, all.names = TRUE)) {
      store <- .truckr_cache$stores[[id]]
      if (!is.null(provider) && !identical(store$provider, provider)) next
      memory_entries <- memory_entries +
        length(ls(get(id, envir = .truckr_cache$mem), all.names = TRUE))
      rm(list = id, envir = .truckr_cache$mem)
      .truckr_cache$stores[[id]] <- NULL
      .truckr_cache$dirty[[id]] <- NULL
      .truckr_cache$loaded <- setdiff(.truckr_cache$loaded, id)
    }
  }

  if (what %in% c("disk", "both")) {
    dir <- cache_dir()
    files <- if (dir.exists(dir)) list.files(dir, pattern = cache_file_pattern, full.names = TRUE) else character(0)
    if (!is.null(provider)) {
      files <- files[startsWith(basename(files), paste0(provider, "."))]
    }
    disk_files <- sum(file.remove(files))
  }

  message(sprintf(
    "Cleared %d answer(s) from memory and %d cache file(s) from disk.",
    memory_entries, disk_files
  ))
  invisible(c(memory_entries = memory_entries, disk_files = as.integer(disk_files)))
}
