# Helpers for the cache tests.

# Isolate a test: a temporary cache folder, empty in-memory state, and a fake
# clock the test can move forward. Everything is put back when the test ends,
# so the real cache is never touched.
local_cache <- function(env = parent.frame(), start = "2026-01-01 00:00:00") {
  dir <- withr::local_tempdir(.local_envir = env)

  clock <- new.env()
  clock$now <- as.numeric(as.POSIXct(start, tz = "UTC"))
  clock$advance <- function(days = 0, seconds = 0) {
    clock$now <- clock$now + days * 86400 + seconds
    invisible(clock$now)
  }

  withr::local_options(
    list(
      truckr.cache_dir = dir,
      truckr.cache = "disk",
      truckr.cache_max_age_days = 120,
      truckr.cache_flush_every = 25L,
      truckr.cache_clock = function() clock$now
    ),
    .local_envir = env
  )

  saved <- as.list(.truckr_cache, all.names = TRUE)
  cache_state_init(.truckr_cache)
  withr::defer(
    for (name in names(saved)) assign(name, saved[[name]], envir = .truckr_cache),
    envir = env
  )

  invisible(list(dir = dir, clock = clock))
}

# A successful provider result.
ok_result <- function(x = 1) {
  list(lon = x, lat = 2, error = NA_character_, error_permanent = FALSE)
}

# A failed provider result.
bad_result <- function(message = "boom", permanent = NA) {
  out <- list(lon = NA_real_, lat = NA_real_, error = message)
  if (!is.na(permanent)) out$error_permanent <- permanent
  out
}

# The keys in a store's file on disk (empty if there is no file).
disk_keys <- function(store) {
  sort(names(read_cache_file(store, quiet = TRUE)))
}
