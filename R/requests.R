# Requests with a cache and a quota ---------------------------------------------------
#
# One engine does the work that is the same for geocoding and for routing: for
# each distinct thing to look up, use the saved answer if there is one, otherwise
# ask the provider, save the answer, and stop cleanly when the quota is used up
# or the provider is clearly not working. Everything already paid for is kept
# (the cache is written when the run ends, however it ends), so running again
# later finishes the job without repeating any work.

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Look things up, using saved answers where there are any
#'
#' Lookups that are already saved cost nothing. The rest are requested one at a
#' time, in order. Before the first request a message says how many requests
#' are needed and, if the provider can say, how many remain today. The run
#' stops early, without an error, when:
#' \itemize{
#'   \item the quota that remains is less than one request (checked before every
#'     request, using the provider's \code{remaining} function), or a request
#'     comes back as "quota exhausted";
#'   \item the provider rejects the API key (there is no point in going on);
#'   \item several requests in a row fail for a temporary reason (default 5, the
#'     option \code{truckr.max_consecutive_failures}), which means an outage and
#'     not a problem with one item. A failure that is a definite answer, such as
#'     "no route found", does not count.
#' }
#' Items not requested get a failure result saying why, so the lanes that depend
#' on them can say so, and are not stored, so a later run requests them.
#'
#' A request that fails does not stop the run (Requirement 12.4); a temporary
#' failure is not saved, a permanent one is remembered for the session only, and
#' a success is saved as usual (see \code{cache_put()}).
#'
#' @param items A list whose names are the cache keys and whose elements are what
#'   to pass to \code{fetch}. Names must be unique.
#' @param store A cache store from \code{cache_store()}.
#' @param fetch A function of one argument (an element of \code{items}) that
#'   asks the provider and returns its result: a list with \code{error},
#'   \code{error_permanent} and \code{error_kind} in addition to the answer.
#'   If it stops with an error, that is treated as a temporary failure.
#' @param remaining \code{NULL}, or a function with no arguments returning how
#'   many requests are left today (\code{NA} if not known).
#' @param cost The number of requests one item is expected to need. Default 1.
#' @param label A word for the messages, such as "Routing".
#' @param provider_name If given, the provider's API key is removed from any
#'   error message.
#'
#' @return A list with \code{results} (a named list, one result per item, in the
#'   order of \code{items}), \code{status} (a named character vector: "cache" for
#'   a saved answer, "fetched" for a successful request, "failed" for a request
#'   that failed, "stopped" for an item not requested because the run stopped),
#'   \code{stop_reason} (\code{NA}, "quota", "auth" or "repeated"),
#'   \code{requested}, \code{saved} and \code{not_requested} (counts).
#' @noRd
cached_requests <- function(items, store, fetch, remaining = NULL, cost = 1,
                            label = "Requests", provider_name = NULL) {
  keys <- names(items)
  if (!is.list(items) || (length(items) > 0 && (is.null(keys) || anyNA(keys) || !all(nzchar(keys)) || anyDuplicated(keys) > 0))) {
    stop("`items` must be a list whose names are unique, non-empty cache keys.", call. = FALSE)
  }
  if (!is.function(fetch)) {
    stop("`fetch` must be a function.", call. = FALSE)
  }
  keys <- keys %||% character(0)
  max_consecutive <- numeric_option("truckr.max_consecutive_failures", 5, whole = TRUE)
  progress_every <- numeric_option("truckr.progress_every", 25, whole = TRUE)

  # Whatever happens below, write the answers already obtained.
  on.exit(cache_flush_all(), add = TRUE)

  results <- stats::setNames(vector("list", length(keys)), keys)
  status <- stats::setNames(rep(NA_character_, length(keys)), keys)

  for (key in keys) {
    entry <- cache_get(store, key)
    if (!is.null(entry)) {
      results[[key]] <- entry$value
      status[[key]] <- "cache"
    }
  }
  todo <- keys[is.na(status)]
  n_saved <- length(keys) - length(todo)

  if (length(keys) == 0) {
    return(request_summary(results, status, NA_character_, 0L, 0L))
  }
  if (length(todo) == 0) {
    message(sprintf("%s: all %s are already saved; no requests needed.", label, count_text(length(keys))))
    return(request_summary(results, status, NA_character_, 0L, n_saved))
  }

  needed <- length(todo) * cost
  left <- ask_remaining(remaining)
  message(preflight_message(label, length(keys), n_saved, needed, left))

  stop_reason <- NA_character_
  consecutive <- 0L
  requested <- 0L
  told_quota <- !is.na(left)

  for (i in seq_along(todo)) {
    key <- todo[i]

    left <- ask_remaining(remaining)
    if (!told_quota && !is.na(left)) {
      told_quota <- TRUE
      still_needed <- (length(todo) - i + 1) * cost
      if (left < still_needed) {
        message(sprintf(
          "%s: about %s requests remain today and %s more are needed; the run will stop when the quota is used up and can be run again after it resets.",
          label, format(left, big.mark = ","), format(still_needed, big.mark = ",")
        ))
      }
    }
    if (!is.na(left) && left < cost) {
      stop_reason <- "quota"
      break
    }

    res <- run_fetch(fetch, items[[key]], provider_name)
    cache_put(store, key, res)
    requested <- requested + 1L

    failed <- result_failed(res)
    kind <- res[["error_kind"]] %||% NA_character_
    if (!failed) {
      results[[key]] <- res
      status[[key]] <- "fetched"
      consecutive <- 0L
    } else if (identical(kind, "quota")) {
      stop_reason <- "quota"
      break
    } else if (identical(kind, "auth")) {
      results[[key]] <- res
      status[[key]] <- "failed"
      stop_reason <- "auth"
      break
    } else {
      results[[key]] <- res
      status[[key]] <- "failed"
      consecutive <- if (isTRUE(res[["error_permanent"]])) 0L else consecutive + 1L
      if (consecutive >= max_consecutive) {
        stop_reason <- "repeated"
        break
      }
    }

    if (requested %% progress_every == 0 && i < length(todo)) {
      message(sprintf("%s: %s of %s requested.", label, count_text(requested), count_text(length(todo))))
    }
  }

  # Anything not requested says why.
  waiting <- keys[is.na(status)]
  if (length(waiting) > 0) {
    stopped <- stopped_result(stop_reason, max_consecutive)
    for (key in waiting) {
      results[[key]] <- stopped
      status[[key]] <- "stopped"
    }
  }

  message(finish_message(label, requested, n_saved, length(waiting), stop_reason))
  request_summary(results, status, stop_reason, requested, n_saved)
}


request_summary <- function(results, status, stop_reason, requested, saved) {
  list(
    results = results, status = status, stop_reason = stop_reason,
    requested = requested, saved = saved, not_requested = sum(status == "stopped", na.rm = TRUE)
  )
}

count_text <- function(n) format(n, big.mark = ",")

ask_remaining <- function(remaining) {
  if (!is.function(remaining)) {
    return(NA_real_)
  }
  value <- tryCatch(remaining(), error = function(e) NA_real_)
  if (!is.numeric(value) || length(value) != 1 || is.na(value)) NA_real_ else value
}

run_fetch <- function(fetch, item, provider_name) {
  res <- tryCatch(
    fetch(item),
    error = function(e) {
      text <- conditionMessage(e)
      if (!is.null(provider_name)) text <- redact_key(text, provider_name)
      list(error = paste0("provider error: ", text), error_permanent = FALSE, error_kind = "provider_error")
    }
  )
  if (!is.list(res)) {
    res <- list(
      error = "the provider returned an unusable result",
      error_permanent = FALSE, error_kind = "unusable"
    )
  }
  res
}

#' The result given to an item that was not requested
#' @noRd
stopped_result <- function(reason, max_consecutive) {
  text <- switch(
    reason %||% "quota",
    quota = "quota exhausted; re-run after reset",
    auth = "not requested: the API key was rejected",
    repeated = sprintf("not requested: stopped after %d requests in a row failed", max_consecutive),
    "not requested"
  )
  list(error = text, error_permanent = FALSE, error_kind = reason %||% "quota")
}

preflight_message <- function(label, total, saved, needed, left) {
  base <- sprintf(
    "%s: %s request%s needed (%s of %s already saved)",
    label, count_text(needed), if (needed == 1) "" else "s", count_text(saved), count_text(total)
  )
  if (is.na(left)) {
    return(paste0(base, "; the remaining quota is not known yet."))
  }
  if (needed <= left) {
    return(paste0(base, sprintf("; about %s remain today.", count_text(left))))
  }
  paste0(base, sprintf(
    "; only about %s remain today, so as many as fit will be requested now and the rest will wait for a re-run after the quota resets.",
    count_text(left)
  ))
}

finish_message <- function(label, requested, saved, not_requested, stop_reason) {
  if (not_requested > 0) {
    why <- switch(
      stop_reason %||% "quota",
      quota = "the quota is used up",
      auth = "the API key was rejected",
      repeated = "several requests in a row failed",
      "the run stopped"
    )
    return(sprintf(
      "%s: stopped early because %s. %s requested, %s not requested. Answers already obtained are saved, so running again finishes the rest.",
      label, why, count_text(requested), count_text(not_requested)
    ))
  }
  sprintf("%s: %s requested, %s already saved.", label, count_text(requested), count_text(saved))
}


# Routing the segments ----------------------------------------------------------------------------

#' Find the distance and time of every segment
#'
#' Takes the de-duplicated segments from \code{build_segments()} and the
#' geocoded locations, and returns the table that \code{assemble_results()}
#' needs. Each distinct pair of coordinates is requested once, however many
#' segments use it, and saved answers are used first.
#'
#' Segments that are not requested, and why:
#' \itemize{
#'   \item a segment marked \code{same_place} is left out altogether (its length
#'     is 0 and \code{assemble_results()} knows it);
#'   \item a segment whose two ends have the same coordinates (to five decimals)
#'     gets distance 0 and time 0 without a request, even if the two locations
#'     were written differently;
#'   \item a segment with an end that could not be geocoded gets an error saying
#'     which end and why, and is not requested.
#' }
#'
#' @param segments The \code{segments} table from \code{build_segments()}.
#' @param geo A named list of geocoding results, named by location key, each
#'   with \code{lon}, \code{lat} and, on failure, \code{error}.
#' @param provider A provider as returned by \code{resolve_provider()}: a list
#'   with \code{name} and \code{provider}.
#'
#' @return A list with \code{seg_results}, a data frame with \code{SegmentId},
#'   \code{distance_m}, \code{duration_s}, \code{error} (\code{NA} when fine),
#'   \code{error_permanent}, \code{error_kind} and \code{source} (where the
#'   answer came from), and \code{summary}, a list with \code{stop_reason},
#'   \code{requested}, \code{saved} and \code{not_requested}.
#' @noRd
route_segments <- function(segments, geo, provider) {
  needed <- c("SegmentId", "from_key", "to_key", "same_place")
  if (!is.data.frame(segments) || !all(needed %in% names(segments))) {
    stop("`segments` must be the segments table from build_segments().", call. = FALSE)
  }
  if (!is.list(geo)) {
    stop("`geo` must be a named list of geocoding results.", call. = FALSE)
  }

  p <- provider$provider
  segs <- segments[!segments$same_place, , drop = FALSE]
  n <- nrow(segs)

  out <- data.frame(
    SegmentId = segs$SegmentId,
    distance_m = rep(NA_real_, n), duration_s = rep(NA_real_, n),
    error = rep(NA_character_, n), error_permanent = rep(FALSE, n),
    error_kind = rep(NA_character_, n), source = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )

  end_problem <- function(g, what) {
    if (!is.null(g) && is.list(g) && is.finite(g[["lon"]] %||% NA_real_) && is.finite(g[["lat"]] %||% NA_real_)) {
      return(NULL)
    }
    reason <- if (is.list(g)) g[["error"]] else NULL
    list(
      text = sprintf("the %s point could not be located: %s", what, if (is.null(reason) || is.na(reason)) "no coordinates were found" else reason),
      permanent = !is.list(g) || isTRUE(g[["error_permanent"]]) || is.null(g)
    )
  }

  items <- list()
  keys <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    from_geo <- geo[[segs$from_key[i]]]
    to_geo <- geo[[segs$to_key[i]]]
    problems <- Filter(Negate(is.null), list(end_problem(from_geo, "start"), end_problem(to_geo, "end")))
    if (length(problems) > 0) {
      out$error[i] <- paste(vapply(problems, function(x) x$text, character(1)), collapse = "; ")
      out$error_permanent[i] <- all(vapply(problems, function(x) isTRUE(x$permanent), logical(1)))
      out$error_kind[i] <- "geocode"
      out$source[i] <- "not_requested"
      next
    }

    from <- c(from_geo$lon, from_geo$lat)
    to <- c(to_geo$lon, to_geo$lat)
    key <- route_cache_key(from, to)
    if (identical(round(from, 5), round(to, 5))) {
      out$distance_m[i] <- 0
      out$duration_s[i] <- 0
      out$error_kind[i] <- "ok"
      out$source[i] <- "same_point"
      next
    }
    keys[i] <- key
    if (is.null(items[[key]])) items[[key]] <- list(from = from, to = to)
  }

  store <- cache_store(provider$name, "route", p[["profile"]])
  remaining <- NULL
  if (is.function(p[["remaining_quota"]])) {
    remaining <- function() p[["remaining_quota"]]("route")
  }
  fetch <- function(item) p$route_segment(item$from, item$to)
  engine <- cached_requests(
    items, store, fetch, remaining = remaining, cost = 1,
    label = "Routing", provider_name = provider$name
  )

  for (i in which(!is.na(keys))) {
    res <- engine$results[[keys[i]]]
    failed <- result_failed(res)
    out$distance_m[i] <- if (failed) NA_real_ else (res[["distance_m"]] %||% NA_real_)
    out$duration_s[i] <- if (failed) NA_real_ else (res[["duration_s"]] %||% NA_real_)
    out$error[i] <- if (failed) as.character(res[["error"]][1]) else NA_character_
    out$error_permanent[i] <- failed && isTRUE(res[["error_permanent"]])
    out$error_kind[i] <- res[["error_kind"]] %||% (if (failed) NA_character_ else "ok")
    out$source[i] <- engine$status[[keys[i]]]
  }

  list(
    seg_results = out,
    summary = engine[c("stop_reason", "requested", "saved", "not_requested")]
  )
}
