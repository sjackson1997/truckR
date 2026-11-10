# OpenRouteService HTTP layer --------------------------------------------------
#
# All traffic to ORS passes through ors_request(). It sits on top of ors_http(),
# which makes exactly one HTTP call and is the only function that touches the
# network, so tests replace ors_http() and never need a connection.
#
#   ors_request()  key handling, spacing between requests, retries with
#                  backoff, failure classification, rate-limit bookkeeping,
#                  and removal of the API key from every message
#   ors_http()     one HTTP call, returned as a plain list
#
# What a failure means for the cache is decided here: every failure is marked
# temporary or permanent (error_permanent), because temporary failures are
# never stored (Requirement 14.13). Anything unclear is temporary.

.truckr_ors <- new.env(parent = emptyenv())

ors_state_init <- function(env) {
  env$last_request <- NULL  # time of the last request, in seconds
  env$quota <- list()       # "directions"/"geocode" -> last rate-limit headers seen
  invisible(env)
}
ors_state_init(.truckr_ors)

# Unverified: forum sources report 40 requests per minute on the free plan.
ors_default_requests_per_minute <- 40


# Settings -------------------------------------------------------------------------

numeric_option <- function(name, default, positive = TRUE, whole = FALSE) {
  value <- getOption(name, default)
  ok <- is.numeric(value) && length(value) == 1 && !is.na(value) &&
    (if (positive) value > 0 else value >= 0) &&
    (!whole || value == round(value))
  if (!ok) {
    stop(
      sprintf(
        "Option `%s` must be a single %s%s number.",
        name, if (positive) "positive" else "non-negative", if (whole) " whole" else ""
      ),
      call. = FALSE
    )
  }
  value
}

#' The web address of an ORS service
#'
#' ORS moved its public API from api.openrouteservice.org (deprecated since
#' April 2026, restricted to 10 percent of the quota, and to be shut down in
#' early November 2026) to api.heigit.org, where each service has its own
#' address: routing under /openrouteservice and geocoding under /pelias/v1.
#' Both are settings so a self-hosted server can be used, for example
#' \code{options(truckr.ors_directions_url = "http://localhost:8080/ors")}.
#' @noRd
ors_service_url <- function(kind = c("directions", "geocode")) {
  kind <- match.arg(kind)
  option <- if (kind == "directions") "truckr.ors_directions_url" else "truckr.ors_geocode_url"
  default <- if (kind == "directions") {
    "https://api.heigit.org/openrouteservice"
  } else {
    "https://api.heigit.org/pelias/v1"
  }

  url <- getOption(option, default)
  if (!is.character(url) || length(url) != 1 || is.na(url) || !grepl("^https?://", url)) {
    stop(sprintf("Option `%s` must be a web address starting with http:// or https://.", option), call. = FALSE)
  }
  sub("/+$", "", url)
}

ors_requests_per_minute <- function() {
  numeric_option("truckr.ors_requests_per_minute", ors_default_requests_per_minute)
}

#' The most times one request is tried in total: the first attempt plus the
#' retries. The default of 3 means the first request and up to two retries.
#' @noRd
ors_max_attempts <- function() {
  numeric_option("truckr.http_attempts", 3, positive = TRUE, whole = TRUE)
}

#' Seconds to wait before the first retry; each retry waits twice as long
#' @noRd
ors_backoff_seconds <- function() {
  numeric_option("truckr.http_backoff_seconds", 2)
}

ors_timeout_seconds <- function() {
  numeric_option("truckr.http_timeout", 30)
}

# The longest we will wait between retries, however large Retry-After is.
ors_max_wait_seconds <- 60
ors_backoff_cap_seconds <- 30

ors_check_settings <- function() {
  ors_service_url("directions")
  ors_service_url("geocode")
  ors_requests_per_minute()
  ors_max_attempts()
  ors_backoff_seconds()
  ors_timeout_seconds()
  invisible(TRUE)
}


# Time ---------------------------------------------------------------------------------
# Separate functions so tests can replace them and never wait.

ors_now <- function() {
  as.numeric(Sys.time())
}

ors_sleep <- function(seconds) {
  if (seconds > 0) Sys.sleep(seconds)
  invisible(seconds)
}


# One HTTP call -----------------------------------------------------------------------------

#' Make one HTTP call
#'
#' The only function that touches the network. Never stops on a network
#' problem; reports it in \code{error}.
#'
#' @param method \code{"GET"} or \code{"POST"}.
#' @param url The full address.
#' @param query Query parameters (a list).
#' @param body For POST: the body, sent as JSON.
#' @param headers A named character vector of extra headers.
#' @param timeout Seconds before giving up.
#'
#' @return A list: \code{status} (integer, \code{NA} if no response arrived),
#'   \code{body} (the parsed JSON, or \code{NULL}), \code{text} (the response
#'   text), \code{headers} (see \code{ors_parse_response()}) and \code{error}
#'   (\code{NA}, or the network error message).
#' @noRd
ors_http <- function(method, url, query = list(), body = NULL, headers = character(0), timeout = 30) {
  resp <- tryCatch(
    if (identical(method, "GET")) {
      httr::GET(
        url, query = query,
        httr::user_agent("truckR (R package)"),
        httr::timeout(timeout),
        httr::add_headers(.headers = c(Accept = "application/json", headers))
      )
    } else {
      httr::POST(
        url, query = query, body = body, encode = "json",
        httr::user_agent("truckR (R package)"),
        httr::timeout(timeout),
        httr::add_headers(.headers = c(Accept = "application/json", headers))
      )
    },
    error = function(e) e
  )

  if (inherits(resp, "error")) {
    return(list(
      status = NA_integer_, body = NULL, text = NA_character_,
      headers = ors_parse_headers(NULL), error = conditionMessage(resp)
    ))
  }
  ors_parse_response(resp)
}

#' Turn an httr response into the plain list used by ors_request()
#' @noRd
ors_parse_response <- function(resp) {
  text <- tryCatch(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    error = function(e) NA_character_
  )
  body <- NULL
  if (!is.na(text) && nzchar(text)) {
    body <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  }
  list(
    status = as.integer(httr::status_code(resp)),
    body = body,
    text = text,
    headers = ors_parse_headers(httr::headers(resp)),
    error = NA_character_
  )
}

#' Pick out the rate-limit headers
#'
#' Header names are matched without regard to case. Values that are missing or
#' are not numbers (for example a Retry-After given as a date) are \code{NA}.
#' @noRd
ors_parse_headers <- function(h) {
  h <- if (is.null(h)) list() else unclass(h)
  lowered <- tolower(names(h))
  number <- function(name) {
    value <- h[lowered == name]
    if (length(value) == 0) return(NA_real_)
    suppressWarnings(as.numeric(as.character(value[[1]])[1]))
  }
  list(
    limit = number("x-ratelimit-limit"),
    remaining = number("x-ratelimit-remaining"),
    reset = number("x-ratelimit-reset"),
    retry_after = number("retry-after")
  )
}


# Judging a response ----------------------------------------------------------------------------

#' Decide what an outcome means
#'
#' @param status The HTTP status, or \code{NA} when no response arrived.
#' @param remaining The \code{x-ratelimit-remaining} header, or \code{NA}.
#' @param network_error \code{TRUE} when the request failed before a response.
#'
#' @return A list: \code{kind}, \code{permanent} (retrying can never help and
#'   the failure may be remembered for the session), \code{retry} (worth trying
#'   again now).
#'
#' Rules: network errors, timeouts, 5xx and rate limits (429) are temporary
#' and retried. A 429 with no quota remaining is the daily quota running out:
#' temporary, but retrying within seconds cannot help, so it is not retried.
#' 400 and 404 mean the request itself cannot be answered (for example no
#' route or no match) and are permanent. 401 and 403 mean the key is wrong or
#' disabled, which is fixed by the user, so they are temporary and not retried.
#' Every other status is temporary and not retried.
#' @noRd
ors_classify <- function(status, remaining = NA_real_, network_error = FALSE) {
  result <- function(kind, permanent = FALSE, retry = FALSE) {
    list(kind = kind, permanent = permanent, retry = retry)
  }

  if (network_error || is.na(status)) return(result("network", retry = TRUE))
  if (status >= 200 && status < 300) return(result("ok"))
  if (status == 429) {
    if (isTRUE(remaining == 0)) return(result("quota"))
    return(result("rate_limit", retry = TRUE))
  }
  if (status >= 500 && status < 600) return(result("server", retry = TRUE))
  if (status %in% c(401, 403)) return(result("auth"))
  if (status == 400) return(result("bad_request", permanent = TRUE))
  if (status == 404) return(result("not_found", permanent = TRUE))
  if (status >= 400 && status < 500) return(result("client"))
  result("unexpected")
}

#' Find ORS's own explanation in an error response
#'
#' ORS puts it in \code{error$message} (with a numeric \code{error$code}), or
#' sometimes in \code{error} as plain text. Falls back to the start of the raw
#' text.
#' @return A string, or \code{NA}.
#' @noRd
ors_error_text <- function(body, text) {
  found <- NULL
  if (is.list(body)) {
    err <- body[["error"]]
    if (is.list(err)) {
      msg <- err[["message"]]
      code <- err[["code"]]
      if (is.character(msg) && length(msg) == 1) {
        found <- if (!is.null(code) && length(code) == 1) sprintf("%s (ORS code %s)", msg, code) else msg
      }
    } else if (is.character(err) && length(err) == 1) {
      found <- err
    } else if (is.character(body[["message"]]) && length(body[["message"]]) == 1) {
      found <- body[["message"]]
    }
  }
  if (is.null(found) && !is.null(text) && !is.na(text) && nzchar(trimws(text))) {
    found <- substr(gsub("\\s+", " ", trimws(text)), 1, 200)
  }
  if (is.null(found)) NA_character_ else found
}

ors_describe_failure <- function(kind, status, detail, headers) {
  with_detail <- function(prefix) {
    if (!is.na(detail) && nzchar(detail)) paste0(prefix, ": ", detail) else prefix
  }

  switch(kind,
    network = paste0("network error: ", detail),
    quota = {
      when <- tryCatch(
        format(as.POSIXct(headers$reset, origin = "1970-01-01", tz = ""), "%Y-%m-%d %H:%M"),
        error = function(e) NA_character_
      )
      paste0(
        "ORS quota exhausted (HTTP 429)",
        if (!is.na(headers$reset) && !is.na(when)) paste0("; it should reset around ", when) else ""
      )
    },
    rate_limit = with_detail("ORS rate limit reached (HTTP 429)"),
    server = with_detail(sprintf("ORS server error (HTTP %s)", status)),
    auth = with_detail(sprintf(
      "ORS rejected the API key (HTTP %s); check that TRUCKR_ORS_API_KEY is correct and the key is active",
      status
    )),
    with_detail(sprintf("ORS returned HTTP %s", status))
  )
}

#' Turn one raw outcome into a result, with its failure classified and the API
#' key removed from the message
#' @noRd
ors_interpret <- function(raw) {
  net <- !is.na(raw$error)
  class <- ors_classify(raw$status, raw$headers$remaining, network_error = net)

  out <- list(
    ok = FALSE, status = raw$status, body = raw$body, error = NA_character_,
    error_permanent = class$permanent, error_kind = class$kind,
    retry = class$retry, retry_after = raw$headers$retry_after
  )

  if (class$kind == "ok") {
    if (is.null(raw$body)) {
      out$error <- "ORS returned a response that could not be read as JSON"
      out$error_kind <- "unparseable"
      return(out)
    }
    out$ok <- TRUE
    return(out)
  }

  detail <- if (net) raw$error else ors_error_text(raw$body, raw$text)
  out$error <- redact_key(ors_describe_failure(class$kind, raw$status, detail, raw$headers), "ors")
  out
}


# Rate limits ---------------------------------------------------------------------------------------

#' Wait if needed so requests stay under the requests-per-minute limit
#'
#' Requests are spaced evenly: 40 per minute means at least 1.5 seconds between
#' requests. The first request never waits.
#' @noRd
ors_throttle <- function() {
  last <- .truckr_ors$last_request
  if (!is.null(last)) {
    wait <- last + 60 / ors_requests_per_minute() - ors_now()
    if (wait > 0) ors_sleep(wait)
  }
  .truckr_ors$last_request <- ors_now()
  invisible(TRUE)
}

#' Remember the rate-limit headers of the latest response
#' @noRd
ors_note_rate_limit <- function(kind, headers) {
  if (all(is.na(c(headers$limit, headers$remaining, headers$reset)))) {
    return(invisible(FALSE))
  }
  .truckr_ors$quota[[kind]] <- list(
    limit = headers$limit,
    remaining = headers$remaining,
    reset = headers$reset,
    observed_at = ors_now()
  )
  invisible(TRUE)
}

#' The latest rate-limit numbers ORS reported
#'
#' Used by the quota pre-flight (Task 11).
#' @param kind \code{"directions"} or \code{"geocode"}.
#' @return A list with \code{limit}, \code{remaining}, \code{reset} (a Unix
#'   time) and \code{observed_at}, or \code{NULL} if nothing has been seen yet
#'   this session.
#' @noRd
ors_quota <- function(kind = c("directions", "geocode")) {
  kind <- match.arg(kind)
  .truckr_ors$quota[[kind]]
}


# The request ------------------------------------------------------------------------------------------

#' Send a request to ORS
#'
#' Reads the key from \code{TRUCKR_ORS_API_KEY} (stopping with a clear message
#' if it is missing, before anything is sent), keeps requests under the rate
#' limit, retries temporary failures with exponential backoff, and returns a
#' result that says what happened. It does not stop on a failed request: the
#' failure is described in the result so that only the affected lane is
#' affected.
#'
#' Retries: a network error, a 5xx, or a 429 that is not the daily quota
#' running out is retried until \code{truckr.http_attempts} attempts have been
#' made in total (default 3: the first request and two retries), with waits of
#' 2 and then 4 seconds (\code{truckr.http_backoff_seconds}, doubled each time,
#' at most 30 seconds). A larger \code{Retry-After} from the server is
#' honoured, up to 60 seconds. Each retry prints a message. Other 4xx responses
#' and an exhausted daily quota are not retried.
#'
#' Where the key goes: directions requests send it in the \code{Authorization}
#' header; geocoding requests send it as the \code{api_key} query parameter
#' (both confirmed in the spike). It never appears in the result or in any
#' message.
#'
#' @param kind \code{"directions"} (a POST with a JSON body) or
#'   \code{"geocode"} (a GET).
#' @param path The path after the service address (see
#'   \code{ors_service_url()}): \code{"/v2/directions/driving-hgv"} for
#'   directions, \code{"/search"} for geocoding.
#' @param query Query parameters (a list).
#' @param body For directions: the request body (a list, sent as JSON).
#'
#' @return A list with \code{ok}, \code{status}, \code{body} (parsed JSON),
#'   \code{error} (\code{NA} on success), \code{error_permanent},
#'   \code{error_kind} (\code{"ok"}, \code{"network"}, \code{"quota"},
#'   \code{"rate_limit"}, \code{"server"}, \code{"auth"}, \code{"bad_request"},
#'   \code{"not_found"}, \code{"client"}, \code{"unparseable"} or
#'   \code{"unexpected"}), \code{attempts} and \code{rate_limit} (the limit,
#'   remaining and reset values from the last response).
#' @noRd
ors_request <- function(kind = c("directions", "geocode"), path, query = list(), body = NULL) {
  kind <- match.arg(kind)
  ors_check_settings()
  key <- get_api_key("ors")

  url <- paste0(ors_service_url(kind), path)
  if (kind == "geocode") {
    method <- "GET"
    query <- c(list(api_key = key), query)
    headers <- character(0)
  } else {
    method <- "POST"
    headers <- c(Authorization = key)
  }

  retries <- ors_max_attempts() - 1L
  attempt <- 0L
  repeat {
    ors_throttle()
    raw <- ors_http(
      method = method, url = url, query = query, body = body,
      headers = headers, timeout = ors_timeout_seconds()
    )
    ors_note_rate_limit(kind, raw$headers)
    out <- ors_interpret(raw)
    attempt <- attempt + 1L

    if (out$ok || !out$retry) break
    if (attempt > retries) {
      if (attempt > 1) {
        out$error <- paste0(out$error, sprintf(" (gave up after %d attempts)", attempt))
      }
      break
    }

    wait <- min(ors_backoff_seconds() * 2^(attempt - 1), ors_backoff_cap_seconds)
    if (!is.na(out$retry_after)) wait <- max(wait, min(out$retry_after, ors_max_wait_seconds))
    message(sprintf(
      "ORS request failed (%s); retrying in %s s (retry %d of %d).",
      out$error, format(round(wait, 1)), attempt, retries
    ))
    ors_sleep(wait)
  }

  list(
    ok = out$ok,
    status = out$status,
    body = out$body,
    error = out$error,
    error_permanent = out$error_permanent,
    error_kind = out$error_kind,
    attempts = attempt,
    rate_limit = raw$headers[c("limit", "remaining", "reset")]
  )
}
