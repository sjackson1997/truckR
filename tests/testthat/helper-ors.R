# Helpers for the ORS HTTP layer tests.

ors_test_key <- "sk-test-ORS-KEY-9876543210abcdef"

no_limits <- function() {
  list(limit = NA_real_, remaining = NA_real_, reset = NA_real_, retry_after = NA_real_)
}

limits <- function(limit = 200, remaining = 199, reset = 1794107462, retry_after = NA_real_) {
  list(limit = limit, remaining = remaining, reset = reset, retry_after = retry_after)
}

# What ors_http() returns, built by hand ------------------------------------------------

raw_ok <- function(body = list(ok = TRUE), headers = limits()) {
  list(
    status = 200L, body = body, text = jsonlite::toJSON(body, auto_unbox = TRUE),
    headers = headers, error = NA_character_
  )
}

raw_status <- function(status, body = NULL, headers = no_limits(), text = NA_character_) {
  list(status = as.integer(status), body = body, text = text, headers = headers, error = NA_character_)
}

raw_error <- function(status, message, code = NULL, headers = no_limits()) {
  err <- list(message = message)
  if (!is.null(code)) err$code <- code
  raw_status(status, body = list(error = err), headers = headers)
}

raw_network <- function(message = "Timeout was reached") {
  list(status = NA_integer_, body = NULL, text = NA_character_, headers = no_limits(), error = message)
}

# Set up one test of ors_request() ---------------------------------------------------------
#
# - the API key is a fake one;
# - all settings are at their defaults whatever the user has set;
# - the clock is fake: waiting moves it forward without waiting, and every wait
#   is recorded in log$sleeps;
# - ors_http() answers from `responses`, one per call, and records each call in
#   log$calls.
local_ors <- function(responses = list(), env = parent.frame(), key = ors_test_key) {
  withr::local_envvar(TRUCKR_ORS_API_KEY = key, .local_envir = env)
  withr::local_options(
    list(
      truckr.ors_base_url = NULL,
      truckr.ors_requests_per_minute = NULL,
      truckr.http_retries = NULL,
      truckr.http_backoff_seconds = NULL,
      truckr.http_timeout = NULL
    ),
    .local_envir = env
  )

  saved <- as.list(.truckr_ors, all.names = TRUE)
  ors_state_init(.truckr_ors)
  withr::defer(
    {
      ors_state_init(.truckr_ors)
      for (name in names(saved)) assign(name, saved[[name]], envir = .truckr_ors)
    },
    envir = env
  )

  log <- new.env()
  log$calls <- list()
  log$sleeps <- numeric(0)
  log$now <- 1e9
  log$queue <- responses

  testthat::local_mocked_bindings(
    ors_now = function() log$now,
    ors_sleep = function(seconds) {
      log$sleeps <- c(log$sleeps, seconds)
      log$now <- log$now + seconds
      invisible(seconds)
    },
    ors_http = function(method, url, query = list(), body = NULL, headers = character(0), timeout = 30) {
      log$calls[[length(log$calls) + 1]] <- list(
        method = method, url = url, query = query, body = body, headers = headers, timeout = timeout
      )
      if (length(log$queue) == 0) stop("the test ran out of scripted responses")
      reply <- log$queue[[1]]
      log$queue <- log$queue[-1]
      reply
    },
    .env = env
  )

  invisible(log)
}

# A response object like the ones httr returns -------------------------------------------------

fake_httr_response <- function(status = 200L, text = "{}", headers = list(), type = "application/json") {
  structure(
    list(
      url = "https://example.test/",
      status_code = as.integer(status),
      headers = structure(
        c(list(`content-type` = paste0(type, "; charset=utf-8")), headers),
        class = c("insensitive", "list")
      ),
      content = charToRaw(text),
      date = Sys.time(),
      times = c(),
      request = NULL,
      cookies = NULL,
      handle = NULL
    ),
    class = "response"
  )
}
