# --- Classifying outcomes ----------------------------------------------------------------------

test_that("every kind of outcome is classified as the design says", {
  # status, remaining -> kind, permanent, retry
  table <- list(
    list(200, NA, "ok", FALSE, FALSE),
    list(204, NA, "ok", FALSE, FALSE),
    list(429, 12, "rate_limit", FALSE, TRUE),
    list(429, NA, "rate_limit", FALSE, TRUE),
    list(429, 0, "quota", FALSE, FALSE),
    list(500, NA, "server", FALSE, TRUE),
    list(503, NA, "server", FALSE, TRUE),
    list(401, NA, "auth", FALSE, FALSE),
    list(403, NA, "auth", FALSE, FALSE),
    list(400, NA, "bad_request", TRUE, FALSE),
    list(404, NA, "not_found", TRUE, FALSE),
    list(409, NA, "client", FALSE, FALSE),
    list(413, NA, "client", FALSE, FALSE),
    list(302, NA, "unexpected", FALSE, FALSE)
  )

  for (row in table) {
    got <- ors_classify(row[[1]], row[[2]])
    label <- sprintf("HTTP %s remaining %s", row[[1]], row[[2]])
    expect_equal(got$kind, row[[3]], label = label)
    expect_equal(got$permanent, row[[4]], label = label)
    expect_equal(got$retry, row[[5]], label = label)
  }
})

test_that("a network error is temporary and retried", {
  got <- ors_classify(NA_integer_, network_error = TRUE)

  expect_equal(got, list(kind = "network", permanent = FALSE, retry = TRUE))
  expect_equal(ors_classify(NA_integer_)$kind, "network")
})

test_that("only a request ORS cannot answer is permanent", {
  permanent <- vapply(
    c(200, 400, 401, 403, 404, 408, 429, 500, 502),
    function(s) ors_classify(s)$permanent, logical(1)
  )

  expect_equal(permanent, c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
})

# --- Reading ORS's explanation -----------------------------------------------------------------------

test_that("ORS's error message and code are extracted", {
  body <- list(error = list(code = 2010, message = "Could not find routable point"))

  expect_equal(ors_error_text(body, NA), "Could not find routable point (ORS code 2010)")
  expect_equal(ors_error_text(list(error = list(message = "Plain")), NA), "Plain")
})

test_that("an error given as plain text, or as a message, is extracted", {
  expect_equal(ors_error_text(list(error = "Access denied"), NA), "Access denied")
  expect_equal(ors_error_text(list(message = "Slow down"), NA), "Slow down")
})

test_that("without JSON the start of the raw text is used", {
  html <- paste0("<html>\n  <body>Bad Gateway ", strrep("x", 300), "</body></html>")

  got <- ors_error_text(NULL, html)

  expect_match(got, "^<html> <body>Bad Gateway")
  expect_lte(nchar(got), 200)
  expect_true(is.na(ors_error_text(NULL, NA_character_)))
  expect_true(is.na(ors_error_text(NULL, "   ")))
})

# --- Reading an httr response and the headers --------------------------------------------------------------

test_that("rate-limit headers are read regardless of letter case, and bad values become NA", {
  h <- structure(
    list(`X-RateLimit-Limit` = "200", `x-ratelimit-remaining` = "187", `X-Ratelimit-Reset` = "1794107462", `Retry-After` = "Wed, 21 Oct 2026 07:28:00 GMT"),
    class = c("insensitive", "list")
  )

  got <- ors_parse_headers(h)

  expect_equal(got$limit, 200)
  expect_equal(got$remaining, 187)
  expect_equal(got$reset, 1794107462)
  expect_true(is.na(got$retry_after))
})

test_that("no headers gives NA for everything", {
  got <- ors_parse_headers(NULL)

  expect_true(all(is.na(unlist(got))))
  expect_named(got, c("limit", "remaining", "reset", "retry_after"))
})

test_that("a JSON response is parsed", {
  resp <- fake_httr_response(
    200, '{"routes":[{"summary":{"distance":1000.5,"duration":600}}]}',
    headers = list(`x-ratelimit-limit` = "200", `x-ratelimit-remaining` = "199")
  )

  got <- ors_parse_response(resp)

  expect_equal(got$status, 200L)
  expect_equal(got$body$routes[[1]]$summary$distance, 1000.5)
  expect_equal(got$headers$remaining, 199)
  expect_true(is.na(got$error))
})

test_that("a response that is not JSON keeps its text and has no body", {
  resp <- fake_httr_response(502, "<html>Bad Gateway</html>", type = "text/html")

  got <- ors_parse_response(resp)

  expect_equal(got$status, 502L)
  expect_null(got$body)
  expect_equal(got$text, "<html>Bad Gateway</html>")
})

test_that("an empty response has no body", {
  got <- ors_parse_response(fake_httr_response(204, ""))

  expect_equal(got$status, 204L)
  expect_null(got$body)
})

# --- The single HTTP call (httr replaced) ----------------------------------------------------------------------------

test_that("a GET sends its query and returns the parsed response", {
  seen <- NULL
  testthat::local_mocked_bindings(
    GET = function(url, ...) {
      seen <<- list(url = url, args = list(...))
      fake_httr_response(200, '{"features":[]}')
    },
    .package = "httr"
  )

  got <- ors_http("GET", "https://example.test/geocode", query = list(text = "Laredo"))

  expect_equal(seen$url, "https://example.test/geocode")
  expect_equal(seen$args$query, list(text = "Laredo"))
  expect_equal(got$status, 200L)
  expect_equal(got$body, list(features = list()))
})

test_that("a POST sends its body as JSON and its headers", {
  seen <- NULL
  testthat::local_mocked_bindings(
    POST = function(url, ...) {
      seen <<- list(url = url, args = list(...))
      fake_httr_response(200, '{"routes":[]}')
    },
    .package = "httr"
  )

  got <- ors_http(
    "POST", "https://example.test/route",
    body = list(coordinates = list(c(-99.1, 19.4), c(-118.2, 34.0))),
    headers = c(Authorization = "abc")
  )
  header_args <- Filter(function(a) is.list(a) && !is.null(a$headers), seen$args)

  expect_equal(seen$args$encode, "json")
  expect_equal(seen$args$body$coordinates[[1]], c(-99.1, 19.4))
  expect_equal(header_args[[1]]$headers[["Authorization"]], "abc")
  expect_equal(got$status, 200L)
})

test_that("a network failure is reported, not raised", {
  testthat::local_mocked_bindings(
    GET = function(url, ...) stop("Could not resolve host: example.test"),
    .package = "httr"
  )

  got <- ors_http("GET", "https://example.test/x")

  expect_true(is.na(got$status))
  expect_match(got$error, "Could not resolve host")
  expect_null(got$body)
})

# --- Success ------------------------------------------------------------------------------------------------------------------

test_that("a successful request returns the body and uses one attempt", {
  log <- local_ors(list(raw_ok(list(routes = list(1)))))

  got <- ors_request("directions", "/v2/directions/driving-hgv", body = list(coordinates = list()))

  expect_true(got$ok)
  expect_equal(got$status, 200L)
  expect_equal(got$body, list(routes = list(1)))
  expect_true(is.na(got$error))
  expect_false(got$error_permanent)
  expect_equal(got$error_kind, "ok")
  expect_equal(got$attempts, 1)
  expect_equal(got$rate_limit, list(limit = 200, remaining = 199, reset = 1794107462))
  expect_length(log$calls, 1)
  expect_length(log$sleeps, 0)
})

test_that("the rate-limit numbers are remembered for each kind of request", {
  local_ors(list(
    raw_ok(headers = limits(200, 150)),
    raw_ok(headers = limits(100, 40))
  ))

  expect_null(ors_quota("directions"))
  ors_request("directions", "/v2/directions/driving-hgv")
  ors_request("geocode", "/geocode/search")

  expect_equal(ors_quota("directions")$remaining, 150)
  expect_equal(ors_quota("directions")$limit, 200)
  expect_equal(ors_quota("geocode")$remaining, 40)
  expect_equal(ors_quota("geocode")$limit, 100)
})

test_that("responses without rate-limit headers leave the remembered numbers alone", {
  local_ors(list(raw_ok(headers = limits(200, 150)), raw_ok(headers = no_limits())))

  ors_request("directions", "/x")
  ors_request("directions", "/x")

  expect_equal(ors_quota("directions")$remaining, 150)
})

# --- Where the key goes -------------------------------------------------------------------------------------------------------------

test_that("directions requests send the key in the Authorization header", {
  log <- local_ors(list(raw_ok()))

  ors_request("directions", "/v2/directions/driving-hgv", body = list(a = 1))
  call <- log$calls[[1]]

  expect_equal(call$method, "POST")
  expect_equal(call$url, "https://api.heigit.org/openrouteservice/v2/directions/driving-hgv")
  expect_equal(call$headers[["Authorization"]], ors_test_key)
  expect_null(call$query$api_key)
  expect_equal(call$body, list(a = 1))
})

test_that("geocoding requests send the key as the api_key parameter", {
  log <- local_ors(list(raw_ok()))

  ors_request("geocode", "/search", query = list(text = "Laredo", size = 1))
  call <- log$calls[[1]]

  expect_equal(call$method, "GET")
  expect_equal(call$url, "https://api.heigit.org/pelias/v1/search")
  expect_equal(call$query$api_key, ors_test_key)
  expect_equal(call$query$text, "Laredo")
  expect_equal(call$query$size, 1)
  expect_false("Authorization" %in% names(call$headers))
})

test_that("a missing key stops before anything is sent", {
  log <- local_ors(list(raw_ok()), key = NA)

  expect_error(ors_request("directions", "/x"), "TRUCKR_ORS_API_KEY")
  expect_error(ors_request("geocode", "/x"), ".Renviron", fixed = TRUE)
  expect_length(log$calls, 0)
})

test_that("the service addresses and timeout can be set, for example for a self-hosted server", {
  log <- local_ors(list(raw_ok(), raw_ok()))
  withr::local_options(
    truckr.ors_directions_url = "http://localhost:8080/ors/",
    truckr.ors_geocode_url = "http://localhost:8081/pelias/v1",
    truckr.http_timeout = 5
  )

  ors_request("directions", "/v2/directions/driving-hgv")
  ors_request("geocode", "/search")

  expect_equal(log$calls[[1]]$url, "http://localhost:8080/ors/v2/directions/driving-hgv")
  expect_equal(log$calls[[2]]$url, "http://localhost:8081/pelias/v1/search")
  expect_equal(log$calls[[1]]$timeout, 5)
})

test_that("the default addresses are the current api.heigit.org ones, not the deprecated host", {
  withr::local_options(truckr.ors_directions_url = NULL, truckr.ors_geocode_url = NULL)

  expect_equal(ors_service_url("directions"), "https://api.heigit.org/openrouteservice")
  expect_equal(ors_service_url("geocode"), "https://api.heigit.org/pelias/v1")
  expect_false(grepl("api.openrouteservice.org", ors_service_url("directions"), fixed = TRUE))
})

# --- Retrying -------------------------------------------------------------------------------------------------------------------------------

test_that("a rate limit (429) then success is retried once, with a message", {
  log <- local_ors(list(raw_status(429, headers = limits(200, 12)), raw_ok()))

  expect_message(got <- ors_request("directions", "/x"), "retrying in 2 s \\(retry 1 of 2\\)")

  expect_true(got$ok)
  expect_equal(got$attempts, 2)
  expect_length(log$calls, 2)
  expect_equal(log$sleeps, 2)
})

test_that("a server error is tried three times in total, then reported as temporary", {
  log <- local_ors(rep(list(raw_error(503, "Service Unavailable")), 3))

  msgs <- testthat::capture_messages(got <- ors_request("directions", "/x"))

  expect_false(got$ok)
  expect_equal(got$attempts, 3)
  expect_length(log$calls, 3)
  expect_equal(log$sleeps, c(2, 4))
  expect_length(msgs, 2)
  expect_equal(got$error_kind, "server")
  expect_false(got$error_permanent)
  expect_match(got$error, "ORS server error \\(HTTP 503\\): Service Unavailable")
  expect_match(got$error, "gave up after 3 attempts")
})

test_that("a network error is retried and can recover", {
  log <- local_ors(list(raw_network(), raw_network("Connection reset"), raw_ok()))

  suppressMessages(got <- ors_request("directions", "/x"))

  expect_true(got$ok)
  expect_equal(got$attempts, 3)
  expect_equal(log$sleeps, c(2, 4))
})

test_that("a network error that never recovers is a temporary failure", {
  local_ors(rep(list(raw_network("Timeout was reached")), 3))

  suppressMessages(got <- ors_request("directions", "/x"))

  expect_false(got$ok)
  expect_true(is.na(got$status))
  expect_equal(got$error_kind, "network")
  expect_false(got$error_permanent)
  expect_match(got$error, "network error: Timeout was reached")
})

test_that("backoff waits are capped at 30 seconds", {
  log <- local_ors(rep(list(raw_status(500)), 6))
  withr::local_options(truckr.http_attempts = 6, truckr.http_backoff_seconds = 10)

  suppressMessages(ors_request("directions", "/x"))

  expect_equal(log$sleeps, c(10, 20, 30, 30, 30))
})

test_that("a larger Retry-After from the server is honoured, up to a limit", {
  log <- local_ors(list(
    raw_status(429, headers = limits(200, 5, retry_after = 10)),
    raw_status(429, headers = limits(200, 5, retry_after = 500)),
    raw_ok()
  ))

  suppressMessages(ors_request("directions", "/x"))

  expect_equal(log$sleeps, c(10, 60))
})

test_that("a smaller Retry-After does not shorten the backoff", {
  log <- local_ors(list(raw_status(429, headers = limits(200, 5, retry_after = 1)), raw_ok()))

  suppressMessages(ors_request("directions", "/x"))

  expect_equal(log$sleeps, 2)
})

test_that("the number of attempts and the first wait are settings", {
  log <- local_ors(rep(list(raw_status(500)), 4))
  withr::local_options(truckr.http_attempts = 4, truckr.http_backoff_seconds = 5)

  suppressMessages(got <- ors_request("directions", "/x"))

  expect_equal(got$attempts, 4)
  expect_equal(log$sleeps, c(5, 10, 20))
})

test_that("one attempt in total means a failure is reported at once", {
  log <- local_ors(list(raw_status(500), raw_ok()))
  withr::local_options(truckr.http_attempts = 1)

  expect_no_message(got <- ors_request("directions", "/x"))

  expect_false(got$ok)
  expect_equal(got$attempts, 1)
  expect_length(log$calls, 1)
})

# --- Not retrying ------------------------------------------------------------------------------------------------------------------------------------

test_that("a request ORS cannot answer is permanent and not retried", {
  log <- local_ors(list(raw_error(404, "Could not find routable point within a radius of 350.0 meters", code = 2010)))

  expect_no_message(got <- ors_request("directions", "/x"))

  expect_false(got$ok)
  expect_equal(got$status, 404L)
  expect_true(got$error_permanent)
  expect_equal(got$error_kind, "not_found")
  expect_match(got$error, "ORS returned HTTP 404: Could not find routable point")
  expect_match(got$error, "ORS code 2010")
  expect_equal(got$attempts, 1)
  expect_length(log$calls, 1)
})

test_that("a bad request is permanent and not retried", {
  local_ors(list(raw_error(400, "Request parameters exceed the server configuration limits", code = 2004)))

  got <- ors_request("directions", "/x")

  expect_true(got$error_permanent)
  expect_equal(got$error_kind, "bad_request")
  expect_equal(got$attempts, 1)
})

test_that("a rejected key is temporary, not retried, and says what to check", {
  for (status in c(401, 403)) {
    log <- local_ors(list(raw_error(status, "Access to this API has been disallowed")))

    got <- ors_request("geocode", "/x")

    expect_false(got$error_permanent)
    expect_equal(got$error_kind, "auth")
    expect_equal(got$attempts, 1)
    expect_match(got$error, "rejected the API key")
    expect_match(got$error, "TRUCKR_ORS_API_KEY")
    expect_length(log$calls, 1)
  }
})

test_that("other 4xx responses are temporary and not retried", {
  log <- local_ors(list(raw_status(409, body = list(error = "Conflict"))))

  got <- ors_request("directions", "/x")

  expect_equal(got$error_kind, "client")
  expect_false(got$error_permanent)
  expect_equal(got$attempts, 1)
  expect_length(log$calls, 1)
})

test_that("a daily quota that has run out is temporary and is not retried", {
  log <- local_ors(list(raw_status(429, headers = limits(200, 0, reset = 1794107462))))

  expect_no_message(got <- ors_request("directions", "/x"))

  expect_false(got$ok)
  expect_equal(got$error_kind, "quota")
  expect_false(got$error_permanent)
  expect_equal(got$attempts, 1)
  expect_length(log$calls, 1)
  expect_match(got$error, "ORS quota exhausted \\(HTTP 429\\); it should reset around 20")
  expect_equal(got$rate_limit$remaining, 0)
  expect_equal(ors_quota("directions")$remaining, 0)
})

test_that("a 429 with no rate-limit headers is treated as a rate limit, not the quota", {
  log <- local_ors(list(raw_status(429), raw_ok()))

  suppressMessages(got <- ors_request("directions", "/x"))

  expect_true(got$ok)
  expect_length(log$calls, 2)
})

test_that("a success status with an unreadable body is a temporary failure", {
  log <- local_ors(list(raw_status(200, body = NULL, text = "not json at all")))

  got <- ors_request("directions", "/x")

  expect_false(got$ok)
  expect_equal(got$error_kind, "unparseable")
  expect_false(got$error_permanent)
  expect_match(got$error, "could not be read as JSON")
  expect_length(log$calls, 1)
})

test_that("an unexpected status is temporary and not retried", {
  log <- local_ors(list(raw_status(302)))

  got <- ors_request("directions", "/x")

  expect_equal(got$error_kind, "unexpected")
  expect_false(got$error_permanent)
  expect_match(got$error, "ORS returned HTTP 302")
  expect_length(log$calls, 1)
})

test_that("an error response with no explanation still gives a usable message", {
  local_ors(list(raw_status(500, text = NA_character_)))
  withr::local_options(truckr.http_attempts = 1)

  got <- ors_request("directions", "/x")

  expect_equal(got$error, "ORS server error (HTTP 500)")
})

# --- Spacing requests ----------------------------------------------------------------------------------------------------------------------------------------

test_that("the first request does not wait, and the next one is spaced at 40 per minute", {
  log <- local_ors(list(raw_ok(), raw_ok(), raw_ok()))

  ors_request("directions", "/x")
  expect_length(log$sleeps, 0)

  ors_request("directions", "/x")
  expect_equal(log$sleeps, 1.5)

  ors_request("directions", "/x")
  expect_equal(log$sleeps, c(1.5, 1.5))
})

test_that("no wait is added when enough time has already passed", {
  log <- local_ors(list(raw_ok(), raw_ok()))

  ors_request("directions", "/x")
  log$now <- log$now + 10
  ors_request("directions", "/x")

  expect_length(log$sleeps, 0)
})

test_that("only the remaining part of the gap is waited", {
  log <- local_ors(list(raw_ok(), raw_ok()))

  ors_request("directions", "/x")
  log$now <- log$now + 0.5
  ors_request("directions", "/x")

  expect_equal(log$sleeps, 1)
})

test_that("the requests-per-minute limit is a setting", {
  log <- local_ors(list(raw_ok(), raw_ok()))
  withr::local_options(truckr.ors_requests_per_minute = 10)

  ors_request("directions", "/x")
  ors_request("directions", "/x")

  expect_equal(log$sleeps, 6)
})

test_that("geocoding and directions requests share the spacing", {
  log <- local_ors(list(raw_ok(), raw_ok()))

  ors_request("geocode", "/x")
  ors_request("directions", "/x")

  expect_equal(log$sleeps, 1.5)
})

# --- The key never appears (Property 12) -----------------------------------------------------------------------------------------------------------------------

test_that("the key is removed from a network error message, the retry notice and the result", {
  leaky <- paste0(
    "Failed to connect to api.openrouteservice.org: GET https://api.openrouteservice.org/geocode/search?api_key=",
    ors_test_key, "&text=Laredo"
  )
  local_ors(rep(list(raw_network(leaky)), 3))

  msgs <- testthat::capture_messages(got <- ors_request("geocode", "/geocode/search"))

  expect_false(grepl(ors_test_key, got$error, fixed = TRUE))
  expect_false(any(grepl(ors_test_key, msgs, fixed = TRUE)))
  expect_match(got$error, "[API key removed]", fixed = TRUE)
  expect_false(grepl(ors_test_key, paste(unlist(got), collapse = " "), fixed = TRUE))
})

test_that("the key is removed when ORS echoes it back in an error", {
  local_ors(list(raw_error(400, paste("Bad request for key", ors_test_key))))

  got <- ors_request("directions", "/x")

  expect_false(grepl(ors_test_key, got$error, fixed = TRUE))
  expect_match(got$error, "[API key removed]", fixed = TRUE)
})

# --- Settings ----------------------------------------------------------------------------------------------------------------------------------------------------------

test_that("bad settings are reported before anything is sent", {
  log <- local_ors(list(raw_ok()))

  withr::local_options(truckr.http_attempts = 0)
  expect_error(ors_request("directions", "/x"), "`truckr.http_attempts` must be a single positive whole number")
  withr::local_options(truckr.http_attempts = 1.5)
  expect_error(ors_request("directions", "/x"), "whole number")
  withr::local_options(truckr.http_attempts = 3, truckr.http_backoff_seconds = 0)
  expect_error(ors_request("directions", "/x"), "`truckr.http_backoff_seconds` must be a single positive number")
  withr::local_options(truckr.http_backoff_seconds = 2, truckr.ors_requests_per_minute = "fast")
  expect_error(ors_request("directions", "/x"), "truckr.ors_requests_per_minute")
  withr::local_options(truckr.ors_requests_per_minute = 40, truckr.ors_directions_url = "ftp://x")
  expect_error(ors_request("directions", "/x"), "`truckr.ors_directions_url` must be a web address starting with http:// or https://")
  withr::local_options(truckr.ors_directions_url = NULL, truckr.ors_geocode_url = "localhost")
  expect_error(ors_request("geocode", "/x"), "`truckr.ors_geocode_url` must be a web address")
  expect_length(log$calls, 0)
})

test_that("an unknown kind of request is rejected", {
  local_ors(list(raw_ok()))

  expect_error(ors_request("weather", "/x"), "should be one of")
})
