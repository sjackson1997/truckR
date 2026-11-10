laredo <- c(-99.5075, 27.5036)
san_antonio <- c(-98.4936, 29.4241)
result_names <- c("distance_m", "duration_s", "error", "error_permanent", "error_kind")

# --- Checking coordinates ----------------------------------------------------------------------

test_that("unnamed coordinates are longitude, then latitude", {
  expect_equal(lonlat_vector(c(-99.1, 19.4)), c(-99.1, 19.4))
})

test_that("names put the coordinates in the right order, whichever way they are written", {
  expect_equal(lonlat_vector(c(lat = 19.4, lon = -99.1)), c(-99.1, 19.4))
  expect_equal(lonlat_vector(c(latitude = 19.4, longitude = -99.1)), c(-99.1, 19.4))
  expect_equal(lonlat_vector(list(lat = 19.4, lng = -99.1)), c(-99.1, 19.4))
  expect_equal(lonlat_vector(c(LAT = 19.4, LON = -99.1)), c(-99.1, 19.4))
})

test_that("naming only one coordinate is an error", {
  expect_error(lonlat_vector(c(lat = 19.4, x = -99.1), "from"), "`from` names only one")
})

test_that("other names are ignored and the order is used", {
  expect_equal(lonlat_vector(c(x = -99.1, y = 19.4)), c(-99.1, 19.4))
})

test_that("the wrong number of values, missing values and text are rejected", {
  for (bad in list(1, c(1, 2, 3), numeric(0), c(NA, 4), c("a", "b"), NULL)) {
    expect_error(lonlat_vector(bad, "to"), "`to` must be two numbers")
  }
})

test_that("values outside the valid range are rejected, which catches a latitude/longitude swap", {
  expect_error(lonlat_vector(c(19.4, -99.1)), "outside the valid range.*longitude first")  # lat, lon
  expect_error(lonlat_vector(c(200, 0)), "outside the valid range")
  expect_error(lonlat_vector(c(0, 91)), "outside the valid range")
  expect_error(lonlat_vector(c(Inf, 0)), "outside the valid range")
  expect_equal(lonlat_vector(c(180, 90)), c(180, 90))
  expect_equal(lonlat_vector(c(-180, -90)), c(-180, -90))
})

# --- A successful route (captured live) ---------------------------------------------------------------

test_that("the live success response gives meters and seconds", {
  fixture <- read_fixture("ors_directions_success")
  local_ors(list(fixture))

  got <- ors_route_segment(laredo, san_antonio)

  expect_named(got, result_names)
  expect_equal(got$distance_m, fixture$body$routes[[1]]$summary$distance)
  expect_equal(got$duration_s, fixture$body$routes[[1]]$summary$duration)
  expect_true(is.na(got$error))
  expect_false(got$error_permanent)
  expect_equal(got$error_kind, "ok")
})

test_that("the values are plausible for Laredo to San Antonio in miles and hours", {
  local_ors(list(read_fixture("ors_directions_success")))

  got <- ors_route_segment(laredo, san_antonio)

  expect_gt(meters_to_miles(got$distance_m), 150)
  expect_lt(meters_to_miles(got$distance_m), 165)
  expect_gt(seconds_to_hours(got$duration_s), 2.5)
  expect_lt(seconds_to_hours(got$duration_s), 3.5)
})

test_that("the request goes to the directions service with the key in the header", {
  log <- local_ors(list(read_fixture("ors_directions_success")))

  ors_route_segment(laredo, san_antonio)
  call <- log$calls[[1]]

  expect_equal(call$method, "POST")
  expect_equal(call$url, "https://api.heigit.org/openrouteservice/v2/directions/driving-hgv")
  expect_equal(call$headers[["Authorization"]], ors_test_key)
  expect_length(log$calls, 1)
})

test_that("the request body has longitude first and switches geometry and instructions off", {
  log <- local_ors(list(read_fixture("ors_directions_success")))

  ors_route_segment(laredo, san_antonio)
  json <- as.character(jsonlite::toJSON(log$calls[[1]]$body, auto_unbox = TRUE, digits = NA))

  expect_equal(
    json,
    '{"coordinates":[[-99.5075,27.5036],[-98.4936,29.4241]],"geometry":false,"instructions":false}'
  )
})

test_that("named latitude and longitude give the same request", {
  log <- local_ors(list(read_fixture("ors_directions_success")))

  ors_route_segment(c(lat = 27.5036, lon = -99.5075), c(lat = 29.4241, lon = -98.4936))
  json <- as.character(jsonlite::toJSON(log$calls[[1]]$body, auto_unbox = TRUE, digits = NA))

  expect_match(json, '"coordinates":[[-99.5075,27.5036],[-98.4936,29.4241]]', fixed = TRUE)
})

test_that("another profile changes the address of the request", {
  log <- local_ors(list(read_fixture("ors_directions_success")))

  ors_route_segment(laredo, san_antonio, profile = "driving-car")

  expect_match(log$calls[[1]]$url, "/openrouteservice/v2/directions/driving-car$")
})

test_that("a bad profile or bad coordinates stop before anything is sent", {
  log <- local_ors(list(read_fixture("ors_directions_success")))

  expect_error(ors_route_segment(laredo, san_antonio, profile = "driving hgv"), "single name")
  expect_error(ors_route_segment(laredo, san_antonio, profile = "../v2/export"), "single name")
  expect_error(ors_route_segment(laredo, san_antonio, profile = NA_character_), "single name")
  expect_error(ors_route_segment(c(27.5, -99.5), san_antonio), "`from` is outside the valid range")
  expect_error(ors_route_segment(laredo, c(1, 2, 3)), "`to` must be two numbers")
  expect_length(log$calls, 0)
})

# --- Failures (two captured live) ------------------------------------------------------------------------

test_that("a point with no road nearby is a permanent failure with ORS's explanation", {
  local_ors(list(read_fixture("ors_directions_not_found")))

  got <- ors_route_segment(c(-140, 30), laredo)

  expect_named(got, result_names)
  expect_true(is.na(got$distance_m))
  expect_true(is.na(got$duration_s))
  expect_match(got$error, "Could not find routable point within a radius of 350.0 meters")
  expect_match(got$error, "ORS code 2010")
  expect_true(got$error_permanent)
  expect_equal(got$error_kind, "not_found")
})

test_that("a route longer than ORS allows is a permanent failure that names the limit", {
  local_ors(list(read_fixture("ors_directions_very_long")))

  got <- ors_route_segment(c(-99.1332, 19.4326), c(-149.9003, 61.2181))

  expect_true(is.na(got$distance_m))
  expect_match(got$error, "must not be greater than 6000000.0 meters")
  expect_match(got$error, "ORS code 2004")
  expect_true(got$error_permanent)
  expect_equal(got$error_kind, "bad_request")
})

test_that("a failed request does not stop anything and is not retried when permanent", {
  log <- local_ors(list(read_fixture("ors_directions_not_found")))

  expect_no_message(got <- ors_route_segment(c(-140, 30), laredo))

  expect_false(is.null(got))
  expect_length(log$calls, 1)
})

test_that("an exhausted quota is a temporary failure", {
  local_ors(list(raw_status(429, headers = limits(2000, 0, reset = 1794410137))))

  got <- ors_route_segment(laredo, san_antonio)

  expect_true(is.na(got$distance_m))
  expect_equal(got$error_kind, "quota")
  expect_false(got$error_permanent)
  expect_match(got$error, "ORS quota exhausted")
})

test_that("a rejected key is a temporary failure", {
  local_ors(list(raw_error(403, "Access to this API has been disallowed")))

  got <- ors_route_segment(laredo, san_antonio)

  expect_equal(got$error_kind, "auth")
  expect_false(got$error_permanent)
  expect_match(got$error, "rejected the API key")
})

test_that("a network problem is retried three times in total, then reported as temporary", {
  log <- local_ors(rep(list(raw_network("Timeout was reached")), 3))

  suppressMessages(got <- ors_route_segment(laredo, san_antonio))

  expect_length(log$calls, 3)
  expect_equal(got$error_kind, "network")
  expect_false(got$error_permanent)
  expect_true(is.na(got$distance_m))
})

test_that("the API key never appears in a failure", {
  leaky <- paste("Failed: https://x.example/route?api_key=", ors_test_key, sep = "")
  local_ors(rep(list(raw_network(leaky)), 3))

  msgs <- testthat::capture_messages(got <- ors_route_segment(laredo, san_antonio))

  expect_false(grepl(ors_test_key, paste(unlist(got), collapse = " "), fixed = TRUE))
  expect_false(any(grepl(ors_test_key, msgs, fixed = TRUE)))
})

# --- Responses that are not what we expect ----------------------------------------------------------------------

test_that("a success response without a usable route is a temporary failure", {
  summary_with <- function(distance, duration) {
    list(routes = list(list(summary = list(distance = distance, duration = duration))))
  }
  bodies <- list(
    "no routes key" = list(metadata = list()),
    "empty routes" = list(routes = list()),
    "routes is not a list" = list(routes = "none"),
    "route is not a list" = list(routes = list("x")),
    "no summary" = list(routes = list(list(bbox = 1))),
    "summary is not a list" = list(routes = list(list(summary = 5))),
    "no distance" = list(routes = list(list(summary = list(duration = 100)))),
    "no duration" = list(routes = list(list(summary = list(distance = 100)))),
    "negative distance" = summary_with(-5, 100),
    "negative duration" = summary_with(100, -5),
    "text distance" = summary_with("far", 100),
    "missing distance value" = summary_with(NA_real_, 100),
    "infinite duration" = summary_with(100, Inf),
    "two distances" = summary_with(c(1, 2), 100)
  )

  for (label in names(bodies)) {
    local_ors(list(raw_ok(bodies[[label]])))

    got <- ors_route_segment(laredo, san_antonio)

    expect_true(is.na(got$distance_m), label = label)
    expect_true(is.na(got$duration_s), label = label)
    expect_false(got$error_permanent, label = label)
    expect_equal(got$error_kind, "unparseable", label = label)
    expect_match(got$error, "^ORS returned", label = label)
  }
})

test_that("a zero-length route is valid", {
  local_ors(list(raw_ok(list(routes = list(list(summary = list(distance = 0, duration = 0)))))))

  got <- ors_route_segment(laredo, laredo)

  expect_equal(got$distance_m, 0)
  expect_equal(got$duration_s, 0)
  expect_true(is.na(got$error))
})

# --- Working with the cache (Property 14) -----------------------------------------------------------------------------------

test_that("each kind of result is cached the way the failure rules say", {
  local_cache()
  s <- cache_store("ors", "route", "driving-hgv")

  cases <- list(
    success = list(read_fixture("ors_directions_success"), "stored"),
    not_found = list(read_fixture("ors_directions_not_found"), "memory_only"),
    too_long = list(read_fixture("ors_directions_very_long"), "memory_only"),
    quota = list(raw_status(429, headers = limits(2000, 0)), "not_stored"),
    auth = list(raw_error(403, "disallowed"), "not_stored"),
    server = list(raw_error(503, "unavailable"), "not_stored"),
    unreadable = list(raw_ok(list(routes = list())), "not_stored")
  )

  for (label in names(cases)) {
    local_ors(rep(list(cases[[label]][[1]]), 3))

    got <- suppressMessages(ors_route_segment(laredo, san_antonio))
    stored <- cache_put(s, label, got)

    expect_equal(stored, cases[[label]][[2]], label = label)
  }
})

test_that("a cached success is read back with its numbers", {
  local_cache()
  local_ors(list(read_fixture("ors_directions_success")))
  s <- cache_store("ors", "route", "driving-hgv")
  key <- route_cache_key(laredo, san_antonio)

  got <- ors_route_segment(laredo, san_antonio)
  cache_put(s, key, got)
  hit <- cache_get(s, key)

  expect_equal(hit$value$distance_m, got$distance_m)
  expect_equal(hit$value$duration_s, got$duration_s)
  expect_false(hit$failure)
})
