# End-to-end tests of route() with a fake provider. Nothing here touches the
# network, the real cache or the real provider registry.

# A fake geocoder: every place gets made-up coordinates that depend only on how
# it is written, so the same place always lands on the same point.
#   fail  - cities that cannot be found
#   check - cities that are found but marked "CHECK:"
fake_geocoder <- function(fail = character(), check = character()) {
  log <- new.env()
  log$calls <- character()
  geocode <- function(location) {
    city <- as.character(location$city)
    zip <- as.character(location$zip)
    label <- paste(city, zip)
    log$calls <- c(log$calls, label)
    if (!is.na(city) && city %in% fail) {
      return(list(
        lon = NA_real_, lat = NA_real_, flag = NA_character_, doubtful = FALSE,
        error = paste("no match found for", city), error_permanent = TRUE, error_kind = "no_match"
      ))
    }
    n <- sum(utf8ToInt(label))
    flag <- if (!is.na(city) && city %in% check) "CHECK: may be the wrong place" else NA_character_
    list(
      lon = -120 + (n %% 40), lat = 25 + (n %% 23) / 2, flag = flag, doubtful = !is.na(flag),
      error = NA_character_, error_permanent = FALSE, error_kind = "ok"
    )
  }
  list(geocode = geocode, log = log)
}

# Register a fake provider as the active one for the length of a test.
# Returns the logs of what it was asked.
local_fake_provider <- function(geocoder = fake_geocoder(), router = fake_router(), env = parent.frame()) {
  local_empty_registry(env)
  local_cache(env)
  provider <- router$provider$provider
  provider$geocode <- geocoder$geocode
  register_provider("fake", provider)
  truckr_set_provider("fake")
  list(geocoder = geocoder, router = router)
}

mexico_lanes <- function(n = 3) {
  df <- data.frame(
    orig_city = "MEXICO CITY", orig_state = "DF", orig_zip = "06600", orig_ctry = "MX",
    dest_city = paste0("DESTCITY", seq_len(n)), dest_state = "CA",
    dest_zip = sprintf("9000%d", seq_len(n)), dest_ctry = "US",
    stringsAsFactors = FALSE
  )
  df
}

base_cols <- c(
  "RouteId", "distance_direct", "distance_routed", "duration_direct", "duration_routed",
  "has_waypoint", "provider_name", "route_error", "geocode_flag", "geocode_check"
)
waypoint_only_cols <- c("waypoint_value", "waypoint_type", "detour_added", "detour_duration_added")

# --- No waypoint ------------------------------------------------------------------------------

test_that("the sample lanes are routed with no waypoint column", {
  fakes <- local_fake_provider()
  df <- df_route_samples[1:10, ]

  out <- suppressMessages(route(df))

  expect_equal(nrow(out), 10)
  expect_equal(out[names(df)], dplyr::as_tibble(df))
  expect_true(all(base_cols %in% names(out)))
  expect_false(any(waypoint_only_cols %in% names(out)))
  expect_equal(out$provider_name, rep("fake", 10))
  expect_false(any(out$has_waypoint))
  expect_true(all(is.na(out$route_error)))
  expect_false(any(out$geocode_check))
  expect_equal(out$distance_direct, out$distance_routed)
  expect_equal(out$duration_direct, out$duration_routed)
  expect_true(all(out$distance_routed > 0))
})

test_that("each distinct place and route is requested once, and in the input order", {
  fakes <- local_fake_provider()
  one <- mexico_lanes(1)
  df <- rbind(one, one, one)
  df$dest_city <- c("A", "B", "A")
  df$dest_zip <- c("90001", "90002", "90001")

  out <- suppressMessages(route(df))

  # Origin plus two destinations; two routes.
  expect_length(fakes$geocoder$log$calls, 3)
  expect_length(fakes$router$log$calls, 2)
  expect_equal(out$distance_routed[1], out$distance_routed[3])
  expect_false(out$distance_routed[1] == out$distance_routed[2])
  expect_equal(out$RouteId[1], out$RouteId[3])
})

test_that("a second run is answered from saved answers with no requests", {
  fakes <- local_fake_provider()
  df <- mexico_lanes(3)

  first <- suppressMessages(route(df))
  geocode_calls <- length(fakes$geocoder$log$calls)
  route_calls <- length(fakes$router$log$calls)
  second <- suppressMessages(route(df))

  expect_equal(length(fakes$geocoder$log$calls), geocode_calls)
  expect_equal(length(fakes$router$log$calls), route_calls)
  expect_equal(second, first)
})

# --- Waypoint ------------------------------------------------------------------------------------

test_that("Mexico to California lanes go through a Laredo waypoint", {
  fakes <- local_fake_provider()
  df <- mexico_lanes(3)
  df$Use_Waypoint <- c(TRUE, TRUE, FALSE)

  out <- suppressMessages(route(df, waypoint = "78040"))

  expect_equal(nrow(out), 3)
  expect_true(all(c(base_cols, waypoint_only_cols) %in% names(out)))
  expect_equal(out$has_waypoint, c(TRUE, TRUE, FALSE))
  expect_equal(out$waypoint_value, c("78040", "78040", NA))
  expect_equal(out$waypoint_type, c("zip", "zip", NA))
  expect_true(all(is.na(out$route_error)))
  expect_true(all(is.na(out$detour_added[3])))

  # The routed numbers of a waypoint lane are the sum of its two legs.
  geo <- function(city, zip) {
    n <- sum(utf8ToInt(paste(city, zip)))
    c(-120 + (n %% 40), 25 + (n %% 23) / 2)
  }
  origin <- geo("MEXICO CITY", "06600")
  laredo <- geo(NA, "78040")
  dest1 <- geo("DESTCITY1", "90001")
  miles <- function(a, b) 1000 * sum(abs(a - b)) / 1609.344
  expect_equal(out$distance_routed[1], miles(origin, laredo) + miles(laredo, dest1))
  expect_equal(out$distance_direct[1], miles(origin, dest1))
  expect_equal(out$detour_added[1], out$distance_routed[1] - out$distance_direct[1])

  # The waypoint is located once and the Mexico City leg is requested once:
  # 1 origin + 3 destinations + 1 waypoint; 3 direct + 1 shared leg + 2 legs.
  expect_length(fakes$geocoder$log$calls, 5)
  expect_length(fakes$router$log$calls, 6)
})

test_that("a waypoint given as coordinates is not geocoded", {
  fakes <- local_fake_provider()
  df <- mexico_lanes(2)
  df$Use_Waypoint <- TRUE

  out <- suppressMessages(route(df, waypoint = c(lat = 27.5, lon = -99.5)))

  expect_true(all(out$waypoint_type == "coordinates"))
  expect_true(all(is.finite(out$distance_routed)))
  # 1 origin + 2 destinations; the waypoint is used as given.
  expect_length(fakes$geocoder$log$calls, 3)
})

# --- Problems with single lanes ---------------------------------------------------------------------

test_that("a lane whose destination cannot be found has no numbers and says why", {
  fakes <- local_fake_provider(fake_geocoder(fail = "DESTCITY2"))
  df <- mexico_lanes(3)

  out <- suppressMessages(route(df))

  expect_equal(nrow(out), 3)
  expect_true(is.na(out$distance_routed[2]))
  expect_match(out$route_error[2], "could not be located")
  expect_match(out$route_error[2], "no match found for DESTCITY2")
  expect_true(all(is.finite(out$distance_routed[c(1, 3)])))
  expect_true(all(is.na(out$route_error[c(1, 3)])))
})

test_that("a lane whose route cannot be found has no numbers and the others are unaffected", {
  df <- mexico_lanes(3)
  # Find the point the second destination is given so its route can be made to fail.
  geocoder <- fake_geocoder()
  from <- unlist(geocoder$geocode(list(city = "MEXICO CITY", zip = "06600"))[c("lon", "lat")])
  to <- unlist(geocoder$geocode(list(city = "DESTCITY2", zip = "90002"))[c("lon", "lat")])
  bad <- list(error = "no road near the point", error_permanent = TRUE, error_kind = "no_route")
  router <- fake_router(fail = stats::setNames(list(bad), route_cache_key(from, to)))
  fakes <- local_fake_provider(geocoder, router)

  out <- suppressMessages(route(df))

  expect_true(is.na(out$distance_routed[2]))
  expect_match(out$route_error[2], "no road near the point")
  expect_true(all(is.finite(out$distance_routed[c(1, 3)])))
})

test_that("a location marked CHECK gives a distance and sets geocode_check", {
  fakes <- local_fake_provider(fake_geocoder(check = "DESTCITY1"))
  df <- mexico_lanes(2)

  out <- suppressMessages(route(df))

  expect_equal(out$geocode_check, c(TRUE, FALSE))
  expect_match(out$geocode_flag[1], "^destination: CHECK:")
  expect_true(is.na(out$geocode_flag[2]))
  expect_true(all(is.finite(out$distance_routed)))
})

test_that("a lane with the same origin and destination and no waypoint is reported, not routed", {
  fakes <- local_fake_provider()
  df <- mexico_lanes(2)
  df$dest_city[2] <- "MEXICO CITY"
  df$dest_state[2] <- "DF"
  df$dest_zip[2] <- "06600"
  df$dest_ctry[2] <- "MX"

  out <- suppressMessages(route(df))

  expect_true(is.na(out$distance_routed[2]))
  expect_match(out$route_error[2], "same origin and destination")
  expect_true(is.finite(out$distance_routed[1]))
})

# --- Stopping and resuming ---------------------------------------------------------------------------

test_that("a run that hits the quota keeps what it has and a later run finishes", {
  fakes <- local_fake_provider(router = fake_router(remaining = 2))
  df <- mexico_lanes(4)

  first <- suppressMessages(route(df))
  done <- !is.na(first$distance_routed)

  expect_equal(sum(done), 2)
  expect_match(first$route_error[!done], "quota exhausted; re-run after reset", all = TRUE)

  fakes$router$log$remaining <- 100
  second <- suppressMessages(route(df))

  expect_true(all(is.finite(second$distance_routed)))
  expect_equal(second$distance_routed[done], first$distance_routed[done])
  # Only the two missing routes were requested the second time.
  expect_length(fakes$router$log$calls, 4)
})

# --- Setup problems ------------------------------------------------------------------------------------

test_that("bad input fails before any request is made", {
  fakes <- local_fake_provider()
  df <- mexico_lanes(2)

  expect_error(route("not a table"), "must be a data frame")
  expect_error(route(df, waypoint = "78040"), "Use_Waypoint")
  df$Use_Waypoint <- TRUE
  expect_error(route(df), "no `waypoint` was supplied")

  expect_length(fakes$geocoder$log$calls, 0)
  expect_length(fakes$router$log$calls, 0)
})

test_that("a missing API key stops the run before any request", {
  fakes <- local_fake_provider()
  provider <- .truckr_state$providers$fake
  provider$validate_credentials <- function() stop("No API key found for provider `fake`.", call. = FALSE)
  register_provider("fake", provider, overwrite = TRUE)

  expect_error(route(mexico_lanes(2)), "No API key found")
  expect_length(fakes$geocoder$log$calls, 0)
})

test_that("with no provider configured the error says how to choose one", {
  local_empty_registry()

  expect_error(route(mexico_lanes(1)), "truckr_set_provider")
})

test_that("a provider can be chosen by name", {
  fakes <- local_fake_provider()
  .truckr_state$active <- NULL

  out <- suppressMessages(route(mexico_lanes(1), provider = "fake"))

  expect_equal(out$provider_name, "fake")
})

test_that("a table with no rows gives a table with no rows", {
  fakes <- local_fake_provider()

  out <- suppressMessages(route(mexico_lanes(2)[0, ]))

  expect_equal(nrow(out), 0)
  expect_true(all(base_cols %in% names(out)))
})

# --- The ORS provider --------------------------------------------------------------------------------------

test_that("the ORS provider is valid, registered at load and active", {
  local_empty_registry()

  expect_no_error(register_ors_provider())

  expect_equal(truckr_list_providers(), "ors")
  expect_equal(truckr_get_provider(), "ors")
  p <- .truckr_state$providers$ors
  expect_equal(p$profile, "driving-hgv")
  expect_equal(p$limits$requests_per_minute, 40)
  expect_true(is.function(p$remaining_quota))
})

test_that("registering ORS does not change a provider the user already chose", {
  local_empty_registry()
  register_provider("other", fake_provider())
  truckr_set_provider("other")

  register_ors_provider()

  expect_equal(truckr_get_provider(), "other")
})

test_that("the ORS provider asks for the API key", {
  withr::local_envvar(TRUCKR_ORS_API_KEY = "")

  expect_error(ors_provider()$validate_credentials(), "TRUCKR_ORS_API_KEY")

  withr::local_envvar(TRUCKR_ORS_API_KEY = "abc")
  expect_true(ors_provider()$validate_credentials())
})
