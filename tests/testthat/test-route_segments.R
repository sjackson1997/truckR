quota_message <- "quota exhausted; re-run after reset"

run_routes <- function(inputs, router) {
  suppressMessages(route_segments(inputs$built$segments, inputs$geo, router$provider))
}

point_of <- function(inputs, key) {
  g <- inputs$geo[[key]]
  c(g$lon, g$lat)
}

# --- The ordinary case --------------------------------------------------------------------------------------

test_that("each segment gets the provider's distance and time, in order", {
  local_cache()
  inputs <- route_inputs(lanes_table(3))
  router <- fake_router()

  got <- run_routes(inputs, router)
  segs <- inputs$built$segments
  expected_m <- vapply(seq_len(nrow(segs)), function(i) {
    1000 * sum(abs(point_of(inputs, segs$from_key[i]) - point_of(inputs, segs$to_key[i])))
  }, numeric(1))

  expect_equal(got$seg_results$SegmentId, segs$SegmentId)
  expect_equal(got$seg_results$distance_m, expected_m)
  expect_equal(got$seg_results$duration_s, expected_m / 20)
  expect_true(all(is.na(got$seg_results$error)))
  expect_equal(got$seg_results$source, rep("fetched", 3))
  expect_length(router$log$calls, 3)
})

test_that("the provider gets longitude first and the start before the end", {
  local_cache()
  inputs <- route_inputs(lanes_table(1))
  router <- fake_router()

  run_routes(inputs, router)
  seg <- inputs$built$segments[1, ]

  expect_equal(router$log$calls[[1]]$from, point_of(inputs, seg$from_key))
  expect_equal(router$log$calls[[1]]$to, point_of(inputs, seg$to_key))
  expect_lt(router$log$calls[[1]]$from[1], 0)      # longitude, which is about -100 here
  expect_gt(router$log$calls[[1]]$from[2], 0)      # latitude, about 30
})

test_that("the result has the columns assemble_results() needs, plus where each answer came from", {
  local_cache()
  inputs <- route_inputs(lanes_table(2))

  got <- run_routes(inputs, fake_router())

  expect_named(
    got$seg_results,
    c("SegmentId", "distance_m", "duration_s", "error", "error_permanent", "error_kind", "source")
  )
})

test_that("a second run uses the saved answers and makes no requests", {
  local_cache()
  inputs <- route_inputs(lanes_table(3))
  router <- fake_router()

  run_routes(inputs, router)
  again <- run_routes(inputs, router)

  expect_length(router$log$calls, 3)
  expect_equal(again$seg_results$source, rep("cache", 3))
  expect_equal(again$summary$requested, 0)
})

test_that("answers are saved under the provider's name and routing profile", {
  local_cache()
  inputs <- route_inputs(lanes_table(2))

  run_routes(inputs, fake_router())
  store <- cache_store("fake", "route", "test-profile")

  expect_equal(store$id, "fake.route.test-profile")
  expect_length(disk_keys(store), 2)
})

# --- Segments that are not requested -------------------------------------------------------------------------------

test_that("a zero-length segment is left out and never requested", {
  local_cache()
  # The waypoint is the lane's origin, so the first leg has zero length.
  df <- lanes_table(1, waypoint = TRUE)
  inputs <- route_inputs(df, waypoint = list(city = "ORIGIN1", state = "TX", zip = "78001", country = "USA"))
  router <- fake_router()

  got <- run_routes(inputs, router)

  expect_equal(sum(inputs$built$segments$same_place), 1)
  expect_equal(nrow(got$seg_results), nrow(inputs$built$segments) - 1)
  expect_false(any(got$seg_results$SegmentId %in% inputs$built$segments$SegmentId[inputs$built$segments$same_place]))
  expect_length(router$log$calls, nrow(inputs$built$segments) - 1)
})

test_that("two different locations at the same point are zero distance without a request", {
  local_cache()
  inputs <- route_inputs(lanes_table(1))
  seg <- inputs$built$segments[1, ]
  inputs$geo[[seg$to_key]] <- inputs$geo[[seg$from_key]]
  router <- fake_router()

  got <- run_routes(inputs, router)

  expect_equal(got$seg_results$distance_m, 0)
  expect_equal(got$seg_results$duration_s, 0)
  expect_true(is.na(got$seg_results$error))
  expect_equal(got$seg_results$source, "same_point")
  expect_length(router$log$calls, 0)
})

test_that("a segment whose end could not be located is not requested and says which end and why", {
  local_cache()
  inputs <- route_inputs(lanes_table(3))
  segs <- inputs$built$segments
  inputs$geo[[segs$to_key[2]]] <- geo_failure("no match found for DEST2, CA, USA")
  router <- fake_router()

  got <- run_routes(inputs, router)

  expect_equal(got$seg_results$error[2], "the end point could not be located: no match found for DEST2, CA, USA")
  expect_equal(got$seg_results$error_kind[2], "geocode")
  expect_true(got$seg_results$error_permanent[2])
  expect_true(is.na(got$seg_results$distance_m[2]))
  expect_equal(got$seg_results$source[2], "not_requested")
  expect_false(is.na(got$seg_results$distance_m[1]))
  expect_false(is.na(got$seg_results$distance_m[3]))
  expect_length(router$log$calls, 2)
})

test_that("when both ends are missing both are named", {
  local_cache()
  inputs <- route_inputs(lanes_table(1))
  seg <- inputs$built$segments[1, ]
  inputs$geo[[seg$from_key]] <- geo_failure("start problem")
  inputs$geo[[seg$to_key]] <- NULL
  router <- fake_router()

  got <- run_routes(inputs, router)

  expect_equal(
    got$seg_results$error,
    "the start point could not be located: start problem; the end point could not be located: no coordinates were found"
  )
  expect_length(router$log$calls, 0)
})

test_that("a temporary geocoding failure is not marked permanent", {
  local_cache()
  inputs <- route_inputs(lanes_table(1))
  seg <- inputs$built$segments[1, ]
  inputs$geo[[seg$to_key]] <- geo_failure("timeout", permanent = FALSE)

  got <- run_routes(inputs, fake_router())

  expect_false(got$seg_results$error_permanent)
})

test_that("a doubtful geocode is routed like any other", {
  local_cache()
  inputs <- route_inputs(lanes_table(1))
  seg <- inputs$built$segments[1, ]
  inputs$geo[[seg$from_key]]$doubtful <- TRUE
  inputs$geo[[seg$from_key]]$flag <- "CHECK: may be the wrong place."
  router <- fake_router()

  got <- run_routes(inputs, router)

  expect_true(is.na(got$seg_results$error))
  expect_length(router$log$calls, 1)
})

test_that("segments with identical coordinates are requested once", {
  local_cache()
  df <- lanes_table(2)
  inputs <- route_inputs(df)
  segs <- inputs$built$segments
  # Both lanes start and end at the same points, written as different locations.
  inputs$geo[[segs$from_key[2]]] <- inputs$geo[[segs$from_key[1]]]
  inputs$geo[[segs$to_key[2]]] <- inputs$geo[[segs$to_key[1]]]
  router <- fake_router()

  got <- run_routes(inputs, router)

  expect_length(router$log$calls, 1)
  expect_equal(got$seg_results$distance_m[1], got$seg_results$distance_m[2])
  expect_false(is.na(got$seg_results$distance_m[2]))
})

# --- The quota ----------------------------------------------------------------------------------------------------------------------

test_that("the run stops at the quota; the lanes left over say so; a second run finishes them", {
  local_cache()
  inputs <- route_inputs(lanes_table(5))
  router <- fake_router(remaining = 2)

  first <- run_routes(inputs, router)

  expect_length(router$log$calls, 2)
  expect_equal(first$seg_results$source, c("fetched", "fetched", "stopped", "stopped", "stopped"))
  expect_equal(first$seg_results$error[3:5], rep(quota_message, 3))
  expect_equal(first$seg_results$error_kind[3:5], rep("quota", 3))
  expect_equal(first$summary$stop_reason, "quota")
  expect_equal(first$summary$not_requested, 3)

  router$log$remaining <- 100
  second <- run_routes(inputs, router)

  expect_length(router$log$calls, 5)                 # only the three that were left
  expect_equal(second$seg_results$source, c("cache", "cache", "fetched", "fetched", "fetched"))
  expect_true(all(is.na(second$seg_results$error)))
})

test_that("the lanes that could not be routed show the quota error in the final table", {
  local_cache()
  df <- lanes_table(3)
  inputs <- route_inputs(df)
  router <- fake_router(remaining = 1)

  routed <- run_routes(inputs, router)
  out <- assemble_results(df, inputs$prepped, inputs$built, routed$seg_results, NULL, "fake")

  expect_equal(out$route_error, c(NA, quota_message, quota_message))
  expect_false(is.na(out$distance_routed[1]))
  expect_true(all(is.na(out$distance_routed[2:3])))
  expect_equal(nrow(out), 3)
})

test_that("a waypoint lane whose second leg ran out of quota is reported for that leg only", {
  local_cache()
  df <- lanes_table(1, waypoint = TRUE)
  inputs <- route_inputs(df, waypoint = c(lat = 27.5, lon = -99.5))
  segs <- inputs$built$segments
  # direct (SEG-0001), origin to waypoint (SEG-0002), waypoint to destination (SEG-0003)
  router <- fake_router(remaining = 2)

  routed <- run_routes(inputs, router)
  out <- assemble_results(df, inputs$prepped, inputs$built, routed$seg_results, inputs$mode$waypoint, "fake")

  expect_equal(nrow(segs), 3)
  expect_equal(out$route_error, paste0("waypoint to destination: ", quota_message))
  expect_true(is.na(out$distance_routed))
  expect_false(is.na(out$distance_direct))
})

test_that("a provider that cannot report its quota still works", {
  local_cache()
  inputs <- route_inputs(lanes_table(2))
  router <- fake_router(remaining = NULL)

  msgs <- testthat::capture_messages(got <- route_segments(inputs$built$segments, inputs$geo, router$provider))

  expect_true(any(grepl("the remaining quota is not known yet", msgs)))
  expect_equal(got$seg_results$source, c("fetched", "fetched"))
})

test_that("the pre-flight message counts what is saved and what is needed", {
  local_cache()
  inputs <- route_inputs(lanes_table(4))
  router <- fake_router(remaining = 500)
  # Save the first two by running only them.
  suppressMessages(route_segments(inputs$built$segments[1:2, ], inputs$geo, router$provider))

  msgs <- testthat::capture_messages(route_segments(inputs$built$segments, inputs$geo, router$provider))

  expect_match(msgs[1], "Routing: 2 requests needed \\(2 of 4 already saved\\); about 498 remain today.")
})

# --- Replies the provider should not give --------------------------------------------------------------------------------------------

test_that("a provider failure is recorded on that segment and the others carry on", {
  local_cache()
  inputs <- route_inputs(lanes_table(3))
  segs <- inputs$built$segments
  key <- route_cache_key(point_of(inputs, segs$from_key[2]), point_of(inputs, segs$to_key[2]))
  router <- fake_router(fail = stats::setNames(
    list(failure_reply("not_found", permanent = TRUE, error = "no route found")), key
  ))

  got <- run_routes(inputs, router)

  expect_equal(got$seg_results$error, c(NA, "no route found", NA))
  expect_equal(got$seg_results$error_permanent, c(FALSE, TRUE, FALSE))
  expect_equal(got$seg_results$error_kind, c("ok", "not_found", "ok"))
  expect_true(is.na(got$seg_results$distance_m[2]))
  expect_equal(got$seg_results$source, c("fetched", "failed", "fetched"))
})

test_that("a provider that returns nothing usable gives a clear error", {
  local_cache()
  inputs <- route_inputs(lanes_table(1))
  router <- fake_router(reply = NULL)
  router$provider$provider$route_segment <- function(from, to) NULL

  got <- run_routes(inputs, router)

  expect_equal(got$seg_results$error, "the provider returned an unusable result")
  expect_equal(got$seg_results$error_kind, "unusable")
})

test_that("a result that leaves out the distance is stored as missing, not as an error in the code", {
  local_cache()
  inputs <- route_inputs(lanes_table(1))
  router <- fake_router(reply = list(error = NA_character_, error_permanent = FALSE, error_kind = "ok"))

  got <- run_routes(inputs, router)

  expect_true(is.na(got$seg_results$distance_m))
  expect_true(is.na(got$seg_results$duration_s))
})

# --- Bad input ---------------------------------------------------------------------------------------------------------------------------------

test_that("inputs that are not what route_segments() expects are rejected", {
  local_cache()
  inputs <- route_inputs(lanes_table(1))

  expect_error(route_segments(data.frame(a = 1), inputs$geo, fake_router()$provider), "segments table from build_segments")
  expect_error(route_segments(inputs$built$segments, "geo", fake_router()$provider), "named list of geocoding results")
})

test_that("with no segments the result is empty", {
  local_cache()
  inputs <- route_inputs(lanes_table(1))

  got <- suppressMessages(route_segments(inputs$built$segments[0, ], inputs$geo, fake_router()$provider))

  expect_equal(nrow(got$seg_results), 0)
})
