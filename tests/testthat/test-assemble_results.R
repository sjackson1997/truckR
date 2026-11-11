# Helpers -------------------------------------------------------------------------------
#
# Segment results are built by hand from round numbers (miles and hours), then
# converted to meters and seconds the way a provider would report them, so every
# expected value below can be checked by eye.

lane_row <- function(dest_zip = "90001", dest_city = "LOS ANGELES") {
  data.frame(
    orig_city = "MEXICO CITY", orig_state = "DF", orig_zip = "06600", orig_ctry = "MX",
    dest_city = dest_city, dest_state = "CA", dest_zip = dest_zip, dest_ctry = "US",
    stringsAsFactors = FALSE
  )
}

setup <- function(df, waypoint = NULL) {
  mode <- resolve_waypoint_mode(df, waypoint)
  prepped <- prep_lanes(df, mode$use_waypoint)
  built <- build_segments(prepped$lanes, mode$waypoint)
  list(df = df, mode = mode, prepped = prepped, built = built)
}

seg_res <- function(miles, hours, errors = character(0)) {
  ids <- names(miles)
  out <- data.frame(
    SegmentId = ids,
    distance_m = unname(miles) * 1609.344,
    duration_s = unname(hours[ids]) * 3600,
    error = rep(NA_character_, length(ids)),
    stringsAsFactors = FALSE
  )
  out$error[match(names(errors), out$SegmentId)] <- unname(errors)
  out
}

finish <- function(s, results, flags = NULL, provider = "fake", ...) {
  assemble_results(s$df, s$prepped, s$built, results, s$mode$waypoint, provider, flags, ...)
}

always_cols <- c(
  "RouteId", "distance_direct", "distance_routed", "duration_direct",
  "duration_routed", "has_waypoint", "provider_name", "route_error", "geocode_flag",
  "geocode_check"
)
waypoint_cols <- c("waypoint_value", "waypoint_type", "detour_added", "detour_duration_added")

# Three lanes:  A  Mexico City -> Los Angeles (direct)
#               B  Mexico City -> Los Angeles via Laredo
#               C  Mexico City -> San Diego (direct)
# Segments:     SEG-0001 A and B direct;  SEG-0002 origin->Laredo;
#               SEG-0003 Laredo->Los Angeles;  SEG-0004 C direct
mixed <- function() {
  df <- rbind(lane_row(), lane_row(), lane_row(dest_zip = "92101", dest_city = "SAN DIEGO"))
  df$Use_Waypoint <- c(FALSE, TRUE, FALSE)
  setup(df, waypoint = "78040")
}

mixed_results <- function(...) {
  seg_res(
    miles = c(`SEG-0001` = 1000, `SEG-0002` = 400, `SEG-0003` = 700, `SEG-0004` = 300),
    hours = c(`SEG-0001` = 20, `SEG-0002` = 8, `SEG-0003` = 14, `SEG-0004` = 6),
    ...
  )
}

# --- Units --------------------------------------------------------------------------------

test_that("unit conversions are exact and keep NA", {
  expect_equal(meters_to_miles(1609.344), 1)
  expect_equal(meters_to_miles(c(0, NA)), c(0, NA))
  expect_equal(seconds_to_hours(3600), 1)
  expect_equal(seconds_to_hours(c(1800, NA)), c(0.5, NA))
})

# --- The normal cases -------------------------------------------------------------------------

test_that("mixed lanes get the right distances and durations", {
  s <- mixed()
  out <- finish(s, mixed_results())

  expect_equal(out$distance_direct, c(1000, 1000, 300))
  expect_equal(out$distance_routed, c(1000, 1100, 300))
  expect_equal(out$duration_direct, c(20, 20, 6))
  expect_equal(out$duration_routed, c(20, 22, 6))
  expect_equal(out$has_waypoint, c(FALSE, TRUE, FALSE))
  expect_equal(out$detour_added, c(NA, 100, NA))
  expect_equal(out$detour_duration_added, c(NA, 2, NA))
  expect_equal(out$waypoint_value, c(NA, "78040", NA))
  expect_equal(out$waypoint_type, c(NA, "zip", NA))
  expect_equal(out$provider_name, rep("fake", 3))
  expect_true(all(is.na(out$route_error)))
  expect_true(all(is.na(out$geocode_flag)))
})

test_that("the original columns come first and are unchanged (Property 1)", {
  s <- mixed()
  s$df$weight <- c(10, 20, 30)
  s$df$note <- c("a", "b", "c")
  s$prepped <- prep_lanes(s$df, s$mode$use_waypoint)
  out <- finish(s, mixed_results())

  expect_equal(nrow(out), 3)
  expect_equal(names(out)[seq_along(names(s$df))], names(s$df))
  expect_equal(as.data.frame(out[, names(s$df)]), s$df)
  expect_equal(out$weight, c(10, 20, 30))
})

test_that("rows sharing a lane share results and order is kept", {
  df <- rbind(lane_row(), lane_row(dest_zip = "92101", dest_city = "SAN DIEGO"), lane_row())
  s <- setup(df)
  out <- finish(s, seg_res(
    miles = c(`SEG-0001` = 1000, `SEG-0002` = 300),
    hours = c(`SEG-0001` = 20, `SEG-0002` = 6)
  ))

  expect_equal(out$distance_routed, c(1000, 300, 1000))
  expect_equal(out$RouteId, c("RTE-0001", "RTE-0002", "RTE-0001"))
})

test_that("the result is a tibble even for a plain data frame", {
  out <- finish(mixed(), mixed_results())

  expect_s3_class(out, "tbl_df")
})

# --- Always-present and conditional columns (Properties 2, 3, 4) ---------------------------------

test_that("with no waypoint lanes, only the always-present columns are added", {
  s <- setup(rbind(lane_row(), lane_row(dest_zip = "92101", dest_city = "SAN DIEGO")))
  out <- finish(s, seg_res(
    miles = c(`SEG-0001` = 1000, `SEG-0002` = 300),
    hours = c(`SEG-0001` = 20, `SEG-0002` = 6)
  ))

  expect_equal(names(out), c(names(s$df), always_cols))
  expect_false(any(waypoint_cols %in% names(out)))
  expect_equal(out$distance_routed, out$distance_direct)
  expect_equal(out$duration_routed, out$duration_direct)
  expect_equal(out$has_waypoint, c(FALSE, FALSE))
})

test_that("with a waypoint lane, the waypoint columns are added after the always-present ones", {
  s <- mixed()
  out <- finish(s, mixed_results())

  expect_equal(names(out), c(names(s$df), always_cols, waypoint_cols))
})

test_that("non-waypoint lanes have routed equal to direct and NA waypoint columns", {
  out <- finish(mixed(), mixed_results())
  plain <- !out$has_waypoint

  expect_equal(out$distance_routed[plain], out$distance_direct[plain])
  expect_equal(out$duration_routed[plain], out$duration_direct[plain])
  expect_true(all(is.na(out$waypoint_value[plain])))
  expect_true(all(is.na(out$waypoint_type[plain])))
  expect_true(all(is.na(out$detour_added[plain])))
})

test_that("a waypoint lane's detour is routed minus direct (Property 5)", {
  out <- finish(mixed(), mixed_results())
  w <- out$has_waypoint

  expect_equal(out$detour_added[w], out$distance_routed[w] - out$distance_direct[w])
  expect_equal(out$detour_duration_added[w], out$duration_routed[w] - out$duration_direct[w])
})

test_that("a coordinate waypoint is described by its coordinates", {
  df <- rbind(lane_row(), lane_row())
  df$Use_Waypoint <- c(TRUE, FALSE)
  s <- setup(df, waypoint = c(lat = 27.5036, lon = -99.5075))
  out <- finish(s, seg_res(
    miles = c(`SEG-0001` = 1000, `SEG-0002` = 400, `SEG-0003` = 700),
    hours = c(`SEG-0001` = 20, `SEG-0002` = 8, `SEG-0003` = 14)
  ))

  expect_equal(out$waypoint_type, c("coordinates", NA))
  expect_equal(out$waypoint_value, c("27.50360,-99.50750", NA))
})

# --- Failures stay local (Property 9) -------------------------------------------------------------

test_that("a failed leg makes only that lane's routed values NA", {
  out <- finish(mixed(), mixed_results(errors = c(`SEG-0003` = "no route found")))

  expect_equal(out$distance_routed, c(1000, NA, 300))
  expect_equal(out$duration_routed, c(20, NA, 6))
  expect_equal(out$distance_direct, c(1000, 1000, 300))
  expect_equal(out$detour_added, rep(NA_real_, 3))
  expect_equal(out$route_error, c(NA, "waypoint to destination: no route found", NA))
})

test_that("both legs failing reports both", {
  out <- finish(mixed(), mixed_results(errors = c(`SEG-0002` = "bad A", `SEG-0003` = "bad B")))

  expect_equal(
    out$route_error[2],
    "origin to waypoint: bad A; waypoint to destination: bad B"
  )
})

test_that("a failed direct segment on a plain lane gives NA and the reason", {
  out <- finish(mixed(), mixed_results(errors = c(`SEG-0004` = "boom")))

  expect_equal(out$distance_routed[3], NA_real_)
  expect_equal(out$distance_direct[3], NA_real_)
  expect_equal(out$route_error, c(NA, NA, "boom"))
  expect_equal(out$distance_routed[1:2], c(1000, 1100))
})

test_that("a failed direct segment on a waypoint lane keeps the routed values", {
  out <- finish(mixed(), mixed_results(errors = c(`SEG-0001` = "boom")))

  # Lane A (plain) uses the failed direct segment; lane B (waypoint) only needs its legs.
  expect_equal(out$distance_routed, c(NA, 1100, 300))
  expect_equal(out$distance_direct, c(NA, NA, 300))
  expect_equal(out$detour_added, rep(NA_real_, 3))
  expect_equal(out$route_error, c("boom", "direct route: boom", NA))
})

test_that("numbers that come with an error are discarded", {
  res <- mixed_results()
  res$error[res$SegmentId == "SEG-0004"] <- "partial result"

  out <- finish(mixed(), res)

  expect_true(is.na(out$distance_routed[3]))
  expect_equal(out$route_error[3], "partial result")
})

test_that("a segment with no result row is reported", {
  res <- mixed_results()
  res <- res[res$SegmentId != "SEG-0003", ]

  out <- finish(mixed(), res)

  expect_equal(out$route_error[2], "waypoint to destination: no result was returned for this segment")
  expect_true(is.na(out$distance_routed[2]))
  expect_equal(out$distance_routed[c(1, 3)], c(1000, 300))
})

test_that("a result with no numbers and no error is reported", {
  res <- mixed_results()
  res$distance_m[res$SegmentId == "SEG-0004"] <- NA

  out <- finish(mixed(), res)

  expect_equal(out$route_error[3], "the provider returned no distance or duration")
})

# --- The routed-versus-direct check (Property 11) ----------------------------------------------------------

check_case <- function(leg1_miles, leg2_miles, leg1_hours = 8, leg2_hours = 14, ...) {
  df <- rbind(lane_row(), lane_row())
  df$Use_Waypoint <- c(FALSE, TRUE)
  s <- setup(df, waypoint = "78040")
  finish(s, seg_res(
    miles = c(`SEG-0001` = 1000, `SEG-0002` = leg1_miles, `SEG-0003` = leg2_miles),
    hours = c(`SEG-0001` = 20, `SEG-0002` = leg1_hours, `SEG-0003` = leg2_hours)
  ), ...)
}

test_that("a waypoint route shorter than direct is flagged but its numbers are kept", {
  out <- check_case(300, 600)

  expect_match(out$route_error[2], "^check: ")
  expect_match(out$route_error[2], "waypoint may be in the wrong place")
  expect_equal(out$distance_routed[2], 900)
  expect_equal(out$detour_added[2], -100)
  expect_true(is.na(out$route_error[1]))
})

test_that("a shorter duration alone is also flagged", {
  out <- check_case(600, 500, leg1_hours = 7, leg2_hours = 8)

  expect_equal(out$distance_routed[2], 1100)
  expect_equal(out$duration_routed[2], 15)
  expect_match(out$route_error[2], "^check: ")
})

test_that("a difference inside the 0.5 percent tolerance is not flagged", {
  inside <- check_case(400, 597)   # 997 miles against 1000: 0.3 percent shorter
  outside <- check_case(400, 590)  # 990 miles against 1000: 1 percent shorter

  expect_true(is.na(inside$route_error[2]))
  expect_match(outside$route_error[2], "^check: ")
})

test_that("the tolerance can be changed", {
  out <- check_case(400, 590, tolerance = 0.02)

  expect_true(is.na(out$route_error[2]))
})

test_that("a waypoint route longer than direct is never flagged", {
  out <- check_case(800, 900)

  expect_true(is.na(out$route_error[2]))
  expect_equal(out$detour_added[2], 700)
})

# --- Flagged lanes ------------------------------------------------------------------------------------------

test_that("a lane with an empty location gets NA values and the reason", {
  df <- rbind(lane_row(), lane_row(dest_zip = "92101", dest_city = "SAN DIEGO"))
  df$orig_city[2] <- NA
  df$orig_zip[2] <- NA
  s <- setup(df)
  out <- finish(s, seg_res(miles = c(`SEG-0001` = 1000), hours = c(`SEG-0001` = 20)))

  expect_equal(out$distance_routed, c(1000, NA))
  expect_equal(out$distance_direct, c(1000, NA))
  expect_equal(out$route_error, c(NA, "origin has no street address, postal code or city"))
})

same_place_row <- function() {
  data.frame(
    orig_city = "MEXICO CITY", orig_state = "DF", orig_zip = "06600", orig_ctry = "MX",
    dest_city = "MEXICO CITY", dest_state = "DF", dest_zip = "06600", dest_ctry = "MX",
    stringsAsFactors = FALSE
  )
}

test_that("same origin and destination without a waypoint is flagged and not routed", {
  s <- setup(rbind(same_place_row(), lane_row()))
  out <- finish(s, seg_res(miles = c(`SEG-0001` = 1000), hours = c(`SEG-0001` = 20)))

  expect_equal(out$distance_routed, c(NA, 1000))
  expect_equal(out$route_error, c("same origin and destination", NA))
  expect_equal(nrow(s$built$segments), 1)
})

test_that("same origin and destination with a waypoint is routed out and back", {
  df <- same_place_row()
  df$Use_Waypoint <- TRUE
  s <- setup(df, waypoint = "78040")
  out <- finish(s, seg_res(
    miles = c(`SEG-0001` = 694, `SEG-0002` = 700),
    hours = c(`SEG-0001` = 13.9, `SEG-0002` = 14.1)
  ))

  expect_equal(out$distance_routed, 1394)
  expect_equal(out$duration_routed, 28)
  expect_true(is.na(out$distance_direct))
  expect_true(is.na(out$duration_direct))
  expect_true(is.na(out$detour_added))
  expect_equal(out$route_error, "same origin and destination; the direct distance does not apply")
  expect_true(out$has_waypoint)
})

test_that("a zero-length leg counts as 0 without needing a result", {
  df <- lane_row()
  df$Use_Waypoint <- TRUE
  # The waypoint is the lane's origin, so leg 1 has zero length.
  s <- setup(df, waypoint = list(city = "Mexico City", state = "DF", zip = "06600", country = "MX"))
  ids <- s$built$lane_segments
  res <- seg_res(
    miles = stats::setNames(c(1000, 1000), c(ids$direct_seg, ids$leg2_seg)),
    hours = stats::setNames(c(20, 20), c(ids$direct_seg, ids$leg2_seg))
  )

  out <- finish(s, res)

  expect_true(s$built$segments$same_place[s$built$segments$SegmentId == ids$leg1_seg])
  expect_equal(out$distance_routed, 1000)
  expect_equal(out$detour_added, 0)
  expect_true(is.na(out$route_error))
})

# --- Geocode notes -------------------------------------------------------------------------------------------------

test_that("location notes appear on the lanes that use those locations", {
  s <- mixed()
  la <- s$built$locations$location_key[which(s$built$locations$city == "LOS ANGELES")]
  wp <- s$built$waypoint_key
  flags <- data.frame(
    location_key = c(la, wp),
    flag = c("matched at city level only", "country inferred as USA"),
    stringsAsFactors = FALSE
  )

  out <- finish(s, mixed_results(), flags = flags)

  expect_equal(out$geocode_flag[1], "destination: matched at city level only")
  expect_equal(
    out$geocode_flag[2],
    "destination: matched at city level only; waypoint: country inferred as USA"
  )
  expect_true(is.na(out$geocode_flag[3]))
})

test_that("a waypoint note is not shown on lanes that do not use the waypoint", {
  s <- mixed()
  flags <- data.frame(location_key = s$built$waypoint_key, flag = "note", stringsAsFactors = FALSE)

  out <- finish(s, mixed_results(), flags = flags)

  expect_equal(out$geocode_flag, c(NA, "waypoint: note", NA))
})

test_that("geocode_check is TRUE only for lanes with a location marked CHECK", {
  s <- mixed()
  la <- s$built$locations$location_key[which(s$built$locations$city == "LOS ANGELES")]
  flags <- data.frame(
    location_key = c(la, s$built$waypoint_key),
    flag = c("CHECK: may be the wrong place", "country inferred as USA"),
    stringsAsFactors = FALSE
  )

  out <- finish(s, mixed_results(), flags = flags)

  # Lanes 1 and 2 go to Los Angeles; lane 3 goes to San Diego.
  expect_equal(out$geocode_check, c(TRUE, TRUE, FALSE))
})

test_that("an ordinary note does not set geocode_check, and the waypoint can", {
  s <- mixed()
  plain <- data.frame(location_key = s$built$waypoint_key, flag = "country inferred as USA", stringsAsFactors = FALSE)
  check <- data.frame(location_key = s$built$waypoint_key, flag = "CHECK: waypoint doubtful", stringsAsFactors = FALSE)

  expect_equal(finish(s, mixed_results(), flags = plain)$geocode_check, c(FALSE, FALSE, FALSE))
  # Only the lane that uses the waypoint is affected.
  expect_equal(finish(s, mixed_results(), flags = check)$geocode_check, c(FALSE, TRUE, FALSE))
  expect_equal(finish(s, mixed_results())$geocode_check, c(FALSE, FALSE, FALSE))
})

# --- Other inputs --------------------------------------------------------------------------------------------------------

test_that("provider_name is recorded on every row", {
  out <- finish(mixed(), mixed_results(), provider = "ors")

  expect_equal(out$provider_name, rep("ors", 3))
})

test_that("a zero-row input gives a zero-row result with the right columns", {
  s <- setup(lane_row()[0, ])
  out <- finish(s, seg_res(miles = c(a = 1)[0], hours = c(a = 1)[0]))

  expect_equal(nrow(out), 0)
  expect_equal(names(out), c(names(s$df), always_cols))
})

test_that("the always-present columns exist in every scenario (Property 2)", {
  scenarios <- list(
    finish(mixed(), mixed_results()),
    finish(mixed(), mixed_results(errors = c(`SEG-0003` = "x"))),
    finish(setup(lane_row()), seg_res(miles = c(`SEG-0001` = 5), hours = c(`SEG-0001` = 1))),
    finish(setup(same_place_row()), seg_res(miles = c(a = 1)[0], hours = c(a = 1)[0]))
  )

  for (out in scenarios) {
    expect_true(all(always_cols %in% names(out)))
    expect_type(out$distance_direct, "double")
    expect_type(out$distance_routed, "double")
    expect_type(out$has_waypoint, "logical")
  }
})

# --- Bad input -------------------------------------------------------------------------------------------------------------

test_that("a column the result adds cannot already exist", {
  s <- mixed()
  s$df$distance_direct <- 1
  s$prepped <- prep_lanes(s$df, s$mode$use_waypoint)

  expect_error(finish(s, mixed_results()), "already has column.*distance_direct")
})

test_that("a waypoint-only column name only clashes when waypoint columns are added", {
  s <- setup(lane_row())
  s$df$detour_added <- 5
  s$prepped <- prep_lanes(s$df)

  expect_no_error(finish(s, seg_res(miles = c(`SEG-0001` = 5), hours = c(`SEG-0001` = 1))))
})

test_that("mismatched or malformed inputs are rejected", {
  s <- mixed()

  expect_error(assemble_results(list(a = 1), s$prepped, s$built, mixed_results()), "must be a data frame")
  expect_error(
    assemble_results(s$df[1:2, ], s$prepped, s$built, mixed_results()),
    "do not describe the same rows"
  )
  expect_error(
    finish(s, data.frame(SegmentId = "SEG-0001")),
    "`seg_results` must be a data frame with columns"
  )
  expect_error(
    finish(s, mixed_results(), flags = data.frame(a = 1)),
    "`location_flags` must be a data frame"
  )
})

test_that("flagged waypoint lanes without a waypoint are rejected", {
  s <- mixed()

  expect_error(
    assemble_results(s$df, s$prepped, s$built, mixed_results(), waypoint = NULL),
    "no `waypoint` was supplied"
  )
})
