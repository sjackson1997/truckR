# Run the real front half of the pipeline: validate -> prepare -> segment.
pipeline <- function(df, waypoint = NULL) {
  mode <- resolve_waypoint_mode(df, waypoint)
  prepped <- prep_lanes(df, mode$use_waypoint)
  build_segments(prepped$lanes, mode$waypoint)
}

mx_la <- function(dest_zip = "90001", ...) {
  data.frame(
    orig_city = "MEXICO CITY", orig_state = "DF", orig_zip = "06600", orig_ctry = "MX",
    dest_city = "LOS ANGELES", dest_state = "CA", dest_zip = dest_zip, dest_ctry = "US",
    stringsAsFactors = FALSE,
    ...
  )
}

pair_count <- function(segs) {
  paste(segs$from_key, segs$to_key)
}

# --- Location keys ---------------------------------------------------------------

test_that("the same place written with different case and spacing has one key", {
  a <- location_key(list(city = "Laredo", state = "TX", zip = "78040", country = "USA"))
  b <- location_key(list(city = "  LAREDO ", state = "tx", zip = "78040", country = "usa"))

  expect_equal(a, b)
})

test_that("different street addresses in one city have different keys", {
  a <- location_key(list(street = "1 Main St", city = "Laredo", state = "TX", country = "USA"))
  b <- location_key(list(street = "9 Other Rd", city = "Laredo", state = "TX", country = "USA"))

  expect_false(a == b)
})

test_that("a missing component is not the same as a different one", {
  with_city <- location_key(list(city = "Laredo", zip = "78040", country = "USA"))
  zip_only <- location_key(list(zip = "78040", country = "USA"))

  expect_false(with_city == zip_only)
})

test_that("coordinates are keyed by rounded coordinates", {
  a <- location_key(list(lat = 27.503601, lon = -99.5075))
  b <- location_key(list(lat = 27.50360, lon = -99.50750))

  expect_equal(a, b)
  expect_match(a, "^COORD\\|")
})

test_that("location_key works on data frames, including empty ones", {
  df <- data.frame(city = c("A", "a", "B"), zip = c("1", "1", "2"))

  keys <- location_key(df)

  expect_length(keys, 3)
  expect_equal(keys[1], keys[2])
  expect_length(location_key(df[0, ]), 0)
})

# --- Segment expansion and de-duplication ----------------------------------------------

test_that("25 lanes through one waypoint share a single origin-to-waypoint segment", {
  df <- do.call(rbind, lapply(1:25, function(i) mx_la(dest_zip = sprintf("900%02d", i))))
  df$dest_city <- paste0("CITY", 1:25)
  df$Use_Waypoint <- TRUE

  res <- pipeline(df, waypoint = "78040")
  segs <- res$segments
  origin_key <- res$locations$location_key[which(res$locations$city == "MEXICO CITY")]
  expect_length(origin_key, 1)

  expect_equal(nrow(res$lane_segments), 25)
  # 1 origin->Laredo + 25 Laredo->destination + 25 direct
  expect_equal(nrow(segs), 51)
  expect_equal(sum(segs$from_key == origin_key & segs$to_key == res$waypoint_key), 1)
  expect_equal(sum(segs$from_key == res$waypoint_key), 25)
  expect_equal(length(unique(res$lane_segments$leg1_seg)), 1)
  expect_equal(length(unique(res$lane_segments$leg2_seg)), 25)
  expect_equal(length(unique(res$lane_segments$direct_seg)), 25)
  # 1 origin + 25 destinations + 1 waypoint
  expect_equal(nrow(res$locations), 27)
})

test_that("two rows with the same origin and destination produce one direct segment", {
  df <- rbind(mx_la(), mx_la())

  res <- pipeline(df)

  expect_equal(nrow(res$segments), 1)
  expect_equal(nrow(res$lane_segments), 1)
  expect_equal(nrow(res$locations), 2)
})

test_that("passing the full lanes table gives one lane_segments row per RouteId", {
  df <- rbind(mx_la(), mx_la(), mx_la(dest_zip = "92101"))
  prepped <- prep_lanes(df)

  res <- build_segments(prepped$lanes)

  expect_equal(nrow(res$lane_segments), 2)
  expect_equal(res$lane_segments$RouteId, prepped$unique_lanes$RouteId)
})

test_that("mixed lanes share the direct segment where origin and destination match", {
  df <- rbind(mx_la(), mx_la(), mx_la(dest_zip = "92101"))
  df$Use_Waypoint <- c(FALSE, TRUE, FALSE)

  res <- pipeline(df, waypoint = "78040")
  ls <- res$lane_segments

  # Lane A (no waypoint) and lane B (waypoint) have the same origin and destination.
  expect_equal(nrow(ls), 3)
  expect_equal(ls$direct_seg[1], ls$direct_seg[2])
  expect_false(is.na(ls$leg1_seg[2]))
  expect_false(is.na(ls$leg2_seg[2]))
  expect_true(is.na(ls$leg1_seg[1]) && is.na(ls$leg2_seg[1]))
  expect_true(is.na(ls$leg1_seg[3]) && is.na(ls$leg2_seg[3]))
  # O->D, O->W, W->D, O->D2
  expect_equal(nrow(res$segments), 4)
})

test_that("no waypoint means direct segments only", {
  df <- rbind(mx_la(), mx_la(dest_zip = "92101"))

  res <- pipeline(df)

  expect_equal(nrow(res$segments), 2)
  expect_true(all(is.na(res$lane_segments$leg1_seg)))
  expect_true(all(is.na(res$lane_segments$leg2_seg)))
  expect_true(is.na(res$waypoint_key))
})

test_that("A to B and B to A are separate segments", {
  forward <- mx_la()
  backward <- data.frame(
    orig_city = "LOS ANGELES", orig_state = "CA", orig_zip = "90001", orig_ctry = "US",
    dest_city = "MEXICO CITY", dest_state = "DF", dest_zip = "06600", dest_ctry = "MX",
    stringsAsFactors = FALSE
  )

  res <- pipeline(rbind(forward, backward))

  expect_equal(nrow(res$segments), 2)
  expect_equal(nrow(res$locations), 2)
})

test_that("a waypoint that equals a lane endpoint shares that location", {
  df <- data.frame(
    orig_zip = "06600", orig_ctry = "MX",
    dest_zip = "78040", dest_ctry = "US",
    Use_Waypoint = TRUE,
    stringsAsFactors = FALSE
  )

  suppressMessages(res <- pipeline(df, waypoint = "78040"))

  # Destination and waypoint are the same place: 2 locations, not 3.
  expect_equal(nrow(res$locations), 2)
  expect_true(res$waypoint_key %in% res$locations$location_key)
})

test_that("a coordinate waypoint is carried through as coordinates", {
  df <- mx_la()
  df$Use_Waypoint <- TRUE

  res <- pipeline(df, waypoint = c(lat = 27.5036, lon = -99.5075))

  expect_match(res$waypoint_key, "^COORD\\|27\\.50360\\|-99\\.50750$")
  wp <- res$locations[res$locations$location_key == res$waypoint_key, ]
  expect_equal(wp$lat, 27.5036)
  expect_equal(wp$lon, -99.5075)
})

test_that("lanes flagged with an error get no segments and add no locations", {
  df <- rbind(mx_la(), mx_la(dest_zip = "92101"))
  df$dest_city[2] <- NA
  df$dest_zip[2] <- NA

  res <- pipeline(df)

  expect_equal(nrow(res$segments), 1)
  expect_equal(res$lane_segments$direct_seg[2], NA_character_)
  expect_false(is.na(res$lane_segments$direct_seg[1]))
  expect_equal(nrow(res$locations), 2)
})

test_that("a flagged waypoint lane does not force a waypoint location", {
  df <- mx_la()
  df$dest_city <- NA
  df$dest_zip <- NA
  df$Use_Waypoint <- TRUE

  res <- pipeline(df, waypoint = "78040")

  expect_equal(nrow(res$segments), 0)
  expect_true(is.na(res$waypoint_key))
  # The only lane is flagged, so no location is needed at all.
  expect_equal(nrow(res$locations), 0)
})

test_that("no segment is listed twice and every key is a known location", {
  df <- rbind(mx_la(), mx_la(dest_zip = "92101"), mx_la())
  df$Use_Waypoint <- c(TRUE, TRUE, FALSE)

  res <- pipeline(df, waypoint = "78040")

  expect_equal(anyDuplicated(pair_count(res$segments)), 0)
  expect_true(all(res$segments$from_key %in% res$locations$location_key))
  expect_true(all(res$segments$to_key %in% res$locations$location_key))
  expect_equal(anyDuplicated(res$locations$location_key), 0)
  expect_match(res$segments$SegmentId, "^SEG-\\d{4}$")
})

test_that("segment ids follow lane order, direct segment first", {
  df <- rbind(mx_la(), mx_la(dest_zip = "92101"))
  df$Use_Waypoint <- c(TRUE, FALSE)

  res <- pipeline(df, waypoint = "78040")

  expect_equal(res$lane_segments$direct_seg[1], "SEG-0001")
  expect_equal(res$lane_segments$leg1_seg[1], "SEG-0002")
  expect_equal(res$lane_segments$leg2_seg[1], "SEG-0003")
  expect_equal(res$lane_segments$direct_seg[2], "SEG-0004")
})

# --- Real sample data --------------------------------------------------------------------

test_that("df_route_samples without a waypoint gives one segment per distinct lane", {
  res <- pipeline(df_route_samples)
  l <- prep_lanes(df_route_samples)$lanes

  expect_equal(nrow(res$segments), 74)
  expect_equal(nrow(res$lane_segments), 74)

  # Independent count of distinct locations from the normalized component columns.
  as_places <- function(prefix) {
    data.frame(
      city = l[[paste0(prefix, "city")]], state = l[[paste0(prefix, "state")]],
      zip = l[[paste0(prefix, "zip")]], country = l[[paste0(prefix, "country")]]
    )
  }
  places <- rbind(as_places("orig_"), as_places("dest_"))
  places[] <- lapply(places, function(x) toupper(ifelse(is.na(x), "", x)))
  expect_equal(nrow(res$locations), nrow(unique(places)))
})

# --- Errors --------------------------------------------------------------------------------

test_that("flagged waypoint lanes without a waypoint are an error", {
  df <- mx_la()
  prepped <- prep_lanes(df, use_waypoint = TRUE)

  expect_error(build_segments(prepped$lanes, waypoint = NULL), "no `waypoint` was supplied")
})

test_that("a table that did not come from prep_lanes() is rejected", {
  expect_error(build_segments(data.frame(a = 1)), "missing column")
})

test_that("zero lanes give empty results", {
  prepped <- prep_lanes(mx_la()[0, ])

  res <- build_segments(prepped$lanes)

  expect_equal(nrow(res$segments), 0)
  expect_equal(nrow(res$locations), 0)
  expect_equal(nrow(res$lane_segments), 0)
})

# --- Same origin and destination -------------------------------------------------------

same_place <- function() {
  # Origin and destination are both Mexico City.
  data.frame(
    orig_city = "MEXICO CITY", orig_state = "DF", orig_zip = "06600", orig_ctry = "MX",
    dest_city = "MEXICO CITY", dest_state = "DF", dest_zip = "06600", dest_ctry = "MX",
    stringsAsFactors = FALSE
  )
}

test_that("a same-place lane without a waypoint produces no segments", {
  res <- pipeline(same_place())

  expect_equal(nrow(res$segments), 0)
  expect_equal(nrow(res$locations), 0)
  expect_true(is.na(res$lane_segments$direct_seg))
})

test_that("a same-place lane does not disturb the other lanes", {
  df <- rbind(same_place(), mx_la())

  res <- pipeline(df)

  expect_equal(nrow(res$segments), 1)
  expect_true(is.na(res$lane_segments$direct_seg[1]))
  expect_false(is.na(res$lane_segments$direct_seg[2]))
})

test_that("a same-place lane with a waypoint gets its two legs but no direct segment", {
  df <- same_place()
  df$Use_Waypoint <- TRUE

  res <- pipeline(df, waypoint = "78040")
  ls <- res$lane_segments

  expect_true(is.na(ls$direct_seg))
  expect_false(is.na(ls$leg1_seg))
  expect_false(is.na(ls$leg2_seg))
  # Mexico City -> Laredo and Laredo -> Mexico City
  expect_equal(nrow(res$segments), 2)
  expect_equal(nrow(res$locations), 2)
  expect_false(any(res$segments$same_place))
  expect_equal(res$segments$from_key[1], res$segments$to_key[2])
})

test_that("a waypoint equal to the origin gives a zero-length leg that is marked", {
  df <- mx_la()
  df$Use_Waypoint <- TRUE

  # The waypoint is Mexico City itself, the lane's origin.
  res <- pipeline(df, waypoint = list(city = "Mexico City", state = "DF", zip = "06600", country = "MX"))
  leg1 <- res$segments[res$segments$SegmentId == res$lane_segments$leg1_seg, ]
  leg2 <- res$segments[res$segments$SegmentId == res$lane_segments$leg2_seg, ]

  expect_true(leg1$same_place)
  expect_false(leg2$same_place)
})

test_that("ordinary segments are never marked as same place", {
  df <- rbind(mx_la(), mx_la(dest_zip = "92101"))
  df$Use_Waypoint <- c(TRUE, FALSE)

  res <- pipeline(df, waypoint = "78040")

  expect_false(any(res$segments$same_place))
})
