make_lanes <- function(...) {
  data.frame(
    orig_city = c("MEXICO CITY", "MEXICO CITY", "MONTERREY"),
    orig_state = c("DF", "DF", "NL"),
    orig_zip = c("06600", "06600", "64000"),
    orig_ctry = c("MX", "MX", "MX"),
    dest_city = c("LOS ANGELES", "LOS ANGELES", "SAN DIEGO"),
    dest_state = c("CA", "CA", "CA"),
    dest_zip = c("90001", "90001", "92101"),
    dest_ctry = c("US", "US", "US"),
    stringsAsFactors = FALSE,
    ...
  )
}

# --- Sample data -------------------------------------------------------------

test_that("df_route_samples gives the measured number of distinct lanes", {
  res <- prep_lanes(df_route_samples)

  expect_equal(nrow(res$lanes), 80)
  expect_equal(nrow(res$unique_lanes), 74)
  expect_equal(sum(res$unique_lanes$n), 80)
  expect_equal(length(unique(res$lanes$RouteId)), 74)
})

test_that("rows are kept in their original order", {
  res <- prep_lanes(df_route_samples)

  expect_equal(res$lanes$row_id, seq_len(nrow(df_route_samples)))
  expect_equal(res$lanes$orig_city, as.character(df_route_samples$orig_city))
})

test_that("clean sample data produces no messages", {
  expect_no_message(prep_lanes(df_route_samples))
})

test_that("components are normalized and RouteIds are well formed", {
  res <- prep_lanes(df_route_samples)

  expect_true(all(res$lanes$orig_country %in% c("USA", "MEX", "CAN")))
  expect_true(all(res$lanes$dest_country %in% c("USA", "MEX", "CAN")))
  expect_match(res$lanes$RouteId, "^RTE-\\d{4}$")
  expect_equal(res$lanes$RouteId[1], "RTE-0001")

  # Canadian postal codes are spaced by fix.ZIP().
  expect_true(all(grepl(
    "^[A-Z][0-9][A-Z] [0-9][A-Z][0-9]$",
    res$lanes$orig_zip[res$lanes$orig_country == "CAN"]
  )))
})

test_that("the columns that were used are reported", {
  res <- prep_lanes(df_route_samples)

  expect_equal(res$Origins$city, "orig_city")
  expect_equal(res$Origins$zip, "orig_zip")
  expect_equal(res$Destinations$country, "dest_ctry")
  expect_null(res$Origins$street)
})

# --- Messy data ---------------------------------------------------------------

test_that("messy country codes are normalized, including 'U.S.'", {
  res <- prep_lanes(df_route_unclean)

  expect_true(all(res$lanes$orig_country %in% c("USA", "MEX", "CAN", NA)))
  expect_true(all(res$lanes$dest_country %in% c("USA", "MEX", "CAN", NA)))

  raw_us <- df_route_unclean$orig_ctry %in% c("U.S.", "US", "USA")
  expect_true(any(raw_us))
  expect_true(all(res$lanes$orig_country[raw_us] == "USA"))
})

test_that("the number of RouteIds equals the number of distinct normalized lanes", {
  res <- prep_lanes(df_route_unclean)
  l <- res$lanes

  comps <- l[, grepl("^(orig|dest)_(street|city|state|zip|country)$", names(l))]
  distinct <- nrow(unique(as.data.frame(lapply(comps, function(x) toupper(ifelse(is.na(x), "", x))))))

  expect_equal(length(unique(l$RouteId)), distinct)
  expect_equal(nrow(res$unique_lanes), distinct)
  expect_equal(sum(res$unique_lanes$n), nrow(df_route_unclean))
})

test_that("a numeric zip that lost its leading zero is repaired", {
  df <- make_lanes()
  df$orig_zip <- c(6600, 6600, 64000)

  res <- prep_lanes(df)

  expect_equal(res$lanes$orig_zip, c("06600", "06600", "64000"))
})

# --- Lane identity -------------------------------------------------------------

test_that("identical lanes share a RouteId", {
  res <- prep_lanes(make_lanes())

  expect_equal(res$lanes$RouteId[1], res$lanes$RouteId[2])
  expect_false(res$lanes$RouteId[1] == res$lanes$RouteId[3])
  expect_equal(res$unique_lanes$n, c(2L, 1L))
})

test_that("lanes differing only in waypoint usage get different RouteIds", {
  res <- prep_lanes(make_lanes(), use_waypoint = c(TRUE, FALSE, FALSE))

  expect_false(res$lanes$RouteId[1] == res$lanes$RouteId[2])
  expect_equal(nrow(res$unique_lanes), 3)
  expect_equal(res$lanes$use_waypoint, c(TRUE, FALSE, FALSE))
})

test_that("letter case does not create separate lanes", {
  df <- make_lanes()
  df$orig_city[2] <- "Mexico City"
  df$dest_city[2] <- "los angeles"

  res <- prep_lanes(df)

  expect_equal(res$lanes$RouteId[1], res$lanes$RouteId[2])
})

# --- Optional components ---------------------------------------------------------

test_that("a street address column is used when present", {
  df <- make_lanes()
  df$orig_address <- c("1 Main St", "1 Main St", "2 Oak Ave")

  res <- prep_lanes(df)

  expect_equal(res$Origins$street, "orig_address")
  expect_equal(res$lanes$orig_street, c("1 Main St", "1 Main St", "2 Oak Ave"))
  expect_true(all(is.na(res$lanes$dest_street)))
})

test_that("different street addresses in one city are different lanes", {
  df <- make_lanes()
  df$orig_address <- c("1 Main St", "9 Other Rd", "2 Oak Ave")

  res <- prep_lanes(df)

  expect_equal(nrow(res$unique_lanes), 3)
})

test_that("a zip-only data set works and reports the missing country", {
  df <- data.frame(
    orig_zip = c("78040", "78040"), dest_zip = c("90001", "92101"),
    stringsAsFactors = FALSE
  )

  msgs <- testthat::capture_messages(res <- prep_lanes(df))

  expect_match(msgs, "No country column found for the origin", all = FALSE)
  expect_match(msgs, "No country column found for the destination", all = FALSE)
  expect_equal(res$lanes$orig_zip, c("78040", "78040"))
  expect_true(all(is.na(res$lanes$orig_city)))
  expect_equal(nrow(res$unique_lanes), 2)
})

test_that("several matching columns: the shortest name wins and a message says so", {
  df <- make_lanes()
  df$orig_statename <- c("Distrito Federal", "Distrito Federal", "Nuevo Leon")

  expect_message(res <- prep_lanes(df), "Several origin columns matched state")
  expect_equal(res$Origins$state, "orig_state")
  expect_equal(res$lanes$orig_state, c("DF", "DF", "NL"))
})

# --- Problems ----------------------------------------------------------------------

test_that("blank locations are flagged on that lane only", {
  df <- make_lanes()
  df$orig_city[2] <- ""
  df$orig_zip[2] <- NA
  df$dest_city[3] <- "  "
  df$dest_zip[3] <- NA

  res <- prep_lanes(df)

  expect_equal(
    res$lanes$lane_error,
    c(NA, "origin has no street address, postal code or city",
      "destination has no street address, postal code or city")
  )
  expect_equal(nrow(res$lanes), 3)
})

test_that("both sides empty is reported as such", {
  df <- make_lanes()
  df[1, c("orig_city", "orig_zip", "dest_city", "dest_zip")] <- NA

  res <- prep_lanes(df)

  expect_equal(
    res$lanes$lane_error[1],
    "origin and destination have no street address, postal code or city"
  )
  expect_true(is.na(res$lanes$lane_error[2]))
})

test_that("missing location columns stop with a clear message", {
  df <- data.frame(dest_city = "X", dest_zip = "90001", other = 1)

  expect_error(prep_lanes(df), "Could not find any column identifying the origin")
  expect_error(prep_lanes(df), "dest_city")
})

test_that("a destination with no usable column stops with a clear message", {
  df <- data.frame(orig_city = "X", orig_zip = "90001", other = 1)

  expect_error(prep_lanes(df), "identifying the destination")
})

test_that("bad use_waypoint input is an error", {
  df <- make_lanes()

  expect_error(prep_lanes(df, use_waypoint = c(TRUE, FALSE)), "one non-missing value per row")
  expect_error(prep_lanes(df, use_waypoint = c(TRUE, NA, FALSE)), "one non-missing value per row")
  expect_error(prep_lanes(df, use_waypoint = c(1, 0, 1)), "must be a logical")
})

test_that("df must be a data frame", {
  expect_error(prep_lanes(list(a = 1)), "must be a data frame")
})

test_that("a data frame with no rows returns empty results", {
  res <- prep_lanes(make_lanes()[0, ])

  expect_equal(nrow(res$lanes), 0)
  expect_equal(nrow(res$unique_lanes), 0)
  expect_type(res$lanes$RouteId, "character")
})

test_that("it works together with resolve_waypoint_mode()", {
  df <- make_lanes()
  df$Use_Waypoint <- c(TRUE, FALSE, TRUE)

  mode <- resolve_waypoint_mode(df, waypoint = "78040")
  res <- prep_lanes(df, use_waypoint = mode$use_waypoint)

  expect_equal(res$lanes$use_waypoint, c(TRUE, FALSE, TRUE))
  expect_equal(nrow(res$unique_lanes), 3)
})

# --- Same origin and destination ----------------------------------------------------

same_place_lanes <- function() {
  df <- make_lanes()
  df$dest_city[2] <- "mexico city"      # same place as the origin, different case
  df$dest_state[2] <- "DF"
  df$dest_zip[2] <- "06600"
  df$dest_ctry[2] <- "MX"
  df
}

test_that("a lane with identical origin and destination is flagged and marked", {
  res <- prep_lanes(same_place_lanes())

  expect_equal(res$lanes$same_od, c(FALSE, TRUE, FALSE))
  expect_equal(res$lanes$lane_error, c(NA, "same origin and destination", NA))
})

test_that("with a waypoint, a same-place lane is marked but is not an error", {
  res <- prep_lanes(same_place_lanes(), use_waypoint = c(FALSE, TRUE, FALSE))

  expect_equal(res$lanes$same_od, c(FALSE, TRUE, FALSE))
  expect_true(all(is.na(res$lanes$lane_error)))
})

test_that("origin and destination that differ in any component are not the same place", {
  df <- make_lanes()
  df$dest_city[2] <- "MEXICO CITY"
  df$dest_state[2] <- "DF"
  df$dest_zip[2] <- "06601"            # different zip
  df$dest_ctry[2] <- "MX"

  res <- prep_lanes(df)

  expect_false(any(res$lanes$same_od))
  expect_true(all(is.na(res$lanes$lane_error)))
})

test_that("an empty location is reported as empty, not as same origin and destination", {
  df <- make_lanes()
  df[1, c("orig_city", "orig_zip", "dest_city", "dest_zip")] <- NA

  res <- prep_lanes(df)

  expect_false(res$lanes$same_od[1])
  expect_match(res$lanes$lane_error[1], "no street address, postal code or city")
})

test_that("the clean sample data has no same-place lanes", {
  res <- prep_lanes(df_route_samples)

  expect_false(any(res$lanes$same_od))
})
