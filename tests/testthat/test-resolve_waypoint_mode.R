lanes <- function(n = 3, flag = NULL) {
  df <- data.frame(
    orig_city = rep("MEXICO CITY", n),
    dest_city = rep("LOS ANGELES", n)
  )
  if (!is.null(flag)) df$Use_Waypoint <- flag
  df
}

# --- The rules table ---------------------------------------------------------

test_that("no column and no waypoint means no waypoint processing", {
  res <- resolve_waypoint_mode(lanes(3))

  expect_equal(res$mode, "none")
  expect_equal(res$use_waypoint, c(FALSE, FALSE, FALSE))
  expect_null(res$waypoint)
})

test_that("flagged lanes with a waypoint resolve to waypoint mode", {
  res <- resolve_waypoint_mode(lanes(3, c(TRUE, FALSE, TRUE)), waypoint = "78040")

  expect_equal(res$mode, "waypoint")
  expect_equal(res$use_waypoint, c(TRUE, FALSE, TRUE))
  expect_equal(res$waypoint$type, "zip")
  expect_equal(res$waypoint$zip, "78040")
})

test_that("TRUE flags without a waypoint is an error", {
  expect_error(
    resolve_waypoint_mode(lanes(2, c(TRUE, FALSE))),
    "Use_Waypoint.*no `waypoint`"
  )
})

test_that("a column with no TRUE and no waypoint routes everything directly", {
  res <- resolve_waypoint_mode(lanes(2, c(FALSE, FALSE)))

  expect_equal(res$mode, "none")
  expect_equal(res$use_waypoint, c(FALSE, FALSE))
  expect_null(res$waypoint)
})

test_that("a column with no TRUE but a waypoint warns and routes directly", {
  expect_warning(
    res <- resolve_waypoint_mode(lanes(2, c(FALSE, FALSE)), waypoint = "78040"),
    "no lane has `Use_Waypoint` = TRUE"
  )

  expect_equal(res$mode, "none")
  expect_null(res$waypoint)
})

test_that("a waypoint without the column is an error naming the column", {
  expect_error(
    resolve_waypoint_mode(lanes(2), waypoint = "78040"),
    "no `Use_Waypoint` column"
  )
})

test_that("a non-logical column is an error", {
  expect_error(
    resolve_waypoint_mode(lanes(2, c("TRUE", "FALSE")), waypoint = "78040"),
    "must be a logical"
  )
  expect_error(
    resolve_waypoint_mode(lanes(2, c(1, 0)), waypoint = "78040"),
    "must be a logical"
  )
})

test_that("NA flags are treated as FALSE with a message", {
  expect_message(
    res <- resolve_waypoint_mode(lanes(3, c(TRUE, NA, FALSE)), waypoint = "78040"),
    "1 lane\\(s\\) have NA"
  )

  expect_equal(res$use_waypoint, c(TRUE, FALSE, FALSE))
})

test_that("a column that differs only by case is reported, not silently ignored", {
  df <- lanes(2)
  df$use_waypoint <- c(TRUE, FALSE)

  expect_error(resolve_waypoint_mode(df, "78040"), "named exactly `Use_Waypoint`")
})

test_that("df must be a data frame", {
  expect_error(resolve_waypoint_mode(list(a = 1)), "must be a data frame")
})

test_that("a zero-row data frame is handled", {
  res <- resolve_waypoint_mode(lanes(0))

  expect_equal(res$mode, "none")
  expect_length(res$use_waypoint, 0)
})

# --- Parsing the waypoint ----------------------------------------------------

test_that("a bare US zip is a zip and the USA country is inferred", {
  wp <- parse_waypoint("78040")

  expect_equal(wp$type, "zip")
  expect_equal(wp$value, "78040")
  expect_equal(wp$country, "USA")
  expect_true(wp$country_inferred)
})

test_that("ZIP+4 is recognized as a zip", {
  wp <- parse_waypoint("78040-1234")

  expect_equal(wp$type, "zip")
  expect_equal(wp$zip, "78040-1234")
})

test_that("a Canadian postal code is normalized and the country inferred", {
  expect_equal(parse_waypoint("L8E2B4")$zip, "L8E 2B4")
  expect_equal(parse_waypoint("l8e 2b4")$zip, "L8E 2B4")

  wp <- parse_waypoint("L8E2B4")
  expect_equal(wp$country, "CAN")
  expect_true(wp$country_inferred)
})

test_that("'City, ST' is a city_state and the country comes from the state", {
  wp <- parse_waypoint("Laredo, TX")

  expect_equal(wp$type, "city_state")
  expect_equal(wp$city, "Laredo")
  expect_equal(wp$state, "TX")
  expect_equal(wp$value, "Laredo, TX")
  expect_equal(wp$country, "USA")

  expect_equal(parse_waypoint("Laredo,tx")$state, "TX")
})

test_that("an ambiguous or unknown state does not guess a country", {
  expect_true(is.na(parse_waypoint("Monterrey, NL")$country))
  expect_true(is.na(parse_waypoint("Mexico City, DF")$country))
})

test_that("other text is a free-text address", {
  wp <- parse_waypoint("1200 Main St, Laredo, TX 78040")

  expect_equal(wp$type, "address")
  expect_equal(wp$street, "1200 Main St, Laredo, TX 78040")
})

test_that("coordinates must be named and are order independent", {
  wp <- parse_waypoint(c(lat = 27.5036, lon = -99.5075))
  expect_equal(wp$type, "coordinates")
  expect_equal(wp$lat, 27.5036)
  expect_equal(wp$lon, -99.5075)
  expect_equal(wp$value, "27.50360,-99.50750")

  swapped <- parse_waypoint(c(lon = -99.5075, lat = 27.5036))
  expect_equal(swapped$lat, 27.5036)

  expect_error(parse_waypoint(c(27.5036, -99.5075)), "must be named")
})

test_that("out-of-range or incomplete coordinates are errors", {
  expect_error(parse_waypoint(c(lat = 95, lon = 0)), "between -90 and 90")
  expect_error(parse_waypoint(c(lat = 0, lon = 200)), "between -180 and 180")
  expect_error(parse_waypoint(list(lat = 27.5)), "both `lat` and `lon`")
})

test_that("a component list can state the country explicitly", {
  wp <- parse_waypoint(list(zip = "78040", country = "US"))

  expect_equal(wp$type, "zip")
  expect_equal(wp$country, "USA")
  expect_false(wp$country_inferred)

  mx <- parse_waypoint(list(city = "Monterrey", state = "NL", country = "MX"))
  expect_equal(mx$type, "city_state")
  expect_equal(mx$country, "MEX")
})

test_that("a component list with a street address is an address", {
  wp <- parse_waypoint(list(street = "1 Main St", city = "Laredo", state = "TX", zip = "78040"))

  expect_equal(wp$type, "address")
  expect_equal(wp$value, "1 Main St, Laredo, TX, 78040")
})

test_that("component aliases and numeric zips are accepted", {
  expect_equal(parse_waypoint(list(postal_code = 78040))$zip, "78040")
  expect_equal(parse_waypoint(list(latitude = 27.5, longitude = -99.5))$type, "coordinates")
})

test_that("bad waypoint input gives a clear error", {
  expect_error(parse_waypoint(NA_character_), "single non-empty string")
  expect_error(parse_waypoint(""), "single non-empty string")
  expect_error(parse_waypoint(c("78040", "78041")), "single non-empty string")
  expect_error(parse_waypoint(list(planet = "Mars")), "unknown component")
  expect_error(parse_waypoint(list(country = "US")), "needs coordinates")
  expect_error(parse_waypoint(TRUE), "expected a string")
})

test_that("resolve_waypoint_mode surfaces an invalid waypoint", {
  expect_error(
    resolve_waypoint_mode(lanes(1, TRUE), waypoint = c(lat = 95, lon = 0)),
    "Invalid `waypoint`"
  )
})
