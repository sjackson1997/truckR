gfx <- function(name) read_fixture(paste0("ors_geocode_", name))

# The coordinates of feature `i` in a fixture, for comparing with a result.
fixture_point <- function(name, i = 1) {
  unlist(gfx(name)$body$features[[i]]$geometry$coordinates)[1:2]
}

query_of <- function(log, i = 1) log$calls[[i]]$query

city_layers_text <- "locality,localadmin,borough,neighbourhood"
result_names <- c(
  "lon", "lat", "match_layer", "label", "flag", "strategy",
  "error", "error_permanent", "error_kind"
)

# --- Coordinates and shape of the result ---------------------------------------------------

test_that("coordinates are returned as they are, with no request", {
  log <- local_ors(list())

  got <- ors_geocode(list(lat = 27.5036, lon = -99.5075))

  expect_equal(c(got$lon, got$lat), c(-99.5075, 27.5036))
  expect_equal(got$strategy, "coordinates")
  expect_true(is.na(got$error))
  expect_length(log$calls, 0)
})

test_that("coordinates must be valid", {
  local_ors(list())

  expect_error(ors_geocode(list(lat = 95, lon = 0)), "outside the valid range")
  expect_error(ors_geocode(list(lat = 27.5)), "must be two numbers")
})

test_that("a result always has the same fields, on success and on failure", {
  local_ors(list(gfx("us_zip")))
  ok <- ors_geocode(list(zip = "78040", country = "USA"))
  fail <- ors_geocode(list(zip = "45134", country = "MEX"))

  expect_named(ok, result_names)
  expect_named(fail, result_names)
})

test_that("a location can be a one-row data frame", {
  local_ors(list(gfx("us_zip")))

  got <- ors_geocode(data.frame(zip = "78040", country = "USA", stringsAsFactors = FALSE))

  expect_equal(c(got$lon, got$lat), fixture_point("us_zip"))
  expect_error(ors_geocode(data.frame(zip = c("1", "2"))), "one row")
  expect_error(ors_geocode("78040"), "must be a list")
})

# --- United States ----------------------------------------------------------------------------

test_that("a US zip is looked up on the postal code layer", {
  log <- local_ors(list(gfx("us_zip")))

  got <- ors_geocode(list(zip = "78040", state = "TX", country = "USA"))
  q <- query_of(log)

  expect_equal(q$text, "78040")
  expect_equal(q$layers, "postalcode")
  expect_equal(q$boundary.country, "US")
  expect_equal(q$size, 3)
  expect_equal(q$api_key, ors_test_key)
  expect_equal(c(got$lon, got$lat), fixture_point("us_zip"))
  expect_equal(got$match_layer, "postalcode")
  expect_equal(got$label, "78040, Laredo, TX, USA")
  expect_equal(got$strategy, "postal code")
  expect_true(is.na(got$flag))
  expect_true(is.na(got$error))
  expect_length(log$calls, 1)
})

test_that("a ZIP+4 is looked up by its first five digits", {
  log <- local_ors(list(gfx("us_zip")))

  ors_geocode(list(zip = "78040-1234", country = "USA"))

  expect_equal(query_of(log)$text, "78040")
})

test_that("a zip in the wrong state is rejected, not trusted", {
  local_ors(list(gfx("us_zip")))

  got <- ors_geocode(list(zip = "78040", state = "OK", country = "USA"))

  expect_true(is.na(got$lon))
  expect_equal(got$error_kind, "no_match")
  expect_true(got$error_permanent)
  expect_match(got$error, "78040, Laredo, TX, USA is in TX, not OK")
})

test_that("an invalid US zip is not sent; the city is used and the flag says why", {
  log <- local_ors(list(gfx("us_city")))

  got <- ors_geocode(list(zip = "123", city = "Laredo", state = "TX", country = "USA"))

  expect_length(log$calls, 1)
  expect_equal(query_of(log)$layers, city_layers_text)
  expect_equal(got$strategy, "city and state")
  expect_match(got$flag, "postal code '123' is not a valid US postal code and was ignored")
})

test_that("a US city is looked up with its state", {
  log <- local_ors(list(gfx("us_city")))

  got <- ors_geocode(list(city = "LAREDO", state = "TX", country = "USA"))
  q <- query_of(log)

  expect_equal(q$text, "LAREDO, TX")
  expect_equal(q$layers, city_layers_text)
  expect_equal(q$size, 5)
  expect_equal(q$boundary.country, "US")
  expect_equal(c(got$lon, got$lat), fixture_point("us_city"))
  expect_equal(got$strategy, "city and state")
  expect_true(is.na(got$flag))
})

test_that("a candidate with the wrong name is skipped (Springfield Township is not Springfield)", {
  local_ors(list(gfx("us_springfield")))

  got <- ors_geocode(list(city = "Springfield", state = "IL", country = "USA"))

  expect_equal(got$label, "Springfield, IL, USA")
  expect_equal(got$match_layer, "locality")
})

test_that("a US city with no state is refused when the name is shared by several places", {
  local_ors(list(gfx("us_springfield")))

  # Springfield, IL appears twice in the fixture at different coordinates.
  got <- ors_geocode(list(city = "Springfield", country = "USA"))

  expect_equal(got$error_kind, "no_match")
  expect_match(got$error, "different places")
  expect_match(got$error, "no state was given")
})

test_that("a US street address is looked up on the address layer", {
  log <- local_ors(list(gfx("us_address")))

  got <- ors_geocode(list(
    street = "1600 Pennsylvania Avenue NW", city = "Washington", state = "DC",
    zip = "20500", country = "USA"
  ))
  q <- query_of(log)

  expect_equal(q$text, "1600 Pennsylvania Avenue NW, Washington, DC, 20500")
  expect_equal(q$layers, "address")
  expect_equal(q$size, 3)
  expect_equal(got$match_layer, "address")
  expect_equal(got$strategy, "street address")
  expect_equal(c(got$lon, got$lat), fixture_point("us_address"))
  expect_true(is.na(got$flag))
})

test_that("an address that is not found falls back to the postal code and says so", {
  log <- local_ors(list(gfx("us_address_missing"), gfx("us_zip")))

  got <- ors_geocode(list(
    street = "99999 Nowhere Road", city = "Laredo", state = "TX", zip = "78040", country = "USA"
  ))

  expect_length(log$calls, 2)
  expect_equal(query_of(log, 1)$layers, "address")
  expect_equal(query_of(log, 2)$layers, "postalcode")
  expect_equal(got$strategy, "postal code")
  expect_equal(got$match_layer, "postalcode")
  expect_equal(got$flag, "street address not matched; located by postal code instead")
})

test_that("an address that is not found falls back to the city when there is no zip", {
  log <- local_ors(list(gfx("us_address_missing"), gfx("us_city")))

  got <- ors_geocode(list(street = "99999 Nowhere Road", city = "Laredo", state = "TX", country = "USA"))

  expect_length(log$calls, 2)
  expect_equal(got$strategy, "city and state")
  expect_equal(got$flag, "street address not matched; located by city instead")
})

test_that("the city's 'fallback' answer to a missing address is never accepted as an address", {
  log <- local_ors(list(gfx("us_address_missing")))

  # Only a street is known, so the street lookup is the only one available.
  got <- ors_geocode(list(street = "99999 Nowhere Road", state = "TX", country = "USA"))

  expect_length(log$calls, 1)
  expect_true(is.na(got$lon))
  expect_equal(got$error_kind, "no_match")
  expect_match(got$error, "Laredo, TX, USA is not at the level asked for")
})

test_that("with a street, zip and city all missed, the flag names everything that was missed", {
  local_ors(list(gfx("us_address_missing"), gfx("mx_zip"), gfx("us_city")))

  got <- ors_geocode(list(
    street = "99999 Nowhere Road", city = "Laredo", state = "TX", zip = "78040", country = "USA"
  ))

  expect_equal(got$flag, "street address and postal code not matched; located by city instead")
})

# --- Canada ----------------------------------------------------------------------------------------------

test_that("a Canadian postal code is looked up on the postal code layer", {
  log <- local_ors(list(gfx("ca_postal")))

  got <- ors_geocode(list(city = "STONEY CREEK", state = "ON", zip = "L8E 2B4", country = "CAN"))
  q <- query_of(log)

  expect_equal(q$text, "L8E 2B4")
  expect_equal(q$layers, "postalcode")
  expect_equal(q$boundary.country, "CA")
  expect_equal(got$label, "L8E 2B4, Hamilton, ON, Canada")
  expect_equal(c(got$lon, got$lat), fixture_point("ca_postal"))
  expect_equal(got$strategy, "postal code")
  expect_true(is.na(got$flag))
  expect_length(log$calls, 1)
})

test_that("a postal code without its space is spaced before it is sent", {
  log <- local_ors(list(gfx("ca_postal")))

  ors_geocode(list(zip = "l8e2b4", country = "CAN"))

  expect_equal(query_of(log)$text, "L8E 2B4")
})

test_that("a truncated Canadian postal code is not sent", {
  log <- local_ors(list(gfx("ca_city")))

  got <- ors_geocode(list(city = "Stoney Creek", state = "ON", zip = "L8E", country = "CAN"))

  expect_length(log$calls, 1)
  expect_equal(query_of(log)$layers, city_layers_text)
  expect_match(got$error, "postal code 'L8E' is not a valid Canadian postal code and was ignored")
})

test_that("a city search that only finds approximate matches in other towns finds nothing", {
  local_ors(list(gfx("ca_city")))

  got <- ors_geocode(list(city = "Stoney Creek", state = "ON", country = "CAN"))

  expect_true(is.na(got$lon))
  expect_equal(got$error_kind, "no_match")
  expect_match(got$error, "only an approximate match")
})

test_that("a real Canadian city is found", {
  log <- local_ors(list(gfx("ca_brampton")))

  got <- ors_geocode(list(city = "BRAMPTON", state = "ON", country = "CAN"))

  expect_equal(query_of(log)$text, "BRAMPTON, ON")
  expect_equal(query_of(log)$boundary.country, "CA")
  expect_equal(got$label, "Brampton, ON, Canada")
  expect_equal(got$strategy, "city and state")
})

test_that("a Canadian street address is looked up on the address layer", {
  log <- local_ors(list(gfx("ca_address")))

  got <- ors_geocode(list(
    street = "290 Bremner Blvd", city = "Toronto", state = "ON", zip = "M5V 3L9", country = "CAN"
  ))

  expect_equal(query_of(log)$text, "290 Bremner Blvd, Toronto, ON, M5V 3L9")
  expect_equal(got$match_layer, "address")
  expect_equal(c(got$lon, got$lat), fixture_point("ca_address"))
})

# --- Mexico ---------------------------------------------------------------------------------------------------

test_that("a Mexican postal code alone is refused without any request", {
  log <- local_ors(list(gfx("mx_zip")))

  got <- ors_geocode(list(zip = "45134", country = "MEX"))

  expect_length(log$calls, 0)
  expect_match(got$error, "zip-only location not supported by this provider")
  expect_true(got$error_permanent)
  expect_equal(got$error_kind, "unsupported")
  expect_true(is.na(got$lon))
})

test_that("a Mexican postal code is never sent, whatever else is known", {
  for (loc in list(
    list(zip = "45134", city = "Zapopan", state = "JAL", country = "MEX"),
    list(zip = "45134", street = "Av Vallarta 1", city = "Zapopan", state = "JAL", country = "MEX"),
    list(zip = "45134", city = "Zapopan", country = "MEX")
  )) {
    plan <- ors_geocode_plan(ors_location(loc))
    layers <- vapply(plan$steps, function(s) s$query$layers, character(1))

    expect_false(any(grepl("postalcode", layers)))
    expect_false(any(vapply(plan$steps, function(s) grepl("45134", s$query$text), logical(1))))
  }
})

test_that("a Mexican city is searched with the full state name, not the code", {
  log <- local_ors(list(gfx("mx_zapopan_name")))

  got <- ors_geocode(list(city = "ZAPOPAN", state = "JAL", zip = "45134", country = "MEX"))
  q <- query_of(log)

  expect_equal(q$text, "ZAPOPAN, Jalisco")
  expect_equal(q$layers, city_layers_text)
  expect_equal(q$boundary.country, "MX")
  expect_equal(got$label, "Zapopan, JAL, Mexico")
  expect_equal(c(got$lon, got$lat), fixture_point("mx_zapopan_name"))
  expect_equal(got$strategy, "city and state")
  expect_true(is.na(got$flag))
  expect_length(log$calls, 1)
})

test_that("the state code in the data is matched to the state ORS returns (NL is NLE)", {
  log <- local_ors(list(gfx("mx_salinas_name")))

  got <- ors_geocode(list(city = "SALINAS VICTORIA", state = "NL", zip = "66616", country = "MEX"))

  expect_equal(query_of(log)$text, "SALINAS VICTORIA, Nuevo Leon")
  expect_equal(got$label, "Salinas Victoria, NLE, Mexico")
  expect_true(is.na(got$flag))
})

test_that("a name the search cannot place is retried on its own and checked against the state", {
  log <- local_ors(list(gfx("mx_guadalupe_name"), gfx("mx_guadalupe_only")))

  got <- ors_geocode(list(city = "GUADALUPE", state = "NL", zip = "67110", country = "MEX"))

  expect_length(log$calls, 2)
  expect_equal(query_of(log, 1)$text, "GUADALUPE, Nuevo Leon")
  expect_equal(query_of(log, 1)$size, 5)
  expect_equal(query_of(log, 2)$text, "GUADALUPE")
  expect_equal(query_of(log, 2)$size, 10)
  expect_equal(got$label, "Guadalupe, NLE, Mexico")
  expect_equal(got$strategy, "city name alone, checked against the state")
  expect_match(got$flag, "located by city name alone; the state was used to choose")
  expect_false(grepl("not matched", got$flag))
})

test_that("an exact match in the wrong state is rejected (Guadalupe in Tamaulipas is not Guadalupe in Nuevo Leon)", {
  log <- local_ors(list(gfx("mx_guadalupe"), gfx("mx_guadalupe_only")))

  got <- ors_geocode(list(city = "GUADALUPE", state = "NL", country = "MEX"))

  expect_equal(got$label, "Guadalupe, NLE, Mexico")
  expect_length(log$calls, 2)
})

test_that("when no candidate can be confirmed in the state, nothing is returned", {
  local_ors(list(gfx("mx_guadalupe_name"), gfx("mx_guadalupe_only")))

  got <- ors_geocode(list(city = "GUADALUPE", state = "DF", country = "MEX"))

  expect_true(is.na(got$lon))
  expect_equal(got$error_kind, "no_match")
  expect_match(got$error, "is in NLE, not DF")
})

test_that("results with the wrong names are skipped even when they are 'exact'", {
  local_ors(list(gfx("mx_salinas"), gfx("mx_salinas")))

  got <- ors_geocode(list(city = "SALINAS VICTORIA", state = "NL", country = "MEX"))

  expect_true(is.na(got$lon))
  expect_match(got$error, "Durango, Mexico is not named SALINAS VICTORIA")
})

test_that("a Mexican city with no state is refused when several places share the name", {
  log <- local_ors(list(gfx("mx_zapopan_only")))

  got <- ors_geocode(list(city = "Zapopan", country = "MEX"))

  expect_equal(query_of(log)$text, "Zapopan")
  expect_equal(query_of(log)$size, 10)
  expect_length(log$calls, 1)
  expect_equal(got$error_kind, "no_match")
  expect_match(got$error, "different places and no state was given")
})

test_that("an unrecognized state is ignored, and the note appears in the failure", {
  log <- local_ors(list(gfx("mx_zapopan_only")))

  got <- ors_geocode(list(city = "Zapopan", state = "XX", country = "MEX"))

  expect_equal(query_of(log)$text, "Zapopan")
  expect_match(got$error, "state 'XX' was not recognized and was ignored")
})

test_that("a Mexican street address is looked up on the address layer", {
  log <- local_ors(list(gfx("mx_address")))

  got <- ors_geocode(list(street = "Paseo de la Reforma 222", city = "Ciudad de Mexico", country = "MEX"))

  expect_equal(query_of(log)$text, "Paseo de la Reforma 222, Ciudad de Mexico")
  expect_equal(query_of(log)$layers, "address")
  expect_equal(got$match_layer, "address")
  expect_equal(c(got$lon, got$lat), fixture_point("mx_address", 1))
})

# --- No country -----------------------------------------------------------------------------------------------------

test_that("with no country the search has no country restriction and the flag says so", {
  log <- local_ors(list(gfx("us_city")))

  got <- ors_geocode(list(city = "Laredo", state = "TX"))

  expect_null(query_of(log)$boundary.country)
  expect_equal(query_of(log)$text, "Laredo, TX")
  expect_equal(got$label, "Laredo, TX, USA")
  expect_match(got$flag, "country not given; matched without a country restriction")
})

test_that("a postal code alone cannot be used without a country", {
  log <- local_ors(list(gfx("us_zip")))

  got <- ors_geocode(list(zip = "78040"))

  expect_length(log$calls, 0)
  expect_match(got$error, "postal code alone cannot be looked up without a country")
  expect_true(got$error_permanent)
})

test_that("a postal code is set aside, with a note, when the country is not given", {
  log <- local_ors(list(gfx("us_city")))

  got <- ors_geocode(list(zip = "78040", city = "Laredo", state = "TX"))

  expect_length(log$calls, 1)
  expect_match(got$flag, "postal code was not used because the country is not given")
})

test_that("an empty location and an unsupported country are refused without a request", {
  log <- local_ors(list())

  empty <- ors_geocode(list())
  france <- ors_geocode(list(city = "Paris", country = "FRA"))

  expect_match(empty$error, "no street address, postal code or city")
  expect_equal(empty$error_kind, "unsupported")
  expect_match(france$error, "country FRA is not supported by this provider yet")
  expect_true(france$error_permanent)
  expect_length(log$calls, 0)
})

# --- Failures of the request itself -------------------------------------------------------------------------------------

test_that("a temporary failure is returned as it is and never replaced by a coarser answer", {
  log <- local_ors(rep(list(raw_network("Timeout was reached")), 3))

  got <- suppressMessages(ors_geocode(list(
    street = "1 Main St", zip = "78040", city = "Laredo", state = "TX", country = "USA"
  )))

  expect_equal(got$error_kind, "network")
  expect_false(got$error_permanent)
  expect_true(is.na(got$lon))
  expect_length(log$calls, 3)                          # three attempts, all at the first lookup
  expect_true(all(vapply(log$calls, function(c) c$query$layers == "address", logical(1))))
})

test_that("an exhausted quota stops the lookup", {
  log <- local_ors(list(raw_status(429, headers = limits(3000, 0)), gfx("us_zip")))

  got <- ors_geocode(list(street = "1 Main St", zip = "78040", country = "USA"))

  expect_equal(got$error_kind, "quota")
  expect_false(got$error_permanent)
  expect_length(log$calls, 1)
})

test_that("a request ORS rejects outright is a permanent failure", {
  local_ors(list(raw_error(400, "layers parameter is invalid")))

  got <- ors_geocode(list(zip = "78040", country = "USA"))

  expect_equal(got$error_kind, "bad_request")
  expect_true(got$error_permanent)
})

test_that("features that cannot be read are skipped", {
  broken <- raw_ok(list(features = list(
    list(),
    list(properties = list(layer = "postalcode"), geometry = list(coordinates = list(1))),
    list(properties = list(layer = "postalcode", match_type = "exact", label = "x"),
         geometry = list(coordinates = list(500, 1))),
    "text"
  )))
  local_ors(list(broken))

  got <- ors_geocode(list(zip = "78040", country = "USA"))

  expect_equal(got$error_kind, "no_match")
})

test_that("a response with no features is a no match", {
  local_ors(list(raw_ok(list(features = list()))))

  got <- ors_geocode(list(zip = "78040", country = "USA"))

  expect_equal(got$error_kind, "no_match")
  expect_match(got$error, "no match found for 78040, USA")
})

test_that("a response with no features key is a no match, not an error", {
  local_ors(list(raw_ok(list(other = 1))))

  got <- ors_geocode(list(zip = "78040", country = "USA"))

  expect_equal(got$error_kind, "no_match")
})

# --- The cache (Property 14) -------------------------------------------------------------------------------------------------

test_that("each kind of result is cached the way the failure rules say", {
  local_cache()
  s <- cache_store("ors", "geocode")

  cases <- list(
    success = list(list(gfx("us_zip")), list(zip = "78040", country = "USA"), "stored"),
    no_match = list(list(raw_ok(list(features = list()))), list(zip = "78040", country = "USA"), "memory_only"),
    zip_only_mexico = list(list(), list(zip = "45134", country = "MEX"), "memory_only"),
    quota = list(list(raw_status(429, headers = limits(3000, 0))), list(zip = "78040", country = "USA"), "not_stored"),
    network = list(rep(list(raw_network()), 3), list(zip = "78040", country = "USA"), "not_stored")
  )

  for (label in names(cases)) {
    local_ors(cases[[label]][[1]])

    got <- suppressMessages(ors_geocode(cases[[label]][[2]]))
    stored <- cache_put(s, label, got)

    expect_equal(stored, cases[[label]][[3]], label = label)
  }
})

test_that("a found location is read back from the cache with its coordinates and flag", {
  local_cache()
  local_ors(list(gfx("us_address_missing"), gfx("us_zip")))
  s <- cache_store("ors", "geocode")
  loc <- list(street = "99999 Nowhere Road", zip = "78040", state = "TX", country = "USA")
  key <- location_key(loc)

  got <- ors_geocode(loc)
  cache_put(s, key, got)
  hit <- cache_get(s, key)

  expect_equal(hit$value$lon, got$lon)
  expect_equal(hit$value$flag, got$flag)
  expect_false(hit$failure)
})

# --- The plan --------------------------------------------------------------------------------------------------------------------------

test_that("the lookups are tried from the most precise to the least", {
  plan <- ors_geocode_plan(ors_location(list(
    street = "1 Main St", city = "Laredo", state = "TX", zip = "78040", country = "USA"
  )))

  expect_equal(vapply(plan$steps, function(s) s$group, character(1)), c("street address", "postal code", "city"))
})

test_that("only Mexican locations get the second, name-alone city lookup", {
  mx <- ors_geocode_plan(ors_location(list(city = "Guadalupe", state = "NL", country = "MEX")))
  us <- ors_geocode_plan(ors_location(list(city = "Laredo", state = "TX", country = "USA")))
  mx_no_state <- ors_geocode_plan(ors_location(list(city = "Guadalupe", country = "MEX")))

  expect_length(mx$steps, 2)
  expect_length(us$steps, 1)
  expect_length(mx_no_state$steps, 1)
})

test_that("the key never appears in a result", {
  local_ors(rep(list(raw_network(paste("failed for api_key=", ors_test_key))), 3))

  msgs <- testthat::capture_messages(got <- ors_geocode(list(zip = "78040", country = "USA")))

  expect_false(grepl(ors_test_key, paste(unlist(got), collapse = " "), fixed = TRUE))
  expect_false(any(grepl(ors_test_key, msgs, fixed = TRUE)))
})
