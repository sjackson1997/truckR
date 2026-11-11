# --- Registering ----------------------------------------------------------------------

test_that("a valid provider can be registered and listed", {
  local_empty_registry()

  expect_equal(truckr_list_providers(), character(0))

  res <- register_provider("fake", fake_provider())

  expect_equal(res, "fake")
  expect_equal(truckr_list_providers(), "fake")
})

test_that("the provider list is sorted", {
  local_empty_registry()
  register_provider("zeta", fake_provider())
  register_provider("alpha", fake_provider())

  expect_equal(truckr_list_providers(), c("alpha", "zeta"))
})

test_that("the registered provider is returned unchanged", {
  local_empty_registry()
  p <- fake_provider()
  register_provider("fake", p)

  expect_identical(get_provider("fake")$provider, p)
})

test_that("registering a name twice is an error unless overwrite is TRUE", {
  local_empty_registry()
  register_provider("fake", fake_provider())

  expect_error(register_provider("fake", fake_provider()), "already registered")
  expect_no_error(register_provider("fake", fake_provider(profile = "other"), overwrite = TRUE))
  expect_equal(get_provider("fake")$provider$profile, "other")
})

test_that("limits and profile are optional", {
  local_empty_registry()

  expect_no_error(register_provider("minimal", fake_provider(limits = NULL, profile = NULL)))
})

test_that("a function that takes ... satisfies the argument requirement", {
  local_empty_registry()

  expect_no_error(register_provider(
    "dots",
    fake_provider(geocode = function(...) NULL, route_segment = function(...) NULL)
  ))
})

# --- Rejecting bad providers -------------------------------------------------------------

test_that("provider names must be simple lower-case names", {
  local_empty_registry()

  for (bad in list("ORS", "", "my-provider", "1abc", "has space", NA_character_, 5, c("a", "b"))) {
    expect_error(register_provider(bad, fake_provider()), "provider name must be")
  }
  expect_no_error(register_provider("google_maps2", fake_provider()))
})

test_that("a provider that is not a named list is rejected", {
  local_empty_registry()

  expect_error(register_provider("bad", "ors"), "named list of functions")
  expect_error(register_provider("bad", data.frame(a = 1)), "named list of functions")
  expect_error(register_provider("bad", list()), "missing required element")
})

test_that("missing functions are all named in the message", {
  local_empty_registry()

  err <- expect_error(
    register_provider("bad", list(validate_credentials = function() TRUE)),
    "missing required element"
  )
  expect_match(conditionMessage(err), "geocode")
  expect_match(conditionMessage(err), "route_segment")
  expect_false(grepl("validate_credentials,", conditionMessage(err), fixed = TRUE))
})

test_that("a required element that is not a function is rejected", {
  local_empty_registry()

  expect_error(
    register_provider("bad", fake_provider(geocode = "not a function")),
    "`geocode` must be a function"
  )
})

test_that("functions with too few arguments are rejected", {
  local_empty_registry()

  expect_error(
    register_provider("bad", fake_provider(geocode = function() NULL)),
    "`geocode` must accept at least 1"
  )
  expect_error(
    register_provider("bad", fake_provider(route_segment = function(from) NULL)),
    "`route_segment` must accept at least 2"
  )
})

test_that("bad limits and profile are rejected", {
  local_empty_registry()

  expect_error(register_provider("bad", fake_provider(limits = 40)), "`limits` must be a list")
  expect_error(
    register_provider("bad", fake_provider(limits = list(requests_per_minute = -1))),
    "positive number"
  )
  expect_error(
    register_provider("bad", fake_provider(limits = list(requests_per_minute = "40"))),
    "positive number"
  )
  expect_error(register_provider("bad", fake_provider(profile = "")), "`profile` must be")
  expect_error(register_provider("bad", fake_provider(profile = c("a", "b"))), "`profile` must be")
})

test_that("a rejected provider is not registered", {
  local_empty_registry()
  try(register_provider("bad", list()), silent = TRUE)

  expect_equal(truckr_list_providers(), character(0))
})

# --- Choosing the active provider -------------------------------------------------------------

test_that("no provider is active until one is chosen", {
  local_empty_registry()
  register_provider("fake", fake_provider())

  expect_null(truckr_get_provider())
})

test_that("a registered provider can be chosen and read back", {
  local_empty_registry()
  register_provider("fake", fake_provider())

  expect_equal(truckr_set_provider("fake"), "fake")
  expect_equal(truckr_get_provider(), "fake")
})

test_that("choosing an unregistered provider lists the available ones", {
  local_empty_registry()
  register_provider("alpha", fake_provider())
  register_provider("beta", fake_provider())

  expect_error(truckr_set_provider("gamma"), "`gamma` is not registered. Available providers: alpha, beta")
  expect_null(truckr_get_provider())
})

test_that("the error says so when nothing is registered", {
  local_empty_registry()

  expect_error(truckr_set_provider("ors"), "none registered")
})

test_that("provider_name must be a single string", {
  local_empty_registry()

  expect_error(truckr_set_provider(NA_character_), "single string")
  expect_error(truckr_set_provider(c("a", "b")), "single string")
  expect_error(truckr_set_provider(1), "single string")
})

test_that("a failed choice leaves the previous provider active", {
  local_empty_registry()
  register_provider("fake", fake_provider())
  truckr_set_provider("fake")

  try(truckr_set_provider("nope"), silent = TRUE)

  expect_equal(truckr_get_provider(), "fake")
})

# --- Resolving the provider to use -----------------------------------------------------------------

test_that("resolve_provider stops clearly when none is configured", {
  local_empty_registry()
  register_provider("fake", fake_provider())

  expect_error(resolve_provider(), "No routing provider is configured")
  expect_error(resolve_provider(), "truckr_set_provider")
  expect_error(resolve_provider(), "Available providers: fake")
})

test_that("resolve_provider uses the active provider, or an explicit name", {
  local_empty_registry()
  register_provider("one", fake_provider(profile = "p1"))
  register_provider("two", fake_provider(profile = "p2"))
  truckr_set_provider("one")

  expect_equal(resolve_provider()$name, "one")
  expect_equal(resolve_provider()$provider$profile, "p1")
  expect_equal(resolve_provider("two")$name, "two")
  expect_equal(resolve_provider("two")$provider$profile, "p2")
})

test_that("resolve_provider rejects an unknown explicit name", {
  local_empty_registry()
  register_provider("one", fake_provider())

  expect_error(resolve_provider("nope"), "`nope` is not registered")
})

# --- The optional remaining_quota function -------------------------------------------------------------------------

test_that("a provider may report its remaining quota", {
  local_empty_registry()

  expect_no_error(register_provider("with_quota", fake_provider(remaining_quota = function(kind) 5)))
  expect_no_error(register_provider("dots_quota", fake_provider(remaining_quota = function(...) 5)))
})

test_that("remaining_quota must be a function that takes the kind of request", {
  local_empty_registry()

  expect_error(
    register_provider("bad", fake_provider(remaining_quota = 5)),
    "`remaining_quota` must be a function"
  )
  expect_error(
    register_provider("bad", fake_provider(remaining_quota = function() 5)),
    "`remaining_quota` must accept at least 1 argument"
  )
})
