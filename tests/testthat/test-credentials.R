secret <- "sk-test-SECRET-1234567890abcdef"

test_that("the variable name follows TRUCKR_<PROVIDER>_API_KEY", {
  expect_equal(key_env_var("ors"), "TRUCKR_ORS_API_KEY")
  expect_equal(key_env_var("google_maps"), "TRUCKR_GOOGLE_MAPS_API_KEY")
  expect_error(key_env_var("ORS"), "provider name must be")
})

test_that("truckr_has_key is FALSE when the variable is unset, empty or blank", {
  withr::local_envvar(TRUCKR_ORS_API_KEY = NA)
  expect_false(truckr_has_key("ors"))

  withr::local_envvar(TRUCKR_ORS_API_KEY = "")
  expect_false(truckr_has_key("ors"))

  withr::local_envvar(TRUCKR_ORS_API_KEY = "   ")
  expect_false(truckr_has_key("ors"))
})

test_that("truckr_has_key is TRUE when a key is set, and only for that provider", {
  withr::local_envvar(TRUCKR_ORS_API_KEY = secret, TRUCKR_GOOGLE_API_KEY = NA)

  expect_true(truckr_has_key("ors"))
  expect_false(truckr_has_key("google"))
})

test_that("truckr_has_key returns only TRUE or FALSE, never the key", {
  withr::local_envvar(TRUCKR_ORS_API_KEY = secret)

  out <- truckr_has_key("ors")

  expect_type(out, "logical")
  expect_length(out, 1)
  expect_false(grepl(secret, paste(capture.output(print(out)), collapse = " "), fixed = TRUE))
})

test_that("get_api_key returns the key, trimmed", {
  withr::local_envvar(TRUCKR_ORS_API_KEY = paste0("  ", secret, " "))

  expect_equal(get_api_key("ors"), secret)
})

test_that("a missing key stops with a message that names the variable and how to set it", {
  withr::local_envvar(TRUCKR_ORS_API_KEY = NA)

  err <- expect_error(get_api_key("ors"), "TRUCKR_ORS_API_KEY")
  msg <- conditionMessage(err)

  expect_match(msg, "provider `ors`")
  expect_match(msg, ".Renviron", fixed = TRUE)
  expect_match(msg, "edit_r_environ", fixed = TRUE)
  expect_match(msg, "Sys.setenv", fixed = TRUE)
})

test_that("a blank key counts as missing", {
  withr::local_envvar(TRUCKR_ORS_API_KEY = "   ")

  expect_error(get_api_key("ors"), "No API key found")
})

test_that("no message produced while a key is set contains the key", {
  withr::local_envvar(TRUCKR_ORS_API_KEY = secret)
  local_empty_registry()

  msgs <- testthat::capture_messages({
    register_provider("ors", fake_provider())
    truckr_set_provider("ors")
    truckr_has_key("ors")
    get_api_key("ors")
  })
  err <- tryCatch(get_api_key("google"), error = function(e) conditionMessage(e))

  expect_false(any(grepl(secret, msgs, fixed = TRUE)))
  expect_false(grepl(secret, err, fixed = TRUE))
})

test_that("redact_key removes every occurrence of the key", {
  withr::local_envvar(TRUCKR_ORS_API_KEY = secret)
  url <- paste0("https://example.com/search?api_key=", secret, "&text=x&again=", secret)

  out <- redact_key(url, "ors")

  expect_false(grepl(secret, out, fixed = TRUE))
  expect_equal(
    out,
    "https://example.com/search?api_key=[API key removed]&text=x&again=[API key removed]"
  )
})

test_that("redact_key handles vectors, NA and text without the key", {
  withr::local_envvar(TRUCKR_ORS_API_KEY = secret)

  out <- redact_key(c("nothing here", paste("key:", secret), NA), "ors")

  expect_equal(out, c("nothing here", "key: [API key removed]", NA))
})

test_that("redact_key leaves text alone when no key is set", {
  withr::local_envvar(TRUCKR_ORS_API_KEY = NA)

  expect_equal(redact_key("some text", "ors"), "some text")
})
