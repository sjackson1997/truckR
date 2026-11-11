quota_message <- "quota exhausted; re-run after reset"

# --- Saved answers ------------------------------------------------------------------------

test_that("what is not saved is requested and saved, and a second run needs no requests", {
  local_cache()
  store <- cache_store("fake", "route")
  f <- engine_fetch()

  suppressMessages(first <- cached_requests(engine_items(4), store, f$fn))
  suppressMessages(second <- cached_requests(engine_items(4), store, f$fn))

  expect_equal(unname(first$status), rep("fetched", 4))
  expect_equal(first$results$k2$value, 20)
  expect_equal(first$requested, 4)
  expect_equal(unname(second$status), rep("cache", 4))
  expect_equal(second$requested, 0)
  expect_equal(second$saved, 4)
  expect_equal(second$results$k3$value, 30)
  expect_length(f$state$calls, 4)                 # none added by the second run
})

test_that("saved answers are used first and only the rest are requested", {
  local_cache()
  store <- cache_store("fake", "route")
  cache_put(store, "k1", list(value = 111, error = NA_character_))
  cache_put(store, "k2", list(value = 222, error = NA_character_))
  f <- engine_fetch()

  suppressMessages(got <- cached_requests(engine_items(4), store, f$fn))

  expect_equal(unname(got$status), c("cache", "cache", "fetched", "fetched"))
  expect_equal(f$state$calls, c("k3", "k4"))
  expect_equal(got$results$k1$value, 111)
  expect_equal(got$saved, 2)
})

test_that("an answer older than 120 days is requested again", {
  cc <- local_cache()
  store <- cache_store("fake", "route")
  f <- engine_fetch()
  suppressMessages(cached_requests(engine_items(2), store, f$fn))

  cc$clock$advance(days = 121)
  suppressMessages(got <- cached_requests(engine_items(2), store, f$fn))

  expect_equal(unname(got$status), c("fetched", "fetched"))
  expect_length(f$state$calls, 4)
})

test_that("results come back in the order of the items", {
  local_cache()
  store <- cache_store("fake", "route")
  cache_put(store, "k2", list(value = 2, error = NA_character_))
  f <- engine_fetch()

  suppressMessages(got <- cached_requests(engine_items(3), store, f$fn))

  expect_named(got$results, c("k1", "k2", "k3"))
  expect_named(got$status, c("k1", "k2", "k3"))
})

test_that("an empty set of items does nothing and says nothing", {
  local_cache()

  expect_no_message(got <- cached_requests(list(), cache_store("fake", "route"), engine_fetch()$fn))

  expect_length(got$results, 0)
  expect_equal(got$requested, 0)
})

test_that("when everything is saved no request is made and the message says so", {
  local_cache()
  store <- cache_store("fake", "route")
  f <- engine_fetch()
  suppressMessages(cached_requests(engine_items(3), store, f$fn))

  expect_message(
    got <- cached_requests(engine_items(3), store, f$fn, label = "Routing"),
    "Routing: all 3 are already saved; no requests needed."
  )
  expect_equal(got$requested, 0)
})

# --- The pre-flight message -----------------------------------------------------------------

test_that("the count of requests needed and the quota are announced up front", {
  local_cache()
  store <- cache_store("fake", "route")
  cache_put(store, "k1", list(value = 1, error = NA_character_))

  msgs <- testthat::capture_messages(
    cached_requests(engine_items(4), store, engine_fetch()$fn, remaining = function() 100, label = "Routing")
  )

  expect_match(msgs[1], "Routing: 3 requests needed \\(1 of 4 already saved\\); about 100 remain today.")
})

test_that("it says so when the quota will not be enough", {
  local_cache()

  msgs <- testthat::capture_messages(
    cached_requests(engine_items(5), cache_store("fake", "route"), engine_fetch()$fn, remaining = function() 2)
  )

  expect_match(msgs[1], "5 requests needed \\(0 of 5 already saved\\); only about 2 remain today, so as many as fit will be requested now")
})

test_that("it says so when the remaining quota is not known", {
  local_cache()

  msgs <- testthat::capture_messages(
    cached_requests(engine_items(2), cache_store("fake", "route"), engine_fetch()$fn)
  )

  expect_match(msgs[1], "2 requests needed \\(0 of 2 already saved\\); the remaining quota is not known yet.")
})

test_that("a single request is not written as a plural", {
  local_cache()

  msgs <- testthat::capture_messages(
    cached_requests(engine_items(1), cache_store("fake", "route"), engine_fetch()$fn)
  )

  expect_match(msgs[1], "1 request needed")
})

test_that("a problem asking for the remaining quota is treated as not known", {
  local_cache()
  f <- engine_fetch()

  suppressMessages(got <- cached_requests(
    engine_items(2), cache_store("fake", "route"), f$fn, remaining = function() stop("no idea")
  ))

  expect_equal(unname(got$status), c("fetched", "fetched"))
})

# --- Stopping when the quota runs out ------------------------------------------------------------------

test_that("the run stops before the known quota is used up, and a second run finishes the rest", {
  local_cache()
  store <- cache_store("fake", "route")
  f <- engine_fetch()
  left <- 2
  remaining <- function() max(0, left - length(f$state$calls))

  suppressMessages(first <- cached_requests(engine_items(5), store, f$fn, remaining = remaining))

  expect_equal(unname(first$status), c("fetched", "fetched", "stopped", "stopped", "stopped"))
  expect_equal(f$state$calls, c("k1", "k2"))             # never went past the quota
  expect_equal(first$stop_reason, "quota")
  expect_equal(first$not_requested, 3)
  expect_equal(first$results$k4$error, quota_message)
  expect_equal(first$results$k4$error_kind, "quota")
  expect_false(first$results$k4$error_permanent)

  # The quota resets; only the three that were not done are requested.
  f$state$calls <- character(0)
  left <- 100
  suppressMessages(second <- cached_requests(engine_items(5), store, f$fn, remaining = function() left))

  expect_equal(unname(second$status), c("cache", "cache", "fetched", "fetched", "fetched"))
  expect_equal(f$state$calls, c("k3", "k4", "k5"))
})

test_that("it tells the user when it stops and how to finish", {
  local_cache()
  f <- engine_fetch()

  msgs <- testthat::capture_messages(
    cached_requests(engine_items(3), cache_store("fake", "route"), f$fn,
                    remaining = function() 1 - length(f$state$calls), label = "Routing")
  )

  expect_match(
    msgs[length(msgs)],
    "Routing: stopped early because the quota is used up. 1 requested, 2 not requested. Answers already obtained are saved, so running again finishes the rest."
  )
})

test_that("a quota that becomes known after the first request is respected from then on", {
  local_cache()
  f <- engine_fetch()
  remaining <- function() if (length(f$state$calls) == 0) NA_real_ else max(0, 2 - length(f$state$calls))

  msgs <- testthat::capture_messages(
    got <- cached_requests(engine_items(4), cache_store("fake", "route"), f$fn, remaining = remaining)
  )

  expect_equal(unname(got$status), c("fetched", "fetched", "stopped", "stopped"))
  expect_true(any(grepl("about 1 requests remain today and 3 more are needed", msgs)))
})

test_that("a quota failure from the provider stops the run and is not saved", {
  local_cache()
  store <- cache_store("fake", "route")
  f <- engine_fetch(list(`3` = failure_reply("quota")))

  suppressMessages(got <- cached_requests(engine_items(5), store, f$fn))

  expect_equal(unname(got$status), c("fetched", "fetched", "stopped", "stopped", "stopped"))
  expect_equal(f$state$calls, c("k1", "k2", "k3"))
  expect_equal(got$stop_reason, "quota")
  expect_equal(got$results$k3$error, quota_message)
  expect_null(cache_get(store, "k3"))
})

test_that("with an unknown quota every item is requested", {
  local_cache()
  f <- engine_fetch()

  suppressMessages(got <- cached_requests(engine_items(6), cache_store("fake", "route"), f$fn))

  expect_length(f$state$calls, 6)
  expect_true(is.na(got$stop_reason))
})

# --- Stopping for other reasons -----------------------------------------------------------------------------

test_that("a rejected key stops the run at once", {
  local_cache()
  f <- engine_fetch(list(`1` = failure_reply("auth", error = "ORS rejected the API key")))

  suppressMessages(got <- cached_requests(engine_items(4), cache_store("fake", "route"), f$fn))

  expect_equal(f$state$calls, "k1")
  expect_equal(unname(got$status), c("failed", "stopped", "stopped", "stopped"))
  expect_equal(got$stop_reason, "auth")
  expect_equal(got$results$k1$error, "ORS rejected the API key")
  expect_equal(got$results$k2$error, "not requested: the API key was rejected")
})

test_that("several temporary failures in a row stop the run", {
  local_cache()
  withr::local_options(truckr.max_consecutive_failures = 3)
  f <- engine_fetch(list(`1` = failure_reply("network"), `2` = failure_reply("server"), `3` = failure_reply("network")))

  suppressMessages(got <- cached_requests(engine_items(6), cache_store("fake", "route"), f$fn))

  expect_equal(f$state$calls, c("k1", "k2", "k3"))
  expect_equal(got$stop_reason, "repeated")
  expect_equal(got$results$k4$error, "not requested: stopped after 3 requests in a row failed")
  expect_equal(got$not_requested, 3)
})

test_that("a success in between starts the count again", {
  local_cache()
  withr::local_options(truckr.max_consecutive_failures = 3)
  fails <- lapply(c(1, 2, 4, 5), function(i) failure_reply("network"))
  f <- engine_fetch(stats::setNames(fails, c("1", "2", "4", "5")))

  suppressMessages(got <- cached_requests(engine_items(6), cache_store("fake", "route"), f$fn))

  expect_true(is.na(got$stop_reason))
  expect_equal(unname(got$status), c("failed", "failed", "fetched", "failed", "failed", "fetched"))
})

test_that("a definite answer such as 'no route' does not count as a failure of the service", {
  local_cache()
  withr::local_options(truckr.max_consecutive_failures = 3)
  nothing <- failure_reply("not_found", permanent = TRUE)
  f <- engine_fetch(stats::setNames(rep(list(nothing), 5), as.character(1:5)))

  suppressMessages(got <- cached_requests(engine_items(6), cache_store("fake", "route"), f$fn))

  expect_true(is.na(got$stop_reason))
  expect_length(f$state$calls, 6)
  expect_equal(unname(got$status), c(rep("failed", 5), "fetched"))
})

test_that("the limit on failures in a row is a setting", {
  local_cache()
  withr::local_options(truckr.max_consecutive_failures = 0)

  expect_error(
    cached_requests(engine_items(2), cache_store("fake", "route"), engine_fetch()$fn),
    "`truckr.max_consecutive_failures` must be a single positive whole number"
  )
})

# --- What failures leave in the cache (Property 14) --------------------------------------------------------------------

test_that("temporary failures are not saved and are requested again next time", {
  local_cache()
  store <- cache_store("fake", "route")
  f <- engine_fetch(list(`2` = failure_reply("network")))

  suppressMessages(cached_requests(engine_items(3), store, f$fn))
  expect_null(cache_get(store, "k2"))

  f2 <- engine_fetch()
  suppressMessages(got <- cached_requests(engine_items(3), store, f2$fn))
  expect_equal(f2$state$calls, "k2")
  expect_equal(unname(got$status), c("cache", "fetched", "cache"))
})

test_that("a permanent failure is remembered for the session, not on disk", {
  local_cache()
  store <- cache_store("fake", "route")
  f <- engine_fetch(list(`2` = failure_reply("not_found", permanent = TRUE, error = "no route")))

  suppressMessages(cached_requests(engine_items(3), store, f$fn))
  f2 <- engine_fetch()
  suppressMessages(again <- cached_requests(engine_items(3), store, f2$fn))
  expect_length(f2$state$calls, 0)
  expect_equal(again$results$k2$error, "no route")
  expect_equal(unname(again$status), rep("cache", 3))

  cache_reset_memory()                       # a new session: only what is on disk is left
  expect_equal(disk_keys(store), c("k1", "k3"))
})

# --- Nothing paid for is lost (Property 16) --------------------------------------------------------------------------------------

test_that("answers are written to disk when a run stops early", {
  local_cache()
  store <- cache_store("fake", "route")
  f <- engine_fetch(list(`4` = failure_reply("quota")))

  suppressMessages(cached_requests(engine_items(6), store, f$fn))
  cache_reset_memory(flush = FALSE)          # discard memory: only the disk is left

  expect_equal(disk_keys(store), c("k1", "k2", "k3"))
})

test_that("answers are written to disk even if the provider's function stops with an error in a way that escapes", {
  local_cache()
  store <- cache_store("fake", "route")
  # `remaining` is called by the engine itself; an error there is handled, but an
  # error in the cache layer would escape. Simulate an interrupt after two items.
  f <- engine_fetch()
  interrupting <- function(item) {
    if (item == 3) stop("interrupted")
    f$fn(item)
  }

  suppressMessages(got <- cached_requests(engine_items(4), store, interrupting))
  cache_reset_memory(flush = FALSE)

  expect_equal(disk_keys(store), c("k1", "k2", "k4"))
  expect_equal(got$status[["k3"]], "failed")
})

# --- Problems inside the provider's function ---------------------------------------------------------------------------------------------

test_that("an error inside the provider's function becomes a temporary failure, with the key removed", {
  local_cache()
  withr::local_envvar(TRUCKR_FAKE_API_KEY = "sk-secret-123456789")
  store <- cache_store("fake", "route")
  boom <- function(item) stop("request to https://x.example/?api_key=sk-secret-123456789 failed")

  suppressMessages(got <- cached_requests(engine_items(1), store, boom, provider_name = "fake"))

  expect_equal(got$status[["k1"]], "failed")
  expect_match(got$results$k1$error, "^provider error: request to")
  expect_false(grepl("sk-secret-123456789", got$results$k1$error, fixed = TRUE))
  expect_match(got$results$k1$error, "[API key removed]", fixed = TRUE)
  expect_false(got$results$k1$error_permanent)
})

test_that("a result that is not a list becomes a temporary failure", {
  local_cache()

  suppressMessages(got <- cached_requests(engine_items(1), cache_store("fake", "route"), function(item) NULL))

  expect_equal(got$results$k1$error, "the provider returned an unusable result")
  expect_equal(got$status[["k1"]], "failed")
})

# --- Messages --------------------------------------------------------------------------------------------------------------------------------

test_that("progress is reported at regular intervals", {
  local_cache()
  withr::local_options(truckr.progress_every = 2)

  msgs <- testthat::capture_messages(
    cached_requests(engine_items(5), cache_store("fake", "route"), engine_fetch()$fn, label = "Routing")
  )

  expect_true(any(grepl("Routing: 2 of 5 requested.", msgs, fixed = TRUE)))
  expect_true(any(grepl("Routing: 4 of 5 requested.", msgs, fixed = TRUE)))
  expect_false(any(grepl("5 of 5 requested", msgs, fixed = TRUE)))
})

test_that("a finished run says how many were requested and how many were saved", {
  local_cache()
  store <- cache_store("fake", "route")
  cache_put(store, "k1", list(value = 1, error = NA_character_))

  msgs <- testthat::capture_messages(
    cached_requests(engine_items(3), store, engine_fetch()$fn, label = "Routing")
  )

  expect_match(msgs[length(msgs)], "Routing: 2 requested, 1 already saved.")
})

test_that("large numbers are written with separators", {
  expect_equal(count_text(2000), "2,000")
  expect_match(preflight_message("Routing", 5000, 0, 5000, 1996), "5,000 requests needed .* only about 1,996 remain")
})

# --- Bad input ------------------------------------------------------------------------------------------------------------------------------------

test_that("items must be named with unique, non-empty keys", {
  local_cache()
  store <- cache_store("fake", "route")
  f <- engine_fetch()$fn

  expect_error(cached_requests(list(1, 2), store, f), "unique, non-empty cache keys")
  expect_error(cached_requests(list(a = 1, a = 2), store, f), "unique, non-empty cache keys")
  expect_error(cached_requests(list(a = 1, 2), store, f), "unique, non-empty cache keys")
  expect_error(cached_requests("a", store, f), "unique, non-empty cache keys")
  expect_error(cached_requests(list(a = 1), store, "not a function"), "`fetch` must be a function")
})
