# --- Stores and keys ------------------------------------------------------------------------

test_that("stores are identified by provider, kind and profile", {
  expect_equal(cache_store("ors", "geocode")$id, "ors.geocode")
  expect_equal(cache_store("ors", "route", "driving-hgv")$id, "ors.route.driving-hgv")
  expect_equal(cache_store("ors", "route")$id, "ors.route.default")
  expect_equal(cache_store("ors", "route", "")$id, "ors.route.default")
})

test_that("a profile with odd characters is made safe for a file name", {
  expect_equal(cache_store("ors", "route", "a b/c.d")$id, "ors.route.a_b_c_d")
})

test_that("a bad provider name or kind is rejected", {
  expect_error(cache_store("ORS", "geocode"), "provider name must be")
  expect_error(cache_store("ors", "weather"), "should be one of")
})

test_that("route keys use rounded coordinates, longitude first", {
  expect_equal(route_cache_key(c(-99.1332, 19.4326), c(-118.2437, 34.0522)), "-99.13320,19.43260>-118.24370,34.05220")
  expect_equal(
    route_cache_key(c(lat = 19.4326, lon = -99.1332), c(lat = 34.0522, lon = -118.2437)),
    "-99.13320,19.43260>-118.24370,34.05220"
  )
  expect_equal(
    route_cache_key(c(-99.133200001, 19.4326), c(-118.2437, 34.0522)),
    route_cache_key(c(-99.1332, 19.4326), c(-118.2437, 34.0522))
  )
  expect_false(route_cache_key(c(1, 2), c(3, 4)) == route_cache_key(c(3, 4), c(1, 2)))
})

test_that("bad route key input is rejected", {
  expect_error(route_cache_key(1, c(3, 4)), "`from` must be two numbers")
  expect_error(route_cache_key(c(1, 2), c(NA, 4)), "`to` must be two numbers")
})

test_that("bad cache keys and values are rejected", {
  local_cache()
  s <- cache_store("fake", "geocode")

  for (bad in list("", NA_character_, 1, c("a", "b"))) {
    expect_error(cache_get(s, bad), "single non-empty string")
    expect_error(cache_put(s, bad, ok_result()), "single non-empty string")
  }
  expect_error(cache_put(s, "k", "not a list"), "must be a list")
})

# --- Hits, misses and lookup order ------------------------------------------------------------

test_that("an empty cache misses", {
  local_cache()

  expect_null(cache_get(cache_store("fake", "geocode"), "anything"))
})

test_that("a saved answer is found again with its value", {
  local_cache()
  s <- cache_store("fake", "geocode")

  expect_equal(cache_put(s, "78040", ok_result(7)), "stored")
  hit <- cache_get(s, "78040")

  expect_equal(hit$value$lon, 7)
  expect_false(hit$failure)
  expect_equal(hit$obtained, get("clock", environment(getOption("truckr.cache_clock")))$now)
})

test_that("different providers, kinds and profiles never share answers", {
  local_cache()
  cache_put(cache_store("alpha", "geocode"), "k", ok_result(1))
  cache_put(cache_store("beta", "geocode"), "k", ok_result(2))
  cache_put(cache_store("alpha", "route", "p1"), "k", ok_result(3))
  cache_put(cache_store("alpha", "route", "p2"), "k", ok_result(4))

  expect_equal(cache_get(cache_store("alpha", "geocode"), "k")$value$lon, 1)
  expect_equal(cache_get(cache_store("beta", "geocode"), "k")$value$lon, 2)
  expect_equal(cache_get(cache_store("alpha", "route", "p1"), "k")$value$lon, 3)
  expect_equal(cache_get(cache_store("alpha", "route", "p2"), "k")$value$lon, 4)
  expect_null(cache_get(cache_store("beta", "route", "p1"), "k"))
})

test_that("putting the same key again replaces the answer", {
  local_cache()
  s <- cache_store("fake", "geocode")

  cache_put(s, "k", ok_result(1))
  cache_put(s, "k", ok_result(2))

  expect_equal(cache_get(s, "k")$value$lon, 2)
})

test_that("keys with unusual characters work", {
  local_cache()
  s <- cache_store("fake", "geocode")
  key <- "1 Main St|LAREDO|TX|78040|USA \u00e9 [x]"

  cache_put(s, key, ok_result(5))

  expect_equal(cache_get(s, key)$value$lon, 5)
})

test_that("a new session finds answers on disk, and reads the file only once", {
  local_cache()
  s <- cache_store("fake", "geocode")
  cache_put(s, "k", ok_result(9))
  cache_flush_all()

  cache_reset_memory(flush = FALSE)          # a new R session
  expect_equal(cache_get(s, "k")$value$lon, 9)

  unlink(cache_file(s))                      # the file is gone, memory still serves
  expect_equal(cache_get(s, "k")$value$lon, 9)
})

test_that("new answers are in memory at once but reach the disk later", {
  local_cache()
  s <- cache_store("fake", "geocode")

  cache_put(s, "k", ok_result())

  expect_false(file.exists(cache_file(s)))
  expect_false(is.null(cache_get(s, "k")))

  cache_flush_all()

  expect_true(file.exists(cache_file(s)))
  expect_equal(disk_keys(s), "k")
})

# --- Writing in batches ----------------------------------------------------------------------------

test_that("answers are written to disk in batches", {
  local_cache()
  withr::local_options(truckr.cache_flush_every = 3L)
  s <- cache_store("fake", "geocode")

  for (i in 1:2) cache_put(s, paste0("k", i), ok_result(i))
  expect_false(file.exists(cache_file(s)))

  cache_put(s, "k3", ok_result(3))
  expect_equal(disk_keys(s), c("k1", "k2", "k3"))

  for (i in 4:7) cache_put(s, paste0("k", i), ok_result(i))
  expect_equal(disk_keys(s), paste0("k", 1:6))          # the 7th is still waiting

  cache_flush_all()
  expect_equal(disk_keys(s), paste0("k", 1:7))
})

test_that("a run that stops early keeps the answers it obtained (Property 16)", {
  local_cache()
  s <- cache_store("fake", "geocode")
  run <- function() {
    on.exit(cache_flush_all())
    for (i in 1:5) cache_put(s, paste0("k", i), ok_result(i))
    stop("quota exhausted")
  }

  try(run(), silent = TRUE)
  cache_reset_memory(flush = FALSE)

  for (i in 1:5) expect_equal(cache_get(s, paste0("k", i))$value$lon, i)
})

test_that("the file is replaced safely and no temporary files are left", {
  cc <- local_cache()
  s <- cache_store("fake", "geocode")

  cache_put(s, "a", ok_result(1)); cache_flush_all()
  cache_put(s, "b", ok_result(2)); cache_flush_all()
  cache_put(s, "c", ok_result(3)); cache_flush_all()

  expect_equal(disk_keys(s), c("a", "b", "c"))
  expect_length(list.files(cc$dir, pattern = "\\.tmp$"), 0)
  expect_equal(list.files(cc$dir), "fake.geocode.rds")
})

test_that("answers saved by another R session are kept when this one writes", {
  local_cache()
  s <- cache_store("fake", "geocode")
  cache_put(s, "mine", ok_result(1))

  # Another session writes the same file in the meantime.
  other <- list(theirs = list(value = ok_result(2), obtained = cache_now(), failure = FALSE))
  write_cache_file(s, other)

  cache_flush_all()

  expect_equal(disk_keys(s), c("mine", "theirs"))
})

test_that("the newer answer wins when two sessions saved the same key", {
  cc <- local_cache()
  s <- cache_store("fake", "geocode")
  newer <- list(k = list(value = ok_result("theirs"), obtained = cache_now() + 100, failure = FALSE))
  cache_put(s, "k", ok_result("mine"))
  write_cache_file(s, newer)

  cache_flush_all()

  expect_equal(read_cache_file(s)$k$value$lon, "theirs")
})

test_that("session-only mode writes nothing to disk", {
  cc <- local_cache()
  withr::local_options(truckr.cache = "session")
  s <- cache_store("fake", "geocode")

  cache_put(s, "k", ok_result())
  cache_flush_all()

  expect_equal(list.files(cc$dir), character(0))
  expect_false(is.null(cache_get(s, "k")))

  cache_reset_memory()
  expect_null(cache_get(s, "k"))
})

test_that("an unwritable folder warns once and the answers stay in memory", {
  cc <- local_cache()
  blocker <- file.path(cc$dir, "not_a_folder")
  file.create(blocker)
  withr::local_options(truckr.cache_dir = blocker)
  s <- cache_store("fake", "geocode")

  cache_put(s, "k", ok_result(4))

  expect_warning(cache_flush_all(), "Could not write the cache for provider `fake`")
  expect_no_warning(cache_flush_all())
  expect_equal(cache_get(s, "k")$value$lon, 4)
})

# --- Expiry (Requirement 14.12, Property 15) --------------------------------------------------------------

test_that("answers up to 120 days old are used, older ones are not", {
  cc <- local_cache()
  s <- cache_store("fake", "geocode")
  cache_put(s, "k", ok_result())

  cc$clock$advance(days = 119)
  expect_false(is.null(cache_get(s, "k")))

  cc$clock$advance(days = 1)                       # exactly 120 days
  expect_false(is.null(cache_get(s, "k")))

  cc$clock$advance(seconds = 1)                    # one second over
  expect_null(cache_get(s, "k"))
})

test_that("an expired answer is replaced by a new one", {
  cc <- local_cache()
  s <- cache_store("fake", "geocode")
  cache_put(s, "k", ok_result("old"))
  cache_flush_all()

  cc$clock$advance(days = 121)
  expect_null(cache_get(s, "k"))

  cache_put(s, "k", ok_result("new"))
  cache_flush_all()
  cache_reset_memory(flush = FALSE)

  hit <- cache_get(s, "k")
  expect_equal(hit$value$lon, "new")
  expect_equal(hit$obtained, cc$clock$now)
})

test_that("age counts from when the answer was obtained, not from when it was last used", {
  cc <- local_cache()
  s <- cache_store("fake", "geocode")
  cache_put(s, "k", ok_result())

  for (i in 1:5) {
    cc$clock$advance(days = 25)
    cache_get(s, "k")                              # used every 25 days
  }

  expect_null(cache_get(s, "k"))                   # 125 days after it was obtained
})

test_that("expired answers are not loaded and are dropped from the file at the next write", {
  cc <- local_cache()
  s <- cache_store("fake", "geocode")
  cache_put(s, "old", ok_result(1))
  cache_flush_all()

  cc$clock$advance(days = 121)
  cache_reset_memory(flush = FALSE)
  expect_null(cache_get(s, "old"))
  expect_equal(disk_keys(s), "old")                # still in the file until a write

  cache_put(s, "new", ok_result(2))
  cache_flush_all()

  expect_equal(disk_keys(s), "new")
})

test_that("the maximum age is a setting", {
  cc <- local_cache()
  withr::local_options(truckr.cache_max_age_days = 10)
  s <- cache_store("fake", "geocode")
  cache_put(s, "k", ok_result())

  cc$clock$advance(days = 9)
  expect_false(is.null(cache_get(s, "k")))
  cc$clock$advance(days = 2)
  expect_null(cache_get(s, "k"))
})

test_that("bad settings are rejected with a clear message", {
  local_cache()
  s <- cache_store("fake", "geocode")

  withr::local_options(truckr.cache = "everywhere")
  expect_error(cache_get(s, "k"), "must be \"disk\" or \"session\"")
  withr::local_options(truckr.cache = "disk", truckr.cache_max_age_days = -5)
  expect_error(cache_get(s, "k"), "positive number")
  withr::local_options(truckr.cache_max_age_days = 120, truckr.cache_flush_every = 0)
  expect_error(cache_put(s, "k", ok_result()), "whole number")
})

# --- Failures (Requirement 14.13, Property 14) ----------------------------------------------------------------

test_that("temporary and unmarked failures are never stored", {
  cc <- local_cache()
  s <- cache_store("fake", "geocode")

  expect_equal(cache_put(s, "a", bad_result("timeout")), "not_stored")                # unmarked
  expect_equal(cache_put(s, "b", bad_result("rate limited", permanent = FALSE)), "not_stored")
  expect_equal(cache_put(s, "c", bad_result("quota exhausted", permanent = FALSE)), "not_stored")

  for (key in c("a", "b", "c")) expect_null(cache_get(s, key))
  cache_flush_all()
  expect_false(file.exists(cache_file(s)))
  expect_length(.truckr_cache$dirty[[s$id]], 0)
})

test_that("a permanent failure is remembered in memory only", {
  cc <- local_cache()
  s <- cache_store("fake", "geocode")

  expect_equal(cache_put(s, "nowhere", bad_result("no match found", permanent = TRUE)), "memory_only")
  cache_put(s, "fine", ok_result())

  hit <- cache_get(s, "nowhere")
  expect_true(hit$failure)
  expect_equal(hit$value$error, "no match found")

  cache_flush_all()
  expect_equal(disk_keys(s), "fine")

  cache_reset_memory(flush = FALSE)
  expect_null(cache_get(s, "nowhere"))
})

test_that("a permanent failure alone creates no file", {
  cc <- local_cache()
  s <- cache_store("fake", "geocode")

  cache_put(s, "nowhere", bad_result("no match found", permanent = TRUE))
  cache_flush_all()

  expect_equal(list.files(cc$dir), character(0))
})

test_that("a success replaces a remembered failure for the same key", {
  local_cache()
  s <- cache_store("fake", "geocode")
  cache_put(s, "k", bad_result("no match", permanent = TRUE))

  cache_put(s, "k", ok_result(3))

  hit <- cache_get(s, "k")
  expect_false(hit$failure)
  expect_equal(hit$value$lon, 3)
})

test_that("a result with an empty error string is a success", {
  local_cache()
  s <- cache_store("fake", "geocode")

  expect_equal(cache_put(s, "a", list(lon = 1, error = "")), "stored")
  expect_equal(cache_put(s, "b", list(lon = 1)), "stored")
  expect_equal(cache_put(s, "c", list(lon = 1, error = NULL)), "stored")
})

# --- The key never reaches the cache (Property 12) ------------------------------------------------------------------

test_that("an API key in a result is removed before it is saved", {
  local_cache()
  secret <- "sk-test-SECRET-1234567890abcdef"
  withr::local_envvar(TRUCKR_FAKE_API_KEY = secret)
  s <- cache_store("fake", "geocode")

  cache_put(s, "k", list(
    lon = 1, error = NA_character_,
    label = paste0("https://x.example/?api_key=", secret),
    nested = list(note = paste("key", secret))
  ))
  cache_put(s, "perm", bad_result(paste("failed for", secret), permanent = TRUE))
  cache_flush_all()

  on_disk <- paste(unlist(read_cache_file(s)), collapse = " ")
  in_memory <- paste(unlist(cache_get(s, "k")), unlist(cache_get(s, "perm")), collapse = " ")

  expect_false(grepl(secret, on_disk, fixed = TRUE))
  expect_false(grepl(secret, in_memory, fixed = TRUE))
  expect_match(on_disk, "[API key removed]", fixed = TRUE)
})

# --- Damaged files -------------------------------------------------------------------------------------------------------

test_that("a damaged cache file is ignored with a message and rebuilt", {
  cc <- local_cache()
  s <- cache_store("fake", "geocode")
  writeLines("this is not an rds file", cache_file(s))

  expect_message(hit <- cache_get(s, "k"), "could not be used")
  expect_null(hit)

  cache_put(s, "k", ok_result(6))
  cache_flush_all()
  cache_reset_memory(flush = FALSE)

  expect_equal(cache_get(s, "k")$value$lon, 6)
})

test_that("a file from another version is ignored with a message", {
  local_cache()
  s <- cache_store("fake", "geocode")
  saveRDS(list(version = 99L, entries = list()), cache_file(s))

  expect_message(cache_get(s, "k"), "not written by this version")
})

test_that("a file whose contents have the wrong shape is ignored", {
  local_cache()
  s <- cache_store("fake", "geocode")
  saveRDS(list(version = 1L, entries = 5), cache_file(s))

  expect_message(cache_get(s, "k"), "not in the expected form")
})

test_that("malformed entries inside a good file are skipped quietly", {
  local_cache()
  s <- cache_store("fake", "geocode")
  good <- list(value = ok_result(1), obtained = cache_now(), failure = FALSE)
  saveRDS(list(version = 1L, entries = list(good = good, bad1 = "text", bad2 = list(value = 1))), cache_file(s))

  expect_no_message(hit <- cache_get(s, "good"))
  expect_false(is.null(hit))
  expect_null(cache_get(s, "bad1"))
  expect_null(cache_get(s, "bad2"))
})

# --- Looking at the cache and clearing it ----------------------------------------------------------------------------------------

test_that("info on an empty cache is an empty table with the right columns", {
  cc <- local_cache()

  expect_message(info <- truckr_cache_info(), "Cache folder:")

  expect_equal(nrow(info), 0)
  expect_equal(
    names(info),
    c("provider", "kind", "profile", "entries", "expired", "unsaved", "oldest_days", "size_bytes")
  )
  expect_equal(attr(info, "cache_dir"), cc$dir)
})

test_that("info reports entries, expired answers, waiting answers and age", {
  cc <- local_cache()
  g <- cache_store("alpha", "geocode")
  r <- cache_store("alpha", "route", "driving-hgv")
  cache_put(g, "old", ok_result()); cache_flush_all()
  cc$clock$advance(days = 130)
  cache_put(g, "new", ok_result()); cache_flush_all()
  cache_put(r, "r1", ok_result())
  cache_put(g, "waiting", ok_result())

  info <- suppressMessages(truckr_cache_info())
  geo <- info[info$kind == "geocode", ]
  rt <- info[info$kind == "route", ]

  expect_equal(nrow(info), 2)
  expect_equal(geo$entries, 1L)                    # "old" was dropped when "new" was written
  expect_equal(geo$expired, 0L)
  expect_equal(geo$unsaved, 1L)
  expect_equal(geo$oldest_days, 0)
  expect_gt(geo$size_bytes, 0)
  expect_equal(rt$entries, 0L)                     # only in memory so far
  expect_equal(rt$unsaved, 1L)
  expect_equal(rt$profile, "driving-hgv")
  expect_true(is.na(rt$size_bytes))
})

test_that("info counts expired answers that are still in the file", {
  cc <- local_cache()
  s <- cache_store("alpha", "geocode")
  cache_put(s, "a", ok_result()); cache_put(s, "b", ok_result()); cache_flush_all()

  cc$clock$advance(days = 121)
  info <- suppressMessages(truckr_cache_info())

  expect_equal(info$entries, 2L)
  expect_equal(info$expired, 2L)
  expect_equal(info$oldest_days, 121)
})

test_that("info shows an unreadable file as unknown instead of failing", {
  cc <- local_cache()
  writeLines("junk", file.path(cc$dir, "alpha.geocode.rds"))

  info <- suppressMessages(truckr_cache_info())

  expect_equal(nrow(info), 1)
  expect_true(is.na(info$entries))
})

test_that("clearing everything removes memory and the cache files only", {
  cc <- local_cache()
  s <- cache_store("alpha", "geocode")
  cache_put(s, "a", ok_result()); cache_put(s, "b", ok_result()); cache_flush_all()
  writeLines("keep me", file.path(cc$dir, "notes.txt"))
  saveRDS(1, file.path(cc$dir, "other.rds"))

  expect_message(out <- truckr_cache_clear(), "Cleared 2 answer\\(s\\) from memory and 1 cache file")

  expect_equal(out, c(memory_entries = 2L, disk_files = 1L))
  expect_null(cache_get(s, "a"))
  expect_setequal(list.files(cc$dir), c("notes.txt", "other.rds"))
})

test_that("clearing one provider leaves the others", {
  cc <- local_cache()
  a <- cache_store("alpha", "geocode")
  b <- cache_store("beta", "geocode")
  cache_put(a, "k", ok_result(1)); cache_put(b, "k", ok_result(2)); cache_flush_all()

  suppressMessages(truckr_cache_clear("alpha"))

  expect_null(cache_get(a, "k"))
  expect_equal(cache_get(b, "k")$value$lon, 2)
  expect_equal(list.files(cc$dir), "beta.geocode.rds")
})

test_that("a provider whose name starts the same is not cleared by mistake", {
  cc <- local_cache()
  a <- cache_store("ors", "geocode")
  b <- cache_store("ors_two", "geocode")
  cache_put(a, "k", ok_result()); cache_put(b, "k", ok_result()); cache_flush_all()

  suppressMessages(truckr_cache_clear("ors"))

  expect_equal(list.files(cc$dir), "ors_two.geocode.rds")
})

test_that("clearing memory saves waiting answers first, so nothing is lost", {
  local_cache()
  s <- cache_store("alpha", "geocode")
  cache_put(s, "k", ok_result(8))

  suppressMessages(truckr_cache_clear(what = "memory"))

  expect_equal(disk_keys(s), "k")
  expect_equal(cache_get(s, "k")$value$lon, 8)    # read back from disk
})

test_that("clearing the disk leaves what this session holds", {
  cc <- local_cache()
  s <- cache_store("alpha", "geocode")
  cache_put(s, "k", ok_result(8)); cache_flush_all()

  suppressMessages(truckr_cache_clear(what = "disk"))

  expect_equal(list.files(cc$dir), character(0))
  expect_equal(cache_get(s, "k")$value$lon, 8)
})

test_that("clearing when there is nothing to clear is harmless", {
  cc <- local_cache()
  unlink(cc$dir, recursive = TRUE)

  expect_message(out <- truckr_cache_clear(), "Cleared 0 answer")
  expect_equal(out, c(memory_entries = 0L, disk_files = 0L))
})

test_that("clearing validates its arguments", {
  local_cache()

  expect_error(truckr_cache_clear("ORS"), "provider name must be")
  expect_error(truckr_cache_clear(what = "everything"), "should be one of")
})
