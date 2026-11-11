# Helpers for the request-engine and routing tests.

# A fake routing provider that records every request it receives.
#
# `remaining` is how many requests it says are left today; it goes down by one
# per request, and a test can set `router$log$remaining` to simulate the quota
# resetting. Leave it NULL for a provider that cannot report its quota.
# `fail` is a list of replies, named by route cache key, to return instead of a
# route.
fake_router <- function(remaining = NULL, fail = list(), reply = NULL) {
  log <- new.env()
  log$calls <- list()
  log$remaining <- remaining

  route_segment <- function(from, to) {
    log$calls[[length(log$calls) + 1]] <- list(from = from, to = to)
    if (!is.null(log$remaining)) log$remaining <- max(0, log$remaining - 1)
    if (!is.null(reply)) return(reply)
    failure <- fail[[route_cache_key(from, to)]]
    if (!is.null(failure)) return(failure)
    meters <- 1000 * sum(abs(from - to))
    list(
      distance_m = meters, duration_s = meters / 20,
      error = NA_character_, error_permanent = FALSE, error_kind = "ok"
    )
  }

  provider <- list(
    geocode = function(location) NULL,
    route_segment = route_segment,
    validate_credentials = function() TRUE,
    profile = "test-profile"
  )
  if (!is.null(remaining)) {
    provider$remaining_quota <- function(kind) log$remaining
  }

  list(provider = list(name = "fake", provider = provider), log = log)
}

# Geocoding results for a table of locations, one made-up point each.
fake_geo <- function(locations) {
  keys <- locations$location_key
  stats::setNames(
    lapply(seq_along(keys), function(i) {
      list(
        lon = -100 + i, lat = 30 + i / 2, flag = NA_character_, doubtful = FALSE,
        error = NA_character_, error_permanent = FALSE, error_kind = "ok"
      )
    }),
    keys
  )
}

# A failed geocoding result.
geo_failure <- function(error = "no match found", permanent = TRUE) {
  list(
    lon = NA_real_, lat = NA_real_, error = error,
    error_permanent = permanent, error_kind = "no_match"
  )
}

# Plain lanes, each with its own origin and destination.
lanes_table <- function(n, waypoint = FALSE) {
  df <- data.frame(
    orig_city = paste0("ORIGIN", seq_len(n)), orig_state = "TX", orig_zip = sprintf("7800%d", seq_len(n)), orig_ctry = "US",
    dest_city = paste0("DEST", seq_len(n)), dest_state = "CA", dest_zip = sprintf("9000%d", seq_len(n)), dest_ctry = "US",
    stringsAsFactors = FALSE
  )
  if (waypoint) df$Use_Waypoint <- TRUE
  df
}

# Run the front half of the pipeline on a table of lanes.
route_inputs <- function(df, waypoint = NULL) {
  mode <- resolve_waypoint_mode(df, waypoint)
  prepped <- prep_lanes(df, mode$use_waypoint)
  built <- build_segments(prepped$lanes, mode$waypoint)
  list(df = df, mode = mode, prepped = prepped, built = built, geo = fake_geo(built$locations))
}

# Items for the engine: item i has the key "k<i>" and the value i.
engine_items <- function(n) {
  stats::setNames(as.list(seq_len(n)), paste0("k", seq_len(n)))
}

# A fetch function for the engine that records which items it was asked for.
# `script` maps an item number (as text) to the reply to give for it.
engine_fetch <- function(script = list()) {
  state <- new.env()
  state$calls <- character(0)
  fn <- function(item) {
    state$calls <- c(state$calls, paste0("k", item))
    reply <- script[[as.character(item)]]
    if (!is.null(reply)) return(reply)
    list(value = item * 10, error = NA_character_, error_permanent = FALSE, error_kind = "ok")
  }
  list(fn = fn, state = state)
}

failure_reply <- function(kind, permanent = FALSE, error = paste("failure:", kind)) {
  list(error = error, error_permanent = permanent, error_kind = kind)
}
