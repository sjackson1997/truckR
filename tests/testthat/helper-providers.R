# Helpers shared by the provider and credential tests.

# A valid provider made of do-nothing functions. Named elements in `...`
# replace or (when NULL) remove the defaults.
fake_provider <- function(...) {
  base <- list(
    geocode = function(location) NULL,
    route_segment = function(from, to) NULL,
    validate_credentials = function() TRUE,
    limits = list(requests_per_minute = 40),
    profile = "driving-hgv"
  )
  utils::modifyList(base, list(...))
}

# Start a test with an empty provider registry and restore the real one after.
local_empty_registry <- function(env = parent.frame()) {
  saved_providers <- .truckr_state$providers
  saved_active <- .truckr_state$active
  .truckr_state$providers <- list()
  .truckr_state$active <- NULL

  withr::defer(
    {
      .truckr_state$providers <- saved_providers
      .truckr_state$active <- saved_active
    },
    envir = env
  )
}
