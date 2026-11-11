.onLoad <- function(libname, pkgname) {
  register_ors_provider()
  invisible()
}

#' Register OpenRouteService as a provider and make it the active one
#'
#' The only provider in this version, so it is the active one from the start.
#' Registering it again (for example after reloading the package) is harmless.
#' @noRd
register_ors_provider <- function() {
  register_provider("ors", ors_provider(), overwrite = TRUE)
  if (is.null(.truckr_state$active)) {
    truckr_set_provider("ors")
  }
  invisible("ors")
}

#' The ORS provider
#' @noRd
ors_provider <- function() {
  list(
    geocode = ors_geocode,
    route_segment = function(from, to) ors_route_segment(from, to),
    validate_credentials = function() {
      get_api_key("ors")
      TRUE
    },
    remaining_quota = ors_remaining_quota,
    limits = list(requests_per_minute = ors_default_requests_per_minute),
    profile = ors_default_profile
  )
}
