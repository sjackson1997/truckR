# ORS routing ------------------------------------------------------------------
#
# The routing half of the ORS provider: the distance and driving time between
# two points. Geocoding is in ors_geocode.R. All traffic goes through
# ors_request(), which handles the key, retries and failure classification.

ors_default_profile <- "driving-hgv"

#' Route between two points with ORS
#'
#' Asks ORS for the driving route from \code{from} to \code{to} on the
#' \code{driving-hgv} (heavy goods vehicle) profile and returns the distance
#' and time. The route geometry and turn-by-turn instructions are switched off:
#' they are not needed and make the response much larger.
#'
#' This is the provider's \code{route_segment} function. It never stops on a
#' failed request: the failure is described in the result so that only the
#' lanes that need this segment are affected.
#'
#' Limits worth knowing about (observed on the public API): a route whose
#' approximated distance is over 6,000 km is rejected with a permanent error
#' (ORS code 2004), and a coordinate with no road within 350 m is rejected the
#' same way (ORS code 2010).
#'
#' @param from,to Coordinates: a named vector or list with \code{lon} and
#'   \code{lat}, or two unnamed numbers in the order longitude, latitude.
#' @param profile The ORS routing profile. Defaults to \code{"driving-hgv"}.
#'
#' @return A list with \code{distance_m} (meters) and \code{duration_s}
#'   (seconds), both \code{NA} on failure; \code{error} (\code{NA} on success,
#'   otherwise the reason, with any API key removed); \code{error_permanent}
#'   (\code{TRUE} only when retrying can never help, which tells the cache
#'   whether the failure may be remembered); and \code{error_kind}
#'   (\code{"ok"}, or one of the kinds listed for \code{ors_request()}).
#' @noRd
ors_route_segment <- function(from, to, profile = ors_default_profile) {
  from <- lonlat_vector(from, "from")
  to <- lonlat_vector(to, "to")
  if (!is.character(profile) || length(profile) != 1 || is.na(profile) ||
      !grepl("^[A-Za-z0-9_-]+$", profile)) {
    stop("`profile` must be a single name such as \"driving-hgv\".", call. = FALSE)
  }

  res <- ors_request(
    "directions",
    paste0("/v2/directions/", profile),
    body = list(coordinates = list(from, to), geometry = FALSE, instructions = FALSE)
  )
  parse_ors_route(res)
}

#' Read the distance and time out of an ORS directions result
#'
#' @param res The result of \code{ors_request()}.
#' @return See \code{ors_route_segment()}.
#' @noRd
parse_ors_route <- function(res) {
  failure <- function(error, permanent = FALSE, kind = "unparseable") {
    list(
      distance_m = NA_real_, duration_s = NA_real_, error = error,
      error_permanent = permanent, error_kind = kind
    )
  }

  if (!isTRUE(res$ok)) {
    return(failure(res$error, isTRUE(res$error_permanent), res$error_kind))
  }

  routes <- res$body[["routes"]]
  if (!is.list(routes) || length(routes) == 0 || !is.list(routes[[1]])) {
    return(failure("ORS returned no route"))
  }
  summary <- routes[[1]][["summary"]]
  distance <- if (is.list(summary)) summary[["distance"]] else NULL
  duration <- if (is.list(summary)) summary[["duration"]] else NULL

  usable <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 0
  if (!usable(distance) || !usable(duration)) {
    return(failure("ORS returned a route without a usable distance and duration"))
  }

  list(
    distance_m = as.numeric(distance), duration_s = as.numeric(duration),
    error = NA_character_, error_permanent = FALSE, error_kind = "ok"
  )
}
