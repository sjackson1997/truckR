#' Driving distance and time for a table of lanes
#'
#' Looks up the driving distance and time of every lane in \code{df}, optionally
#' through one waypoint, using the active routing provider (see
#' \code{\link{truckr_set_provider}}). The work is organised so that nothing is
#' looked up twice: each distinct location is geocoded once and each distinct
#' point-to-point route is requested once, however many lanes use it, and
#' answers are saved on disk and reused by later runs (see
#' \code{\link{truckr_cache_info}}). If the provider's daily quota runs out
#' part way, the run stops cleanly, keeps what it has, and says which lanes are
#' missing; run it again after the quota resets to finish them.
#'
#' @section Input:
#' \code{df} needs the lane columns used elsewhere in the package
#' (\code{orig_city}, \code{orig_state}, \code{orig_zip}, \code{orig_ctry},
#' \code{dest_city}, \code{dest_state}, \code{dest_zip}, \code{dest_ctry}; street
#' address columns are used when present). It may have a logical column
#' \code{Use_Waypoint} marking the lanes that should go through the waypoint.
#' A waypoint is given once, in \code{waypoint}: a zip or postal code
#' (\code{"78040"}), a city and state (\code{"Laredo, TX"}), a street address,
#' coordinates (\code{c(lat = 27.5, lon = -99.5)}), or a named list of
#' components. Only one waypoint is supported.
#'
#' @section Result:
#' A tibble with one row for each row of \code{df}, in the same order, with
#' \code{df}'s own columns first and these added. Distances are in miles and
#' durations in hours.
#' \itemize{
#'   \item \code{RouteId}: the same for lanes that are the same route;
#'   \item \code{distance_direct}, \code{duration_direct}: the shortest route
#'     from origin to destination;
#'   \item \code{distance_routed}, \code{duration_routed}: the route as
#'     requested (through the waypoint for a waypoint lane, otherwise the same
#'     as direct);
#'   \item \code{has_waypoint};
#'   \item \code{provider_name};
#'   \item \code{route_error}: \code{NA}, or why a lane has no numbers (or a
#'     warning about them);
#'   \item \code{geocode_flag}: \code{NA}, or a note about how a location was
#'     found;
#'   \item \code{geocode_check}: \code{TRUE} when a location may be the wrong
#'     place (look at \code{geocode_flag}), and the distance should be checked.
#' }
#' When at least one lane uses a waypoint, these are added as well (\code{NA}
#' for lanes that do not): \code{waypoint_value}, \code{waypoint_type},
#' \code{detour_added} (miles) and \code{detour_duration_added} (hours).
#'
#' A lane that could not be routed has \code{NA} numbers and the reason in
#' \code{route_error}; it does not stop the other lanes.
#'
#' @param df A data frame of lanes.
#' @param waypoint \code{NULL}, or the single waypoint used by the lanes with
#'   \code{Use_Waypoint = TRUE}.
#' @param provider The name of a registered provider, or \code{NULL} (the
#'   default) for the active provider.
#'
#' @return A tibble, as described above.
#' @seealso \code{\link{truckr_set_provider}}, \code{\link{truckr_has_key}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Needs an API key; see truckr_has_key("ors").
#' route(df_route_samples[1:5, ])
#'
#' lanes$Use_Waypoint <- TRUE
#' route(lanes, waypoint = "Laredo, TX")
#' }
route <- function(df, waypoint = NULL, provider = NULL) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  prov <- resolve_provider(provider)
  p <- prov$provider

  # Everything is checked before the first request is made.
  mode <- resolve_waypoint_mode(df, waypoint)
  prepped <- prep_lanes(df, mode$use_waypoint)
  p$validate_credentials()

  # Whatever happens, write the answers already obtained.
  on.exit(cache_flush_all(), add = TRUE)

  built <- build_segments(prepped$lanes, mode$waypoint)
  geo <- geocode_locations(built$locations, prov)
  routed <- route_segments(built$segments, geo, prov)

  out <- assemble_results(
    df, prepped, built, routed$seg_results, mode$waypoint,
    provider_name = prov$name, location_flags = geocode_flags(geo)
  )
  finish_route_message(out)
  out
}


#' Find the coordinates of every location
#'
#' Locations given as coordinates are used as they are. The others are
#' geocoded by the provider, one request for each distinct location, with saved
#' answers used first (see \code{cached_requests()}).
#'
#' @param locations The \code{locations} table from \code{build_segments()}.
#' @param provider A provider as returned by \code{resolve_provider()}.
#'
#' @return A named list (named by \code{location_key}) of geocoding results,
#'   each with \code{lon}, \code{lat}, \code{flag}, \code{error} and so on.
#' @noRd
geocode_locations <- function(locations, provider) {
  p <- provider$provider
  keys <- locations$location_key
  geo <- stats::setNames(vector("list", length(keys)), keys)
  if (length(keys) == 0) {
    return(geo)
  }

  as_item <- function(i) {
    item <- as.list(locations[i, setdiff(names(locations), "location_key"), drop = FALSE])
    lapply(item, function(v) if (length(v) == 0) NA else v[[1]])
  }

  given <- !is.na(locations$lat) & !is.na(locations$lon)
  for (i in which(given)) {
    geo[[i]] <- list(
      lon = locations$lon[i], lat = locations$lat[i], flag = NA_character_,
      doubtful = FALSE, error = NA_character_, error_permanent = FALSE, error_kind = "ok"
    )
  }

  to_look_up <- which(!given)
  if (length(to_look_up) > 0) {
    items <- stats::setNames(lapply(to_look_up, as_item), keys[to_look_up])
    remaining <- NULL
    if (is.function(p[["remaining_quota"]])) {
      remaining <- function() p[["remaining_quota"]]("geocode")
    }
    engine <- cached_requests(
      items, cache_store(provider$name, "geocode"), function(item) p$geocode(item),
      remaining = remaining, cost = 1, label = "Geocoding", provider_name = provider$name
    )
    for (key in names(items)) geo[[key]] <- engine$results[[key]]
  }
  geo
}

#' The notes about locations, in the form assemble_results() takes
#' @noRd
geocode_flags <- function(geo) {
  keys <- names(geo)
  flags <- vapply(geo, function(g) {
    f <- if (is.list(g)) g[["flag"]] else NULL
    if (is.null(f) || length(f) != 1 || is.na(f)) NA_character_ else as.character(f)
  }, character(1))
  data.frame(location_key = keys %||% character(0), flag = unname(flags), stringsAsFactors = FALSE)
}


#' Say how the run went
#' @noRd
finish_route_message <- function(out) {
  n <- nrow(out)
  if (n == 0) {
    return(invisible(NULL))
  }
  failed <- sum(is.na(out$distance_routed))
  checks <- sum(out$geocode_check)
  parts <- sprintf("%s of %s lanes have a distance", count_text(n - failed), count_text(n))
  if (failed > 0) {
    parts <- paste0(parts, sprintf("; %s do not (see `route_error`)", count_text(failed)))
  }
  if (checks > 0) {
    parts <- paste0(parts, sprintf(
      "; %s %s a location that may be the wrong place (`geocode_check` is TRUE; see `geocode_flag`)",
      count_text(checks), if (checks == 1) "has" else "have"
    ))
  }
  message("Done: ", parts, ".")
  invisible(NULL)
}
