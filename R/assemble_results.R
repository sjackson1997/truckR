#' Convert meters to miles
#'
#' @param m Distance in meters.
#' @return Distance in miles.
#' @noRd
meters_to_miles <- function(m) {
  m / 1609.344
}


#' Convert seconds to hours
#'
#' @param s Duration in seconds.
#' @return Duration in hours.
#' @noRd
seconds_to_hours <- function(s) {
  s / 3600
}


#' Join the non-missing messages in several parallel vectors
#'
#' @param ... Character vectors of the same length. \code{NA} means "nothing to
#'   report".
#' @return A character vector with the non-missing messages of each position
#'   joined by "; ", or \code{NA} where there were none.
#' @noRd
combine_messages <- function(...) {
  parts <- list(...)
  n <- length(parts[[1]])
  vapply(seq_len(n), function(i) {
    msgs <- vapply(parts, function(p) as.character(p[i]), character(1))
    msgs <- msgs[!is.na(msgs)]
    if (length(msgs) == 0) NA_character_ else paste(msgs, collapse = "; ")
  }, character(1))
}


#' Assemble the final result table from segment results
#'
#' Internal. Takes the distances and durations of the individual segments and
#' builds one result row per input row. All distances are in miles and all
#' durations in hours (providers report meters and seconds; the conversion
#' happens here, once).
#'
#' Columns added after the original columns, always:
#' \itemize{
#'   \item \code{RouteId};
#'   \item \code{distance_direct}, \code{distance_routed},
#'     \code{duration_direct}, \code{duration_routed}: the shortest
#'     origin-to-destination route, and the route as specified (the sum of the
#'     two waypoint legs for a waypoint lane, otherwise the same as direct);
#'   \item \code{has_waypoint};
#'   \item \code{provider_name};
#'   \item \code{route_error}: \code{NA} when there is nothing to report,
#'     otherwise the reason (a failed lookup, a flagged lane, or a failed
#'     sanity check);
#'   \item \code{geocode_flag}: \code{NA} when there is nothing to report,
#'     otherwise a note about a location.
#' }
#' Added only when at least one lane uses a waypoint (and \code{NA} for lanes
#' that do not): \code{waypoint_value}, \code{waypoint_type},
#' \code{detour_added} (miles) and \code{detour_duration_added} (hours).
#'
#' Rules applied to each lane:
#' \itemize{
#'   \item A lane flagged in \code{lane_error} (empty location, or same origin
#'     and destination without a waypoint) has all numbers \code{NA} and that
#'     text in \code{route_error}.
#'   \item A lane without a waypoint takes its numbers from its direct segment.
#'   \item A waypoint lane's routed numbers are the sum of its two legs; if
#'     either leg failed they are \code{NA} and the reason is given. Its direct
#'     numbers come from its direct segment. A waypoint lane whose origin and
#'     destination are the same has \code{NA} direct numbers and detour, and a
#'     note that the direct distance does not apply.
#'   \item A segment that failed, or is missing from \code{seg_results}, has
#'     \code{NA} numbers. A segment marked \code{same_place} is zero length and
#'     needs no result.
#'   \item If a waypoint lane's routed distance or duration is shorter than its
#'     direct one by more than \code{tolerance}, a "check:" note is added to
#'     \code{route_error} (the numbers are kept), because the waypoint may have
#'     been placed in the wrong spot.
#' }
#'
#' @param df The original input data frame. Its columns come first and are not
#'   changed.
#' @param prepped The result of \code{prep_lanes()}.
#' @param built The result of \code{build_segments()}.
#' @param seg_results A data frame with one row per routed segment and columns
#'   \code{SegmentId}, \code{distance_m}, \code{duration_s} and \code{error}
#'   (\code{NA} when the segment succeeded).
#' @param waypoint The parsed waypoint (from \code{resolve_waypoint_mode()}),
#'   or \code{NULL}.
#' @param provider_name The provider's name, recorded in each row.
#' @param location_flags Optional data frame with columns \code{location_key}
#'   and \code{flag}: notes about locations, shown in \code{geocode_flag} for
#'   the lanes that use them.
#' @param tolerance Fraction by which a routed value may be shorter than the
#'   direct value before it is flagged. Default 0.005 (0.5 percent).
#'
#' @return A tibble with one row per row of \code{df}, in the same order.
#' @noRd
assemble_results <- function(df, prepped, built, seg_results, waypoint = NULL,
                             provider_name = NA_character_, location_flags = NULL,
                             tolerance = 0.005) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  lanes <- prepped$lanes
  if (nrow(df) != nrow(lanes)) {
    stop("`df` and `prepped` do not describe the same rows.", call. = FALSE)
  }
  needed <- c("SegmentId", "distance_m", "duration_s", "error")
  if (!is.data.frame(seg_results) || !all(needed %in% names(seg_results))) {
    stop(
      "`seg_results` must be a data frame with columns: ", paste(needed, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!is.null(location_flags) &&
      !(is.data.frame(location_flags) && all(c("location_key", "flag") %in% names(location_flags)))) {
    stop("`location_flags` must be a data frame with columns: location_key, flag.", call. = FALSE)
  }

  any_waypoint <- any(lanes$use_waypoint)
  if (any_waypoint && is.null(waypoint)) {
    stop("Some lanes use a waypoint but no `waypoint` was supplied.", call. = FALSE)
  }

  always_cols <- c(
    "RouteId", "distance_direct", "distance_routed", "duration_direct",
    "duration_routed", "has_waypoint", "provider_name", "route_error", "geocode_flag"
  )
  waypoint_cols <- c("waypoint_value", "waypoint_type", "detour_added", "detour_duration_added")
  clash <- intersect(names(df), c(always_cols, if (any_waypoint) waypoint_cols))
  if (length(clash) > 0) {
    stop(
      "The input already has column(s) that the result adds: ", paste(clash, collapse = ", "),
      ". Rename them first.",
      call. = FALSE
    )
  }

  # --- Per-segment numbers (miles, hours) and errors ---------------------------------
  segs <- built$segments
  at <- match(segs$SegmentId, seg_results$SegmentId)
  seg_dist <- meters_to_miles(as.numeric(seg_results$distance_m[at]))
  seg_dur <- seconds_to_hours(as.numeric(seg_results$duration_s[at]))
  seg_err <- as.character(seg_results$error[at])

  seg_err[is.na(at)] <- "no result was returned for this segment"
  no_numbers <- is.na(seg_err) & (is.na(seg_dist) | is.na(seg_dur))
  seg_err[no_numbers] <- "the provider returned no distance or duration"

  zero_length <- segs$same_place
  seg_dist[zero_length] <- 0
  seg_dur[zero_length] <- 0
  seg_err[zero_length] <- NA_character_

  failed <- !is.na(seg_err)
  seg_dist[failed] <- NA_real_
  seg_dur[failed] <- NA_real_

  # --- Per-lane numbers ----------------------------------------------------------------
  lt <- lanes[!duplicated(lanes$RouteId), ]
  ls <- built$lane_segments[match(lt$RouteId, built$lane_segments$RouteId), ]
  n_lanes <- nrow(lt)

  take <- function(seg_ids, x) x[match(seg_ids, segs$SegmentId)]
  d_dist <- take(ls$direct_seg, seg_dist)
  d_dur <- take(ls$direct_seg, seg_dur)
  d_err <- take(ls$direct_seg, seg_err)
  l1_dist <- take(ls$leg1_seg, seg_dist)
  l1_dur <- take(ls$leg1_seg, seg_dur)
  l1_err <- take(ls$leg1_seg, seg_err)
  l2_dist <- take(ls$leg2_seg, seg_dist)
  l2_dur <- take(ls$leg2_seg, seg_dur)
  l2_err <- take(ls$leg2_seg, seg_err)

  valid <- is.na(lt$lane_error)
  waypoint_lane <- valid & lt$use_waypoint
  plain_lane <- valid & !lt$use_waypoint

  # A lane that needs a direct segment but has none is a bug upstream; say so.
  needs_direct <- valid & !lt$same_od
  d_err[needs_direct & is.na(ls$direct_seg)] <- "no direct segment was built for this lane"

  routed_dist <- rep(NA_real_, n_lanes)
  routed_dur <- rep(NA_real_, n_lanes)
  routed_dist[plain_lane] <- d_dist[plain_lane]
  routed_dur[plain_lane] <- d_dur[plain_lane]
  routed_dist[waypoint_lane] <- l1_dist[waypoint_lane] + l2_dist[waypoint_lane]
  routed_dur[waypoint_lane] <- l1_dur[waypoint_lane] + l2_dur[waypoint_lane]

  direct_dist <- d_dist
  direct_dur <- d_dur
  direct_dist[!valid | lt$same_od] <- NA_real_
  direct_dur[!valid | lt$same_od] <- NA_real_

  detour_dist <- rep(NA_real_, n_lanes)
  detour_dur <- rep(NA_real_, n_lanes)
  detour_dist[waypoint_lane] <- (routed_dist - direct_dist)[waypoint_lane]
  detour_dur[waypoint_lane] <- (routed_dur - direct_dur)[waypoint_lane]

  # Sanity check: going through a waypoint should not be shorter than going direct.
  both <- waypoint_lane & !lt$same_od
  short_dist <- both & !is.na(routed_dist) & !is.na(direct_dist) &
    (routed_dist < direct_dist * (1 - tolerance))
  short_dur <- both & !is.na(routed_dur) & !is.na(direct_dur) &
    (routed_dur < direct_dur * (1 - tolerance))
  check_msg <- rep(NA_character_, n_lanes)
  check_msg[short_dist %in% TRUE | short_dur %in% TRUE] <- paste0(
    "check: the route through the waypoint is shorter than the direct route; ",
    "the waypoint may be in the wrong place"
  )

  # --- Messages ---------------------------------------------------------------------------
  only_if <- function(x, keep) replace(x, !keep, NA_character_)
  label <- function(prefix, x) ifelse(is.na(x), NA_character_, paste0(prefix, x))

  route_error <- combine_messages(
    only_if(lt$lane_error, !valid),
    only_if(d_err, plain_lane),
    only_if(label("origin to waypoint: ", l1_err), waypoint_lane),
    only_if(label("waypoint to destination: ", l2_err), waypoint_lane),
    only_if(label("direct route: ", d_err), waypoint_lane & !lt$same_od),
    only_if(
      rep("same origin and destination; the direct distance does not apply", n_lanes),
      waypoint_lane & lt$same_od
    ),
    check_msg
  )

  location_note <- function(keys) {
    if (is.null(location_flags)) {
      return(rep(NA_character_, length(keys)))
    }
    as.character(location_flags$flag[match(keys, location_flags$location_key)])
  }
  side_keys <- function(prefix) {
    comps <- c("street", "city", "state", "zip", "country")
    x <- lapply(comps, function(cm) lt[[paste0(prefix, cm)]])
    names(x) <- comps
    location_key(x)
  }
  waypoint_key <- rep_len(built$waypoint_key, n_lanes)
  geocode_flag <- combine_messages(
    only_if(label("origin: ", location_note(side_keys("orig_"))), valid),
    only_if(label("destination: ", location_note(side_keys("dest_"))), valid),
    only_if(label("waypoint: ", location_note(waypoint_key)), waypoint_lane)
  )

  # --- Back to one row per input row --------------------------------------------------------
  row_lane <- match(lanes$RouteId, lt$RouteId)
  n <- nrow(df)

  out <- dplyr::as_tibble(df, .name_repair = "minimal")
  out$RouteId <- lanes$RouteId
  out$distance_direct <- direct_dist[row_lane]
  out$distance_routed <- routed_dist[row_lane]
  out$duration_direct <- direct_dur[row_lane]
  out$duration_routed <- routed_dur[row_lane]
  out$has_waypoint <- lanes$use_waypoint
  out$provider_name <- rep_len(as.character(provider_name), n)
  out$route_error <- route_error[row_lane]
  out$geocode_flag <- geocode_flag[row_lane]

  if (any_waypoint) {
    out$waypoint_value <- rep(NA_character_, n)
    out$waypoint_value[lanes$use_waypoint] <- waypoint$value
    out$waypoint_type <- rep(NA_character_, n)
    out$waypoint_type[lanes$use_waypoint] <- waypoint$type
    out$detour_added <- detour_dist[row_lane]
    out$detour_duration_added <- detour_dur[row_lane]
  }

  out
}
