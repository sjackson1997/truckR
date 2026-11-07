#' Build a normalized key for each location
#'
#' Internal. Two locations get the same key when all of their components
#' (street address, city, state, zip, country) are the same after ignoring
#' letter case and extra spaces. Because every component is part of the key,
#' two different street addresses in one city stay distinct, while the same
#' place written with different capitalization is one location. A location
#' given as coordinates is keyed by its coordinates, rounded to five decimals.
#'
#' @param x A data frame or named list with any of the elements \code{street},
#'   \code{city}, \code{state}, \code{zip}, \code{country}, \code{lat} and
#'   \code{lon}. Missing elements are treated as \code{NA}. Elements must have
#'   one common length (or length 1).
#'
#' @return A character vector of keys, one per location.
#' @noRd
location_key <- function(x) {
  n <- if (is.data.frame(x)) nrow(x) else max(0L, lengths(x))

  text <- function(name) {
    v <- x[[name]]
    if (is.null(v)) v <- NA_character_
    v <- rep_len(stringr::str_squish(as.character(v)), n)
    toupper(ifelse(is.na(v), "", v))
  }
  number <- function(name) {
    v <- x[[name]]
    if (is.null(v)) v <- NA_real_
    rep_len(suppressWarnings(as.numeric(v)), n)
  }

  key <- paste(text("street"), text("city"), text("state"), text("zip"), text("country"), sep = "|")

  lat <- number("lat")
  lon <- number("lon")
  has_coords <- !is.na(lat) & !is.na(lon)
  key[has_coords] <- sprintf("COORD|%.5f|%.5f", lat[has_coords], lon[has_coords])

  key
}


#' Expand lanes into a de-duplicated pool of point-to-point segments
#'
#' Internal. Every lane becomes one or more segments, each of which is a plain
#' "route from location A to location B" request that any provider can answer:
#' \itemize{
#'   \item a lane without a waypoint: one \emph{direct} segment;
#'   \item a lane with a waypoint: the direct segment, plus
#'     origin-to-waypoint (\emph{leg 1}) and waypoint-to-destination
#'     (\emph{leg 2}).
#' }
#' All segments from all lanes are pooled and de-duplicated by the pair of
#' (from location, to location), so a segment such as "Mexico City to Laredo"
#' is listed once however many lanes use it. Direction matters: A to B and B to
#' A are separate segments, because the routes can differ.
#'
#' Lanes that \code{prep_lanes()} flagged in \code{lane_error} get no segments
#' (their segment ids are \code{NA}) and do not contribute locations. That
#' includes lanes whose origin and destination are the same place and which have
#' no waypoint. A same-place lane that does use a waypoint gets its two legs but
#' no direct segment (\code{direct_seg} is \code{NA}).
#'
#' The \code{same_place} column of \code{segments} is \code{TRUE} for a segment
#' whose start and end are the same location, which can only happen for a leg
#' when the waypoint equals the origin or destination. Such a segment has zero
#' length and must not be sent to a provider.
#'
#' @param lanes The \code{lanes} or \code{unique_lanes} table from
#'   \code{prep_lanes()}. Lanes sharing a \code{RouteId} are treated as one.
#' @param waypoint The parsed waypoint from \code{resolve_waypoint_mode()}, or
#'   \code{NULL} when no lane uses one.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{segments}: \code{SegmentId}, \code{from_key}, \code{to_key},
#'     \code{same_place}, one row per distinct segment, numbered in order of
#'     first appearance;
#'   \item \code{locations}: \code{location_key} and the components
#'     \code{street}, \code{city}, \code{state}, \code{zip}, \code{country},
#'     \code{lat}, \code{lon}, one row per distinct location used by a segment;
#'   \item \code{lane_segments}: one row per \code{RouteId} with
#'     \code{direct_seg}, \code{leg1_seg} and \code{leg2_seg} (\code{NA} where
#'     not applicable);
#'   \item \code{waypoint_key}: the key of the waypoint location, or \code{NA}.
#' }
#' @noRd
build_segments <- function(lanes, waypoint = NULL) {
  comps <- c("street", "city", "state", "zip", "country")
  needed <- c("RouteId", "use_waypoint", "same_od", "lane_error",
              paste0("orig_", comps), paste0("dest_", comps))
  missing_cols <- setdiff(needed, names(lanes))
  if (length(missing_cols) > 0) {
    stop(
      "`lanes` is missing column(s): ", paste(missing_cols, collapse = ", "),
      ". Use the output of prep_lanes().",
      call. = FALSE
    )
  }

  lanes <- lanes[!duplicated(lanes$RouteId), , drop = FALSE]
  n <- nrow(lanes)

  ok <- is.na(lanes$lane_error)
  uses_waypoint <- lanes$use_waypoint & ok

  if (any(uses_waypoint) && is.null(waypoint)) {
    stop("Some lanes use a waypoint but no `waypoint` was supplied.", call. = FALSE)
  }

  side <- function(prefix) {
    out <- lapply(comps, function(cm) lanes[[paste0(prefix, cm)]])
    names(out) <- comps
    out$lat <- rep(NA_real_, n)
    out$lon <- rep(NA_real_, n)
    out
  }
  origin <- side("orig_")
  dest <- side("dest_")
  origin_key <- location_key(origin)
  dest_key <- location_key(dest)

  waypoint_key <- NA_character_
  if (any(uses_waypoint)) {
    waypoint_key <- location_key(waypoint)
  }

  pieces <- function(kind, rows, from, to) {
    data.frame(
      lane = rows,
      kind = rep_len(kind, length(rows)),
      from_key = from,
      to_key = to,
      stringsAsFactors = FALSE
    )
  }
  # A lane whose origin and destination are the same place never gets a direct
  # segment (nothing to route). If it uses a waypoint, its two legs are still built.
  direct_rows <- which(ok & !lanes$same_od)
  wp_rows <- which(uses_waypoint)
  candidates <- rbind(
    pieces("direct", direct_rows, origin_key[direct_rows], dest_key[direct_rows]),
    pieces("leg1", wp_rows, origin_key[wp_rows], rep_len(waypoint_key, length(wp_rows))),
    pieces("leg2", wp_rows, rep_len(waypoint_key, length(wp_rows)), dest_key[wp_rows])
  )
  candidates <- candidates[order(candidates$lane, match(candidates$kind, c("direct", "leg1", "leg2"))), ]

  pair <- paste(candidates$from_key, candidates$to_key, sep = "\u001f")
  seg_num <- match(pair, unique(pair))
  n_segments <- length(unique(pair))
  width <- max(4L, nchar(as.character(n_segments)))
  seg_id <- if (n_segments == 0) {
    character(0)
  } else {
    paste0("SEG-", formatC(seg_num, width = width, flag = "0"))
  }

  first <- !duplicated(pair)
  segments <- dplyr::tibble(
    SegmentId = seg_id[first],
    from_key = candidates$from_key[first],
    to_key = candidates$to_key[first],
    same_place = candidates$from_key[first] == candidates$to_key[first]
  )

  lane_segments <- dplyr::tibble(
    RouteId = lanes$RouteId,
    direct_seg = rep(NA_character_, n),
    leg1_seg = rep(NA_character_, n),
    leg2_seg = rep(NA_character_, n)
  )
  for (kind in c("direct", "leg1", "leg2")) {
    pick <- candidates$kind == kind
    lane_segments[[paste0(kind, "_seg")]][candidates$lane[pick]] <- seg_id[pick]
  }

  as_locations <- function(x, key) {
    dplyr::tibble(
      location_key = key,
      street = rep_len(as.character(x$street), length(key)),
      city = rep_len(as.character(x$city), length(key)),
      state = rep_len(as.character(x$state), length(key)),
      zip = rep_len(as.character(x$zip), length(key)),
      country = rep_len(as.character(x$country), length(key)),
      lat = rep_len(as.numeric(x$lat), length(key)),
      lon = rep_len(as.numeric(x$lon), length(key))
    )
  }
  pool <- rbind(
    as_locations(lapply(origin, function(v) v[ok]), origin_key[ok]),
    as_locations(lapply(dest, function(v) v[ok]), dest_key[ok])
  )
  if (any(uses_waypoint)) {
    pool <- rbind(pool, as_locations(waypoint, waypoint_key))
  }
  locations <- pool[!duplicated(pool$location_key), ]

  list(
    segments = segments,
    locations = locations,
    lane_segments = lane_segments,
    waypoint_key = waypoint_key
  )
}
