#' Prepare lanes: find location columns, normalize them, assign lane identity
#'
#' Internal. Finds the origin and destination location columns of \code{df}
#' (using \code{get_location_columns1()}), normalizes them, and assigns a
#' lane-level \code{RouteId}. Every original row is kept, in its original
#' order, so results can be joined back to each input row.
#'
#' Five components are recognized for each of the origin and destination:
#' street address, city, state, postal code, and country. At least one of
#' street address, postal code or city must be found for each side; state and
#' country are optional. Postal codes are normalized with \code{fix.ZIP()} and
#' countries with \code{fix.CTRY()} (to \code{USA}, \code{MEX}, \code{CAN}).
#' Blank strings become \code{NA}.
#'
#' When several columns match one component (for example \code{orig_state} and
#' \code{orig_statename}), the column with the shortest name is used and a
#' message lists the others.
#'
#' Two rows share a \code{RouteId} when their normalized origin, destination
#' and waypoint usage are the same (letter case is ignored). Lanes that differ
#' only in waypoint usage get different \code{RouteId}s. IDs are numbered in
#' order of first appearance.
#'
#' A row whose origin or destination has no street address, postal code or
#' city is not an error here: it is kept and flagged in \code{lane_error} so
#' that only that lane is affected downstream.
#'
#' A row whose origin and destination are the same location (identical after
#' normalization) has \code{same_od} set to \code{TRUE}. Without a waypoint it
#' is flagged in \code{lane_error} as "same origin and destination" and is never
#' sent to a provider. With a waypoint it is not an error: the lane is routed
#' out to the waypoint and back. Only locations that normalize to the identical
#' key are detected; the same place written two different ways (for example a
#' zip in one column and a city in the other) is not.
#'
#' @param df A data frame of lanes.
#' @param use_waypoint A logical vector with one non-missing element per row of
#'   \code{df} saying which lanes use a waypoint (as returned by
#'   \code{resolve_waypoint_mode()}). Defaults to all \code{FALSE}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{lanes}: one row per row of \code{df}, with \code{row_id},
#'     \code{RouteId}, \code{lane_error}, \code{use_waypoint}, \code{same_od}, and the
#'     normalized components \code{orig_street}, \code{orig_city},
#'     \code{orig_state}, \code{orig_zip}, \code{orig_country} and the matching
#'     \code{dest_*} columns;
#'   \item \code{unique_lanes}: one row per \code{RouteId}, with the same
#'     columns except \code{row_id}, plus \code{n} (the number of input rows);
#'   \item \code{Origins}, \code{Destinations}: the original column name used
#'     for each component that was found.
#' }
#' @noRd
prep_lanes <- function(df, use_waypoint = NULL) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  n <- nrow(df)

  if (is.null(use_waypoint)) use_waypoint <- rep(FALSE, n)
  if (!is.logical(use_waypoint) || length(use_waypoint) != n || anyNA(use_waypoint)) {
    stop(
      "`use_waypoint` must be a logical vector with one non-missing value per row of `df`.",
      call. = FALSE
    )
  }

  found <- get_location_columns1(df)
  origin_cols <- pick_location_columns(found$Origins, "origin", names(df))
  dest_cols <- pick_location_columns(found$Destinations, "destination", names(df))

  for (side in list(list(origin_cols, "origin"), list(dest_cols, "destination"))) {
    if (is.na(side[[1]][["country"]])) {
      message(sprintf(
        "No country column found for the %s; locations will not be restricted to a country.",
        side[[2]]
      ))
    }
  }

  o <- read_location(df, origin_cols)
  d <- read_location(df, dest_cols)
  o_key <- location_key(o)
  d_key <- location_key(d)
  names(o) <- paste0("orig_", names(o))
  names(d) <- paste0("dest_", names(d))

  empty_side <- function(x, prefix) {
    is.na(x[[paste0(prefix, "street")]]) &
      is.na(x[[paste0(prefix, "city")]]) &
      is.na(x[[paste0(prefix, "zip")]])
  }
  o_empty <- empty_side(o, "orig_")
  d_empty <- empty_side(d, "dest_")
  lane_error <- rep(NA_character_, n)
  lane_error[o_empty & !d_empty] <- "origin has no street address, postal code or city"
  lane_error[!o_empty & d_empty] <- "destination has no street address, postal code or city"
  lane_error[o_empty & d_empty] <- "origin and destination have no street address, postal code or city"

  # A route from a place to itself is not requested. Without a waypoint the lane
  # is flagged; with a waypoint it is still routed out and back (see build_segments()).
  same_od <- !o_empty & !d_empty & o_key == d_key
  lane_error[same_od & !use_waypoint] <- "same origin and destination"

  key <- function(x) toupper(ifelse(is.na(x), "", x))
  lane_key <- do.call(
    paste,
    c(lapply(c(o, d), key), list(as.integer(use_waypoint)), sep = "|")
  )
  id <- match(lane_key, unique(lane_key))
  n_ids <- length(unique(lane_key))
  width <- max(4L, nchar(as.character(n_ids)))
  route_id <- if (n == 0) {
    character(0)
  } else {
    paste0("RTE-", formatC(id, width = width, flag = "0"))
  }

  lanes <- dplyr::tibble(
    row_id = seq_len(n),
    RouteId = route_id,
    lane_error = lane_error,
    use_waypoint = use_waypoint,
    same_od = same_od,
    !!!o,
    !!!d
  )

  unique_lanes <- lanes[!duplicated(lanes$RouteId), ]
  unique_lanes$n <- tabulate(id, nbins = n_ids)
  unique_lanes$row_id <- NULL

  list(
    lanes = lanes,
    unique_lanes = unique_lanes,
    Origins = as.list(origin_cols[!is.na(origin_cols)]),
    Destinations = as.list(dest_cols[!is.na(dest_cols)])
  )
}


#' Choose one column per location component
#'
#' @param matched The named list of matched column names for one side, from
#'   \code{get_location_columns1()}.
#' @param side \code{"origin"} or \code{"destination"}, used in messages.
#' @param all_names All column names of the data frame, used in the error.
#'
#' @return A named character vector with elements \code{street}, \code{city},
#'   \code{state}, \code{zip}, \code{country}; each is a column name or
#'   \code{NA}.
#' @noRd
pick_location_columns <- function(matched, side, all_names) {
  components <- c(
    street = "StreetAddress", city = "City", state = "State",
    zip = "Zip", country = "Country"
  )

  chosen <- vapply(names(components), function(component) {
    cols <- matched[[components[[component]]]]
    if (length(cols) == 0) {
      return(NA_character_)
    }
    pick <- cols[order(nchar(cols), seq_along(cols))][1]
    if (length(cols) > 1) {
      message(sprintf(
        "Several %s columns matched %s (%s); using `%s`.",
        side, component, paste0("`", cols, "`", collapse = ", "), pick
      ))
    }
    pick
  }, character(1))

  if (all(is.na(chosen[c("street", "city", "zip")]))) {
    stop(
      sprintf(
        paste0(
          "Could not find any column identifying the %s. Need at least one of: ",
          "street address, postal code, city. ",
          "Columns are matched by name: the name must contain \"%s\" and a component ",
          "word such as \"city\", \"zip\" or \"postal\", or \"address\". ",
          "Columns in the data: %s."
        ),
        side,
        if (side == "origin") "orig" else "dest",
        paste(utils::head(all_names, 20), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  chosen
}


#' Read and normalize the location components of one side
#'
#' @param df The data frame.
#' @param cols The named character vector from \code{pick_location_columns()}.
#'
#' @return A named list of character vectors with elements \code{street},
#'   \code{city}, \code{state}, \code{zip}, \code{country}.
#' @noRd
read_location <- function(df, cols) {
  get_text <- function(col) {
    if (is.na(col)) {
      return(rep(NA_character_, nrow(df)))
    }
    x <- stringr::str_squish(as.character(df[[col]]))
    x[!nzchar(x)] <- NA_character_
    x
  }

  out <- lapply(cols, get_text)
  names(out) <- names(cols)

  out$state <- toupper(out$state)

  has_zip <- !is.na(out$zip)
  out$zip[has_zip] <- fix.ZIP(toupper(out$zip[has_zip]))

  has_country <- !is.na(out$country)
  out$country[has_country] <- fix.CTRY(out$country[has_country])

  out
}
