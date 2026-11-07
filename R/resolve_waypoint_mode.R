#' Decide whether and how a waypoint applies to a set of lanes
#'
#' Internal. Validates the optional \code{Use_Waypoint} column of \code{df}
#' together with the \code{waypoint} argument, so that bad input fails before
#' any API call is made.
#'
#' | \code{Use_Waypoint} column | \code{waypoint} | Result |
#' |---|---|---|
#' | absent | NULL | no waypoints; all lanes direct |
#' | absent | supplied | error |
#' | present, has TRUE | supplied | TRUE lanes use the waypoint |
#' | present, has TRUE | NULL | error |
#' | present, no TRUE | NULL | all lanes direct |
#' | present, no TRUE | supplied | all lanes direct, with a warning |
#' | present, not logical | any | error |
#'
#' \code{NA} in the column is treated as \code{FALSE}.
#'
#' @param df A data frame of lanes.
#' @param waypoint \code{NULL}, or a single waypoint location. See
#'   \code{parse_waypoint()} for the accepted forms.
#'
#' @return A list with \code{mode} (\code{"none"} or \code{"waypoint"}),
#'   \code{use_waypoint} (a logical vector with one element per row of
#'   \code{df}), and \code{waypoint} (\code{NULL}, or the parsed location).
#' @noRd
resolve_waypoint_mode <- function(df, waypoint = NULL) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  col <- "Use_Waypoint"
  nm <- names(df)

  if (!col %in% nm) {
    near <- nm[tolower(nm) == tolower(col)]
    if (length(near) > 0) {
      stop(
        sprintf(
          "Found a column named `%s`; the waypoint flag column must be named exactly `%s`.",
          near[1], col
        ),
        call. = FALSE
      )
    }
  }

  has_col <- col %in% nm
  has_waypoint <- !is.null(waypoint)

  if (has_col) {
    flag <- df[[col]]
    if (!is.logical(flag)) {
      stop(
        sprintf(
          "`%s` must be a logical (TRUE/FALSE) column, but it is of class `%s`.",
          col, class(flag)[1]
        ),
        call. = FALSE
      )
    }
    n_na <- sum(is.na(flag))
    if (n_na > 0) {
      message(sprintf("%d lane(s) have NA in `%s`; treating them as FALSE.", n_na, col))
    }
    use <- !is.na(flag) & flag
  } else {
    use <- rep(FALSE, nrow(df))
  }

  if (!has_col && has_waypoint) {
    stop(
      sprintf(
        "A `waypoint` was supplied but `df` has no `%s` column. Add a logical `%s` column marking which lanes use the waypoint.",
        col, col
      ),
      call. = FALSE
    )
  }

  if (any(use) && !has_waypoint) {
    stop(
      sprintf(
        "`%s` has TRUE values but no `waypoint` was supplied. Supply a `waypoint`, or set `%s` to FALSE.",
        col, col
      ),
      call. = FALSE
    )
  }

  if (!any(use)) {
    if (has_waypoint) {
      warning(
        sprintf(
          "A `waypoint` was supplied but no lane has `%s` = TRUE; all lanes will be routed directly.",
          col
        ),
        call. = FALSE
      )
    }
    return(list(mode = "none", use_waypoint = use, waypoint = NULL))
  }

  list(mode = "waypoint", use_waypoint = use, waypoint = parse_waypoint(waypoint))
}


#' Parse a waypoint specification into components
#'
#' Internal. Accepts one of:
#' \itemize{
#'   \item a named numeric vector or list with \code{lat} and \code{lon}
#'     (coordinates);
#'   \item a single unnamed string, classified automatically: a five-digit or
#'     ZIP+4 code or a Canadian postal code is a \code{zip}; "City, ST" is a
#'     \code{city_state}; anything else is a free-text \code{address};
#'   \item a named list or character vector of components (\code{street},
#'     \code{city}, \code{state}, \code{zip}, \code{country}, \code{lat},
#'     \code{lon}), which is how a country is stated explicitly.
#' }
#'
#' The country is inferred only where unambiguous (US five-digit zips and US
#' states as \code{USA}; Canadian postal codes and provinces other than the
#' ambiguous \code{NL} as \code{CAN}) and then \code{country_inferred} is
#' \code{TRUE}. A bare five-digit zip is assumed to be a US zip.
#'
#' @param waypoint The waypoint specification.
#'
#' @return A list with \code{type} (\code{"coordinates"}, \code{"address"},
#'   \code{"zip"} or \code{"city_state"}), \code{value} (a display string),
#'   \code{street}, \code{city}, \code{state}, \code{zip}, \code{country},
#'   \code{lat}, \code{lon}, and \code{country_inferred}.
#' @noRd
parse_waypoint <- function(waypoint) {
  fail <- function(msg) stop(paste0("Invalid `waypoint`: ", msg), call. = FALSE)

  us_zip <- "^\\d{5}(-\\d{4})?$"
  ca_zip <- "^[A-Za-z]\\d[A-Za-z][ -]?\\d[A-Za-z]\\d$"

  comps <- list(
    street = NA_character_, city = NA_character_, state = NA_character_,
    zip = NA_character_, country = NA_character_,
    lat = NA_real_, lon = NA_real_
  )

  aliases <- c(
    street = "street", address = "street", street_address = "street",
    city = "city", state = "state", province = "state",
    zip = "zip", postal = "zip", postal_code = "zip", zip_code = "zip",
    country = "country", ctry = "country",
    lat = "lat", latitude = "lat",
    lon = "lon", lng = "lon", long = "lon", longitude = "lon"
  )

  is_component_form <- (is.list(waypoint) || is.atomic(waypoint)) &&
    !is.null(names(waypoint)) && any(nzchar(names(waypoint)))

  if (is_component_form) {
    given <- tolower(names(waypoint))
    unknown <- setdiff(given, names(aliases))
    if (length(unknown) > 0) {
      fail(sprintf(
        "unknown component name(s): %s. Allowed: street, city, state, zip, country, lat, lon.",
        paste(unknown, collapse = ", ")
      ))
    }
    for (i in seq_along(waypoint)) {
      key <- aliases[[given[i]]]
      val <- waypoint[[i]]
      if (length(val) != 1 || is.na(val)) {
        fail(sprintf("component `%s` must be a single non-missing value.", given[i]))
      }
      if (key %in% c("lat", "lon")) {
        num <- suppressWarnings(as.numeric(val))
        if (is.na(num)) fail(sprintf("component `%s` must be numeric.", given[i]))
        comps[[key]] <- num
      } else {
        comps[[key]] <- trimws(as.character(val))
      }
    }
  } else if (is.numeric(waypoint)) {
    fail("coordinates must be named, for example c(lat = 27.5, lon = -99.5).")
  } else if (is.character(waypoint)) {
    if (length(waypoint) != 1 || is.na(waypoint) || !nzchar(trimws(waypoint))) {
      fail("a text waypoint must be a single non-empty string.")
    }
    txt <- trimws(waypoint)
    city_state <- "^([^,0-9]+?)\\s*,\\s*([A-Za-z]{2})$"
    if (grepl(us_zip, txt) || grepl(ca_zip, txt)) {
      comps$zip <- txt
    } else if (grepl(city_state, txt)) {
      comps$city <- trimws(sub(city_state, "\\1", txt))
      comps$state <- sub(city_state, "\\2", txt)
    } else {
      comps$street <- txt
    }
  } else {
    fail("expected a string, coordinates c(lat =, lon =), or a named list of components.")
  }

  # Normalize the individual components.
  if (!is.na(comps$zip)) {
    z <- toupper(trimws(comps$zip))
    if (grepl(ca_zip, z)) z <- gsub("[ -]", "", z)
    comps$zip <- fix.ZIP(z)
  }
  if (!is.na(comps$state)) comps$state <- toupper(comps$state)

  # Decide the type.
  has_lat <- !is.na(comps$lat)
  has_lon <- !is.na(comps$lon)
  if (has_lat != has_lon) {
    fail("coordinates need both `lat` and `lon`.")
  }
  if (has_lat) {
    if (!is.finite(comps$lat) || abs(comps$lat) > 90) fail("`lat` must be between -90 and 90.")
    if (!is.finite(comps$lon) || abs(comps$lon) > 180) fail("`lon` must be between -180 and 180.")
    type <- "coordinates"
  } else if (!is.na(comps$street)) {
    type <- "address"
  } else if (!is.na(comps$zip)) {
    type <- "zip"
  } else if (!is.na(comps$city)) {
    type <- "city_state"
  } else {
    fail("it needs coordinates, a street address, a zip/postal code, or a city.")
  }

  # Country: normalize if given, otherwise infer only where unambiguous.
  country_inferred <- FALSE
  if (!is.na(comps$country)) {
    comps$country <- fix.CTRY(toupper(comps$country))
  } else if (type != "coordinates") {
    inferred <- infer_country(comps$zip, comps$state)
    if (!is.na(inferred)) {
      comps$country <- inferred
      country_inferred <- TRUE
    }
  }

  value <- if (type == "coordinates") {
    sprintf("%.5f,%.5f", comps$lat, comps$lon)
  } else {
    parts <- c(comps$street, comps$city, comps$state, comps$zip)
    paste(parts[!is.na(parts)], collapse = ", ")
  }

  c(list(type = type, value = value), comps, list(country_inferred = country_inferred))
}


#' Infer a country only where it is unambiguous
#'
#' @param zip A normalized zip/postal code, or NA.
#' @param state An upper-case state/province abbreviation, or NA.
#'
#' @return \code{"USA"}, \code{"CAN"}, or \code{NA_character_}.
#' @noRd
infer_country <- function(zip, state) {
  us_states <- c(datasets::state.abb, "DC", "PR")
  ca_provinces <- c("AB", "BC", "MB", "NB", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT")
  # "NL" is deliberately left out: it is both Newfoundland and Labrador and the
  # Mexican state of Nuevo Leon.

  if (!is.na(zip)) {
    if (grepl("^\\d{5}(-\\d{4})?$", zip)) return("USA")
    if (grepl("^[A-Z]\\d[A-Z] \\d[A-Z]\\d$", zip)) return("CAN")
  }
  if (!is.na(state)) {
    if (state %in% us_states) return("USA")
    if (state %in% ca_provinces) return("CAN")
  }
  NA_character_
}
