# ORS geocoding ---------------------------------------------------------------------
#
# The geocoding half of the ORS provider. The strategy belongs to the provider
# (Requirement 14.8) and rests on what live tests showed:
#
#   * US zips and Canadian postal codes match exactly on the postal-code layer;
#     Mexican postal codes match nothing at all.
#   * A street address is matched exactly on the address layer, but a missing
#     address quietly comes back as the city, labelled "fallback".
#   * A search for a city with a state ABBREVIATION is unreliable in Mexico
#     ("Zapopan, JAL" returned places called Jal); with the full state name it
#     works. A city name that is shared by many places ("Guadalupe") may need a
#     search on the name alone, checked against the state afterwards.
#   * `confidence` is not a quality signal ("exact" with confidence 1 came back
#     for the wrong state, and for a street in place of a zip). Results are
#     judged instead by layer, match type, name, country and state.
#
# A wrong place is worse than no place, so a candidate is rejected unless it is
# an exact match at the right level with the right name and, where both can be
# recognized, the right state.

ors_city_layers <- c("locality", "localadmin", "borough", "neighbourhood")
ors_supported_countries <- c("USA", "MEX", "CAN")
ors_country_iso2 <- c(USA = "US", MEX = "MX", CAN = "CA")

#' Geocode one location with ORS
#'
#' This is the provider's \code{geocode} function. It tries the most precise
#' lookup the location allows and falls back to coarser ones, in this order:
#' street address, postal code, city (with the state). Mexican postal codes are
#' never used, because ORS cannot look them up. A location given as
#' coordinates is returned as it is, with no request.
#'
#' A failure that could be temporary (network, rate limit, quota) stops at once
#' and is returned as it is: it is never "fixed" by quietly falling back to a
#' coarser lookup, which would hide the problem and could store a less precise
#' answer.
#'
#' @param location A list (or one-row data frame) with any of \code{street},
#'   \code{city}, \code{state}, \code{zip}, \code{country} (\code{USA},
#'   \code{MEX} or \code{CAN}; \code{NA} if unknown), \code{lat} and
#'   \code{lon}. Missing elements are treated as unknown.
#'
#' @return A list with \code{lon}, \code{lat}, \code{match_layer},
#'   \code{label} (what ORS matched, for review), \code{flag} (\code{NA}, or a
#'   note when the result is less precise or less certain than usual),
#'   \code{strategy}, \code{error} (\code{NA} on success),
#'   \code{error_permanent} and \code{error_kind}.
#' @noRd
ors_geocode <- function(location) {
  loc <- ors_location(location)

  if (!is.na(loc$lat) || !is.na(loc$lon)) {
    point <- lonlat_vector(c(lon = loc$lon, lat = loc$lat), "location")
    return(geo_ok(
      list(lon = point[1], lat = point[2], layer = NA_character_, label = NA_character_),
      "coordinates", character()
    ))
  }

  if (!is.na(loc$country) && !loc$country %in% ors_supported_countries) {
    return(geo_fail(
      sprintf(
        "country %s is not supported by this provider yet (supported: %s)",
        loc$country, paste(ors_supported_countries, collapse = ", ")
      ),
      permanent = TRUE, kind = "unsupported"
    ))
  }

  plan <- ors_geocode_plan(loc)
  if (!is.null(plan$failure)) {
    return(plan$failure)
  }

  flags <- plan$notes
  missed <- character()
  rejected <- character()
  for (step in plan$steps) {
    out <- ors_run_step(step)
    if (out$outcome == "error") {
      return(out$result)
    }
    if (out$outcome == "match") {
      earlier <- setdiff(unique(missed), step$group)
      if (length(earlier) > 0) {
        flags <- c(flags, sprintf("%s not matched; located by %s instead", paste(earlier, collapse = " and "), step$group))
      }
      flags <- c(flags, step$note, out$flags)
      return(geo_ok(out$feature, step$strategy, flags))
    }
    missed <- c(missed, step$group)
    rejected <- c(rejected, out$notes)
  }

  # The most useful reasons first: why a place was ruled out by its state, or
  # was ambiguous, says more than that some other place had a different name.
  rejected <- unique(rejected)
  rejected <- rejected[order(!grepl(" is in |different places|could be confirmed", rejected))]
  detail <- unique(c(plan$notes, rejected))
  geo_fail(
    paste0(
      "no match found for ", describe_location(loc),
      if (length(detail) > 0) paste0(" (", paste(utils::head(detail, 3), collapse = "; "), ")") else ""
    ),
    permanent = TRUE, kind = "no_match"
  )
}


# Results ------------------------------------------------------------------------------

geo_ok <- function(feature, strategy, flags) {
  flags <- unique(flags[!is.na(flags) & nzchar(flags)])
  list(
    lon = feature$lon, lat = feature$lat,
    match_layer = feature$layer, label = feature$label,
    flag = if (length(flags) > 0) paste(flags, collapse = "; ") else NA_character_,
    strategy = strategy,
    error = NA_character_, error_permanent = FALSE, error_kind = "ok"
  )
}

geo_fail <- function(error, permanent, kind) {
  list(
    lon = NA_real_, lat = NA_real_, match_layer = NA_character_, label = NA_character_,
    flag = NA_character_, strategy = NA_character_,
    error = error, error_permanent = permanent, error_kind = kind
  )
}

describe_location <- function(loc) {
  parts <- c(loc$street, loc$city, loc$state, loc$zip, loc$country)
  parts <- parts[!is.na(parts)]
  if (length(parts) == 0) "an empty location" else paste(parts, collapse = ", ")
}


# The location --------------------------------------------------------------------------

#' Tidy up the pieces of a location
#' @noRd
ors_location <- function(location) {
  if (is.data.frame(location)) {
    if (nrow(location) != 1) {
      stop("`location` must be one location (one row).", call. = FALSE)
    }
    location <- as.list(location)
  }
  if (!is.list(location)) {
    stop("`location` must be a list with street, city, state, zip and country.", call. = FALSE)
  }

  text <- function(name) {
    value <- location[[name]]
    if (is.null(value) || length(value) != 1 || is.na(value)) return(NA_character_)
    value <- stringr::str_squish(as.character(value))
    if (nzchar(value)) value else NA_character_
  }
  number <- function(name) {
    value <- location[[name]]
    if (is.null(value) || length(value) != 1) return(NA_real_)
    suppressWarnings(as.numeric(value))
  }

  country <- text("country")
  if (!is.na(country)) country <- fix.CTRY(country)
  state <- text("state")
  if (!is.na(state)) state <- toupper(state)

  list(
    street = text("street"), city = text("city"), state = state, zip = text("zip"),
    country = country, lat = number("lat"), lon = number("lon")
  )
}


# The plan --------------------------------------------------------------------------------

#' Put a postal code into the form ORS expects, or say it is not valid
#' @return The code, or \code{NA}.
#' @noRd
ors_clean_zip <- function(zip, country) {
  if (is.na(zip) || is.na(country)) return(NA_character_)
  code <- toupper(trimws(zip))
  if (country == "USA") {
    if (grepl("^\\d{5}(-\\d{4})?$", code)) return(substr(code, 1, 5))
  } else if (country == "CAN") {
    code <- sub("^([A-Z]\\d[A-Z])\\s?(\\d[A-Z]\\d)$", "\\1 \\2", code)
    if (grepl("^[A-Z]\\d[A-Z] \\d[A-Z]\\d$", code)) return(code)
  }
  NA_character_
}

#' Decide which lookups to try, in order
#'
#' @return A list with \code{steps} (each a lookup to try), \code{notes}
#'   (things about the input worth reporting if a lookup succeeds), and
#'   \code{failure} (a ready-made failure when no lookup is possible).
#' @noRd
ors_geocode_plan <- function(loc) {
  country <- loc$country
  notes <- character()
  steps <- list()

  # The state, in the form searched for, and in the form checked against.
  state <- loc$state
  state_text <- NA_character_
  if (!is.na(state)) {
    if (!is.na(country) && !valid_state_code(state, country)) {
      notes <- c(notes, sprintf("state '%s' was not recognized and was ignored", state))
      state <- NA_character_
    } else if (!is.na(country) && country == "MEX") {
      state_text <- mx_state_name(mx_state_key(state))
    } else if (valid_state_code(state, country)) {
      state_text <- state
    } else {
      state <- NA_character_
    }
  }

  query <- function(text, layers, size) {
    q <- list(text = text, layers = paste(layers, collapse = ","), size = size)
    if (!is.na(country)) q[["boundary.country"]] <- unname(ors_country_iso2[country])
    q
  }
  rule <- function(layers, city = NA_character_, strict_state = FALSE, check_ambiguity = FALSE) {
    list(
      layers_ok = layers, city = city, country = country, state = state,
      strict_state = strict_state, check_ambiguity = check_ambiguity
    )
  }
  join <- function(...) {
    parts <- c(...)
    paste(parts[!is.na(parts)], collapse = ", ")
  }

  zip_found <- !is.na(loc$zip)
  zip <- ors_clean_zip(loc$zip, country)
  zip_usable <- !is.na(zip)

  if (!is.na(loc$street)) {
    zip_for_street <- if (!is.na(country) && country == "MEX") NA_character_ else loc$zip
    steps[[length(steps) + 1]] <- list(
      group = "street address", strategy = "street address", note = NA_character_,
      query = query(join(loc$street, loc$city, state_text, zip_for_street), "address", 3),
      rule = rule("address")
    )
  }

  if (zip_usable) {
    steps[[length(steps) + 1]] <- list(
      group = "postal code", strategy = "postal code", note = NA_character_,
      query = query(zip, "postalcode", 3),
      rule = rule("postalcode")
    )
  } else if (zip_found && !is.na(country) && country != "MEX") {
    notes <- c(notes, sprintf(
      "postal code '%s' is not a valid %s postal code and was ignored",
      loc$zip, if (country == "USA") "US" else "Canadian"
    ))
  } else if (zip_found && is.na(country)) {
    notes <- c(notes, "postal code was not used because the country is not given")
  }

  if (!is.na(loc$city)) {
    has_state <- !is.na(state_text)
    steps[[length(steps) + 1]] <- list(
      group = "city", strategy = if (has_state) "city and state" else "city",
      note = if (is.na(country)) "country not given; matched without a country restriction" else NA_character_,
      query = query(join(loc$city, state_text), ors_city_layers, if (has_state) 5 else 10),
      rule = rule(ors_city_layers, city = loc$city, check_ambiguity = TRUE)
    )
    # Some state names confuse the search ("Guadalupe, Nuevo Leon" finds Leon);
    # the city name alone, checked against the state, finds them.
    if (has_state && !is.na(country) && country == "MEX") {
      steps[[length(steps) + 1]] <- list(
        group = "city", strategy = "city name alone, checked against the state",
        note = "located by city name alone; the state was used to choose among places with that name",
        query = query(loc$city, ors_city_layers, 10),
        rule = rule(ors_city_layers, city = loc$city, strict_state = TRUE)
      )
    }
  }

  if (length(steps) == 0) {
    return(list(steps = steps, notes = notes, failure = ors_no_lookup_failure(loc, zip_found, notes)))
  }
  list(steps = steps, notes = notes, failure = NULL)
}

#' Explain why a location cannot be looked up at all
#' @noRd
ors_no_lookup_failure <- function(loc, zip_found, notes) {
  country <- loc$country
  if (zip_found && !is.na(country) && country == "MEX") {
    return(geo_fail(
      paste0(
        "zip-only location not supported by this provider (ORS cannot look up Mexican postal codes); ",
        "add the city and state for ", describe_location(loc)
      ),
      permanent = TRUE, kind = "unsupported"
    ))
  }
  if (zip_found && is.na(country)) {
    return(geo_fail(
      paste0("a postal code alone cannot be looked up without a country: ", describe_location(loc)),
      permanent = TRUE, kind = "unsupported"
    ))
  }
  if (zip_found) {
    return(geo_fail(
      paste0("no usable postal code and no city for ", describe_location(loc), "; ", paste(notes, collapse = "; ")),
      permanent = TRUE, kind = "unsupported"
    ))
  }
  geo_fail("the location has no street address, postal code or city", permanent = TRUE, kind = "unsupported")
}


# Running a lookup ---------------------------------------------------------------------------------

#' Send one lookup and judge what comes back
#'
#' @return A list with \code{outcome}: \code{"match"} (with \code{feature} and
#'   \code{flags}), \code{"none"} (nothing acceptable, with \code{notes} saying
#'   why candidates were rejected) or \code{"error"} (the request itself
#'   failed, with the failure in \code{result}).
#' @noRd
ors_run_step <- function(step) {
  res <- ors_request("geocode", "/search", query = step$query)
  if (!isTRUE(res$ok)) {
    return(list(
      outcome = "error",
      result = geo_fail(res$error, isTRUE(res$error_permanent), res$error_kind)
    ))
  }

  features <- res$body[["features"]]
  chosen <- ors_choose(if (is.list(features)) features else list(), step$rule)
  if (is.null(chosen$feature)) {
    return(list(outcome = "none", notes = chosen$notes))
  }
  list(outcome = "match", feature = chosen$feature, flags = chosen$flags)
}

#' Read one Pelias feature into a flat list, or NULL if it is unusable
#' @noRd
ors_parse_feature <- function(f) {
  if (!is.list(f)) return(NULL)
  props <- f[["properties"]]
  coords <- unlist(f[["geometry"]][["coordinates"]])
  if (!is.list(props) || !is.numeric(coords) || length(coords) < 2 ||
      !all(is.finite(coords[1:2])) || abs(coords[1]) > 180 || abs(coords[2]) > 90) {
    return(NULL)
  }
  text <- function(name) {
    v <- props[[name]]
    if (is.character(v) && length(v) == 1 && nzchar(v)) v else NA_character_
  }
  list(
    lon = coords[1], lat = coords[2],
    layer = text("layer"), label = text("label"), name = text("name"),
    region = text("region_a"), country = text("country_a"), match_type = text("match_type")
  )
}

#' Pick the acceptable candidate, if there is one
#'
#' A candidate is rejected unless it is: at one of the asked-for layers; an
#' exact match (not a "fallback" to something coarser); named like the city
#' that was asked for (city lookups); in the right country; and not provably in
#' the wrong state. Of those that remain, one confirmed in the right state is
#' preferred. When no state is given and the remaining candidates are different
#' places, the name is ambiguous and nothing is chosen.
#'
#' @param features The \code{features} list from a Pelias response.
#' @param rule A list: \code{layers_ok}, \code{city} (\code{NA} for no name
#'   check), \code{country}, \code{state}, \code{strict_state} (accept only a
#'   candidate confirmed in the state) and \code{check_ambiguity}.
#' @return A list with \code{feature} (or \code{NULL}), \code{flags} and
#'   \code{notes} (why candidates were rejected).
#' @noRd
ors_choose <- function(features, rule) {
  parsed <- Filter(Negate(is.null), lapply(features, ors_parse_feature))
  notes <- character()
  accepted <- list()

  for (p in parsed) {
    why <- NULL
    if (is.na(p$layer) || !p$layer %in% rule$layers_ok) {
      why <- sprintf("%s is not at the level asked for", p$label)
    } else if (!identical(p$match_type, "exact")) {
      why <- sprintf("%s was only an approximate match", p$label)
    } else if (!is.na(rule$city) && !identical(norm_place(rule$city), norm_place(p$name))) {
      why <- sprintf("%s is not named %s", p$label, rule$city)
    } else if (!is.na(rule$country) && !is.na(p$country) && !identical(p$country, rule$country)) {
      why <- sprintf("%s is in %s, not %s", p$label, p$country, rule$country)
    } else {
      p$relation <- state_relation(rule$state, p$region, rule$country)
      if (p$relation == "mismatch") {
        why <- sprintf("%s is in %s, not %s", p$label, p$region, rule$state)
      }
    }
    if (is.null(why)) {
      accepted[[length(accepted) + 1]] <- p
    } else {
      notes <- c(notes, why)
    }
  }

  none <- function(extra = NULL) list(feature = NULL, flags = character(), notes = c(notes, extra))
  if (length(accepted) == 0) {
    return(none())
  }

  if (!is.na(rule$state)) {
    confirmed <- Filter(function(p) identical(p$relation, "match"), accepted)
    if (length(confirmed) > 0) {
      return(list(feature = confirmed[[1]], flags = character(), notes = notes))
    }
    if (isTRUE(rule$strict_state)) {
      return(none(sprintf("no place with that name could be confirmed in %s", rule$state)))
    }
    return(list(feature = accepted[[1]], flags = "state could not be verified", notes = notes))
  }

  if (isTRUE(rule$check_ambiguity)) {
    places <- unique(vapply(accepted, function(p) sprintf("%s|%.2f|%.2f", p$label, p$lon, p$lat), character(1)))
    if (length(places) > 1) {
      return(none(sprintf(
        "the name matches %d different places and no state was given to choose between them", length(places)
      )))
    }
  }
  list(feature = accepted[[1]], flags = character(), notes = notes)
}
