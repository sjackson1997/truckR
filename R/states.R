# States, provinces and place names -----------------------------------------------
#
# Helpers for comparing the state or province in a lane with the one a geocoder
# returns, and for comparing place names. They know nothing about any one
# provider.

#' Remove accents from Latin letters and lower-case the text
#'
#' Written with Unicode escapes so the source stays plain ASCII.
#' @param x A character vector.
#' @return A lower-case character vector without accents.
#' @noRd
strip_accents <- function(x) {
  from <- "\u00e1\u00e0\u00e2\u00e4\u00e3\u00e5\u00e9\u00e8\u00ea\u00eb\u00ed\u00ec\u00ee\u00ef\u00f3\u00f2\u00f4\u00f6\u00f5\u00fa\u00f9\u00fb\u00fc\u00f1\u00e7"
  to <- "aaaaaaeeeeiiiiooooouuuunc"
  chartr(from, to, tolower(x))
}

#' Put a place name into a form that can be compared
#'
#' Ignores case, accents, punctuation and hyphens, and writes a leading "St" or
#' "Ste" as "Saint" or "Sainte", so "ST. LOUIS", "St Louis" and "Saint Louis"
#' compare equal. Mexico City goes by several names and they are made equal.
#' @param x A character vector.
#' @return A character vector.
#' @noRd
norm_place <- function(x) {
  out <- strip_accents(x)
  out <- gsub("[.'`,]", "", out)
  out <- gsub("[-_/]", " ", out)
  out <- gsub("\\s+", " ", trimws(out))
  out <- sub("^st ", "saint ", out)
  out <- sub("^ste ", "sainte ", out)
  out[out %in% c("cdmx", "distrito federal", "ciudad de mexico", "mexico city")] <- "mexico city"
  out
}

# Mexican states ---------------------------------------------------------------------
# key: the three-letter code geocoders such as Pelias return; name: the plain
# name used in a search; aliases: other codes and spellings people use. Codes
# that mean two states ("CH", "CO") or the country ("MX") are left out on
# purpose: an unrecognized state is checked by nothing rather than by a guess.
mx_states <- data.frame(
  key = c("AGU", "BCN", "BCS", "CAM", "CHP", "CHH", "COA", "COL", "DF", "DUR", "GUA",
          "GRO", "HID", "JAL", "MEX", "MIC", "MOR", "NAY", "NLE", "OAX", "PUE", "QUE",
          "ROO", "SLP", "SIN", "SON", "TAB", "TAM", "TLA", "VER", "YUC", "ZAC"),
  name = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
           "Chiapas", "Chihuahua", "Coahuila", "Colima", "Ciudad de Mexico", "Durango",
           "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de Mexico",
           "Michoacan", "Morelos", "Nayarit", "Nuevo Leon", "Oaxaca", "Puebla",
           "Queretaro", "Quintana Roo", "San Luis Potosi", "Sinaloa", "Sonora",
           "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatan", "Zacatecas"),
  aliases = c("AGS,AG", "BC", "", "CAMP,CM", "CHIS,CS", "CHIH", "COAH,Coahuila de Zaragoza",
              "CL", "CDMX,CMX,DIF,Distrito Federal", "DGO,DG", "GTO,GT", "GR", "HGO,HG",
              "JA,JL", "EM,EDOMEX", "MICH,MI,Michoacan de Ocampo", "MO", "", "NL",
              "OA", "PU", "QRO,QT", "QROO,QR", "SL", "SI", "SO", "TB", "TAMPS,TM",
              "TLAX,TL", "VE,Veracruz de Ignacio de la Llave", "YU", "ZA"),
  stringsAsFactors = FALSE
)

state_token <- function(x) {
  toupper(gsub("[^A-Za-z]", "", strip_accents(x)))
}

# token -> key, built once
mx_state_index <- local({
  tokens <- list()
  for (i in seq_len(nrow(mx_states))) {
    aliases <- strsplit(mx_states$aliases[i], ",", fixed = TRUE)[[1]]
    all_forms <- c(mx_states$key[i], mx_states$name[i], aliases)
    for (form in all_forms) tokens[[state_token(form)]] <- mx_states$key[i]
  }
  unlist(tokens)
})

#' The key of a Mexican state, whatever code or spelling was used
#' @param x A state code or name, or \code{NA}.
#' @return The three-letter key, or \code{NA} if the state is not recognized.
#' @noRd
mx_state_key <- function(x) {
  vapply(x, function(one) {
    if (is.na(one)) return(NA_character_)
    key <- mx_state_index[state_token(one)]
    if (is.na(key)) NA_character_ else unname(key)
  }, character(1), USE.NAMES = FALSE)
}

#' The plain name of a Mexican state, for use in a search
#' @param key A key from \code{mx_state_key()}.
#' @noRd
mx_state_name <- function(key) {
  mx_states$name[match(key, mx_states$key)]
}

us_state_codes <- function() {
  c(datasets::state.abb, "DC", "PR")
}

ca_province_codes <- c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT")

#' Is this a usable state or province code for the country?
#'
#' For an unknown country, a code valid in either the United States or Canada.
#' @noRd
valid_state_code <- function(state, country) {
  if (is.na(state)) return(FALSE)
  code <- toupper(state)
  switch(
    if (is.na(country)) "NA" else country,
    USA = code %in% us_state_codes(),
    CAN = code %in% ca_province_codes,
    MEX = !is.na(mx_state_key(state)),
    `NA` = code %in% c(us_state_codes(), ca_province_codes),
    FALSE
  )
}

#' Compare the state in a lane with the state a geocoder returned
#'
#' @param expected The state or province from the lane, or \code{NA}.
#' @param got The state the geocoder returned (its \code{region_a}), or \code{NA}.
#' @param country \code{"USA"}, \code{"CAN"}, \code{"MEX"} or \code{NA}.
#'
#' @return \code{"match"}, \code{"mismatch"} (both are recognized and differ),
#'   or \code{"unknown"} (one is missing or not recognized, so nothing can be
#'   said). Only a proven mismatch is a reason to reject a result.
#' @noRd
state_relation <- function(expected, got, country) {
  if (is.na(expected) || is.na(got)) return("unknown")

  if (!is.na(country) && country == "MEX") {
    e <- mx_state_key(expected)
    g <- mx_state_key(got)
    if (is.na(e) || is.na(g)) return("unknown")
    return(if (identical(e, g)) "match" else "mismatch")
  }

  e <- toupper(expected)
  g <- toupper(got)
  if (identical(e, g)) return("match")
  if (valid_state_code(e, country) && valid_state_code(g, country)) return("mismatch")
  "unknown"
}
