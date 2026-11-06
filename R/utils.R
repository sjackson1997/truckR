#' Normalize ZIP / postal codes
#'
#' Cleans a vector of ZIP or postal codes: strips spaces, pads 4-digit US ZIPs
#' with a leading zero, maps a couple of special sentinel values, and spaces out
#' Canadian postal codes.
#'
#' @param zip A character vector of ZIP / postal codes.
#' @return A character vector of cleaned codes.
#' @export
fix.ZIP <- function(zip) {
  zip <- stringr::str_replace_all(zip, " ", "")
  zip <- dplyr::case_when(
    zip == "926" ~ "PrRico",
    zip == "0" ~ "IMPEXP",
    stringr::str_length(zip) == 4 ~ stringr::str_c("0", zip),
    stringr::str_detect(zip, "([:alpha:][:digit:]){3}") ~ stringr::str_replace(
      zip,
      "([:alpha:][:digit:][:alpha:])([:digit:][:alpha:][:digit:])",
      "\\1 \\2"
    ),
    TRUE ~ zip
  )
  return(zip)
}

#' Normalize country codes
#'
#' Maps common 2-letter country codes to the 3-letter forms expected downstream
#' (US -> USA, MX -> MEX, CA -> CAN).
#'
#' @param ctry A character vector of country codes.
#' @return A character vector of normalized country codes.
#' @export
fix.CTRY <- function(ctry) {
  ctry <- dplyr::case_when(
    stringr::str_detect(ctry, "US") ~ "USA",
    stringr::str_detect(ctry, "MX") ~ "MEX",
    stringr::str_detect(ctry, "CA") ~ "CAN",
    TRUE ~ ctry
  )
  return(ctry)
}

#' Normalize transportation mode codes
#'
#' Upper-cases the mode and collapses common variants to canonical values
#' (e.g. OTR / *TL -> TL, anything starting with I -> INTRMDL).
#'
#' @param mode A character vector of mode codes.
#' @return A character vector of normalized mode codes.
#' @export
fix.MODE <- function(mode) {
  mode <- stringr::str_to_upper(mode)
  mode <- dplyr::case_when(
    stringr::str_detect(mode, "(OTR)|([^L]TL)") ~ "TL",
    stringr::str_detect(mode, "^I") ~ "INTRMDL",
    TRUE ~ mode
  )
  return(mode)
}
