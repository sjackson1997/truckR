#' Check a coordinate pair and put it in longitude, latitude order
#'
#' Internal. Routing services take coordinates longitude first, the opposite of
#' how people usually say them, so this is where a mix-up is caught. Names are
#' used when present, which makes \code{c(lat = 19.4, lon = -99.1)} safe;
#' otherwise the first number is the longitude.
#'
#' @param p Two numbers: a named vector or list with the names \code{lon} (or
#'   \code{lng}, \code{long}, \code{longitude}) and \code{lat} (or
#'   \code{latitude}), or two unnamed numbers in the order longitude, latitude.
#' @param what The argument name, used in error messages.
#'
#' @return An unnamed numeric vector: longitude, latitude.
#' @noRd
lonlat_vector <- function(p, what = "coordinates") {
  names_lower <- tolower(names(p))
  if (length(names_lower) > 0) {
    lon_at <- which(names_lower %in% c("lon", "lng", "long", "longitude"))
    lat_at <- which(names_lower %in% c("lat", "latitude"))
    if (length(lon_at) + length(lat_at) == 1) {
      stop(
        sprintf("`%s` names only one of the two coordinates; name both (lon and lat) or neither.", what),
        call. = FALSE
      )
    }
    if (length(lon_at) == 1 && length(lat_at) == 1) {
      p <- p[c(lon_at, lat_at)]
    }
  }

  p <- suppressWarnings(as.numeric(unlist(unname(p))))
  if (length(p) != 2 || anyNA(p)) {
    stop(sprintf("`%s` must be two numbers: longitude and latitude.", what), call. = FALSE)
  }
  if (!all(is.finite(p)) || abs(p[1]) > 180 || abs(p[2]) > 90) {
    stop(
      sprintf(
        paste0(
          "`%s` is outside the valid range (longitude -180 to 180, latitude -90 to 90). ",
          "Coordinates are given longitude first."
        ),
        what
      ),
      call. = FALSE
    )
  }
  p
}
