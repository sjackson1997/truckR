#' truckR: truck routing and bid report utilities
#'
#' Distances and driving times for freight lanes, with optional waypoints
#' (see \code{\link{route}}), plus helpers for reading transportation bid
#' reports and for the PC*MILER service.
#'
#' Routes are looked up through a provider (OpenRouteService is built in), and
#' answers are saved on disk so that no lookup is paid for twice. See
#' \code{\link{truckr_has_key}} for setting up the API key and
#' \code{\link{truckr_cache_info}} for the saved answers.
#'
#' @keywords internal
"_PACKAGE"

# Column names used inside dplyr and tidyselect calls in the PC*MILER functions.
utils::globalVariables(c(".", "RouteId", "Label", "LMiles", "TEstghg"))
