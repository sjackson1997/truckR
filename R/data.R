#' Sample lanes
#'
#' A small set of clean example lanes (Canada, the United States and Mexico),
#' for trying out the package.
#'
#' @format A tibble with 80 rows and 9 columns:
#' \describe{
#'   \item{orig_city, orig_state, orig_zip, orig_ctry}{Origin city, state or
#'     province, postal code and country.}
#'   \item{dest_city, dest_state, dest_zip, dest_ctry}{The same for the
#'     destination.}
#'   \item{duplicate}{Reserved; always \code{NA}.}
#' }
"df_route_samples"

#' Sample lanes with deliberate errors
#'
#' Lanes with the kinds of mistakes found in real data (bad postal codes,
#' country names written in different ways), for testing clean-up code such as
#' \code{\link{fix.ZIP}} and \code{\link{fix.CTRY}}.
#'
#' @format A data frame of lanes with the same kind of columns as
#'   \code{\link{df_route_samples}}.
"df_route_unclean"
