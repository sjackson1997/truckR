get_location_columns2 <- function(srchNames, orig_or_dest, regex_list) {
  vec <- grep(orig_or_dest, srchNames, ignore.case = TRUE, value = TRUE)

  lst_out <- match_columns(vec, regex_list)

  return(lst_out)
}

#' Identify origin and destination location columns in a data frame
#'
#' Scans the column names of \code{df} and groups the ones that look like
#' origin/destination address fields, using a list of regex patterns.
#'
#' @param df A data frame whose column names will be searched.
#' @param regex_list Named list of regex patterns for each address component.
#'   Defaults to the package's internal \code{pcm_address_regexes}.
#'
#' @return A list with \code{Origins}, \code{Destinations}, and \code{v_names}
#'   (a flat vector of all matched column names).
#' @export
get_location_columns1 <- function(df, regex_list = pcm_address_regexes) {
  srchNames <- colnames(df)
  lst_origins <-
    get_location_columns2(
      srchNames = srchNames,
      orig_or_dest = "orig",
      regex_list
    )
  lst_destinations <-
    get_location_columns2(
      srchNames = srchNames,
      orig_or_dest = "dest",
      regex_list
    )

  v_names <- unname(unlist(c(lst_origins, lst_destinations)))

  return(list(
    "Origins" = lst_origins,
    "Destinations" = lst_destinations,
    "v_names" = v_names
  ))
}
