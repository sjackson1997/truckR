#' Pick one row of a data frame out as a named list
#'
#' Used to build the address entries of a PC*MILER request.
#'
#' @param df A data frame.
#' @param pcm_lst_addressHdrs A named list or vector: the names are the
#'   PC*MILER address field names and the values are the matching column names
#'   in \code{df}.
#' @param i The row number.
#'
#' @return A named list with one element for each address field.
#' @export
convert_row_to_list <- function(df, pcm_lst_addressHdrs, i) {
  stats::setNames(
    lapply(pcm_lst_addressHdrs, function(column_name) {
      df[[column_name]][i]
    }),
    names(pcm_lst_addressHdrs)
  )
}
