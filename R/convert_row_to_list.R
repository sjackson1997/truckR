#' Title
#'
#' @param df
#' @param pcm_lst_addressHdrs
#' @param i
#'
#' @return
#' @export
#'
#' @examples
convert_row_to_list <- function(df, pcm_lst_addressHdrs, i) {
  setNames(
    lapply(pcm_lst_addressHdrs, function(column_name) {
      df[[column_name]][i]
    }),
    names(pcm_lst_addressHdrs)
  )
}

