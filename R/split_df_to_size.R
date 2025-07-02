#' Create list of dataframes from original dataframe with max rows_per_chunk
#'
#' @param df A dataframe.
#' @param rows_per_chunk An integer indicating the max number of rows for each dataframe in the resulting list.
#'
#' @return A list of dataframes each sized to 'rows_per_chunk' or less.
#' @export
#'
#' @examples
#' split_df_to_size(df_route_samples)
#' split_df_to_size(df_route_samples, rows_per_chunk = 13)
split_df_to_size <- function(df, rows_per_chunk = 20L) {
  group_factor <- ceiling(seq_along(df[[1]]) / rows_per_chunk)
  list_of_dfs_out <- split(df, group_factor)
  return(list_of_dfs_out)
}
