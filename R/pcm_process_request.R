pcm_process_request <- function(lst) {
  lst_to_process <- split_df_to_size(lst$Unique)


  df_routes <- do.call(rbind, lapply(
    seq_along(lst_to_process),
    function(x) {
      cat(paste0(
        "\033[0;36mThere are \033[0;41m",
        (length(lst_to_process) - x + 1L),
        "\033[0;36m sets remaining.\033[0m\n"
      ))
      pcm_POST(lst_to_process[[x]],
        lst_origins = lst$Origins,
        lst_destinations = lst$Destinations
      )
    }
  ))

  df_final <- left_join(lst$Labeled, df_routes, by = "RouteId") %>%
    glimpse()
}

split_df_to_size <- function(df, rows_per_chunk = 20) {
  group_factor <- ceiling(seq_along(df[[1]]) / rows_per_chunk)
  list_of_dfs_out <- split(df, group_factor)
  return(list_of_dfs_out)
}
