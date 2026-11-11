#' Run a prepared set of routes through PC*MILER
#'
#' @param lst The list made by \code{\link{pcm_prep_dataframe}}.
#'
#' @return A data frame of the labelled input rows with the PC*MILER results.
#' @export
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

  df_final <- dplyr::left_join(lst$Labeled, df_routes, by = "RouteId") %>%
    dplyr::glimpse()
}
