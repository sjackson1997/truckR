#' Prepare dataframe for efficient use of PC Miler API
#'
#' @param df A dataframe that contains both origin and destination columns.
#'
#' @return A list containing prepared data for processing via PC Miler API.
#' @export
#'
#' @examples
#' pcm_prep_dataframe(df_route_samples)

pcm_prep_dataframe <- function(df) {
  lst <- get_location_columns1(df)
  v_names <- lst$v_names


  df_labeled <- df %>%
    dplyr::arrange(!!!rlang::syms(v_names)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(v_names))) %>%
    dplyr::mutate(
      RouteId = paste(
        "RTE",
        stringr::str_pad(
          dplyr::cur_group_id(),
          width = ceiling(log(dim(.)[1], base = 10)) + 1,
          side = "left",
          pad = "0"
        ),
        sep = "-"
      ),
      .before = 1,
    ) %>%
    dplyr::ungroup()

  df_unique <- df_labeled %>%
    dplyr::group_by(RouteId, dplyr::across(dplyr::all_of(v_names))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  lst_out <- list(
    "Labeled" = df_labeled,
    "Unique" = df_unique,
    "Origins" = lst$Origins,
    "Destinations" = lst$Destinations
  )

  return(lst_out)
}
