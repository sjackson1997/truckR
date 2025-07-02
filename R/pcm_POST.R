#' Title
#'
#' @param df
#' @param lst_origins
#' @param lst_destinations
#'
#' @return
#' @export
#'
#' @examples
pcm_POST <- function(df, lst_origins = lst_origins,
                     lst_destinations = lst_destinations) {
  pcm_json <- pcm_Route_json(df,
                             lst_origins = lst_origins,
                             lst_destinations = lst_destinations
  )

  pcm_response <- httr::POST(
    url = route_url_post,
    httr::add_headers(Authorization = PC_MILER_API_KEY),
    body = pcm_json,
    httr::content_type_json(),
    encode = "json"
  )

  df_result <- pcm_MileageReport_to_df(httr::content(pcm_response))

  route_list <- split(df_result, df_result$Label)

  df_destination <- route_list$Destination %>%
    dplyr::rename_with(~ sprintf("%s%s", "dest_", .), .cols = !RouteId) %>%
    dplyr::glimpse()

  df_origin <- route_list$Origin %>%
    dplyr::select(!c(Label, LMiles:TEstghg)) %>%
    dplyr::rename_with(~ sprintf("%s%s", "orig_", .), .cols = !RouteId) %>%
    dplyr::glimpse()

  df_routes <- dplyr::full_join(df_origin, df_destination, by = "RouteId") %>%
    dplyr::glimpse()

  return(df_routes)
}
