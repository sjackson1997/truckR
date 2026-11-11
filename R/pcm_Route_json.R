#' Convert dataframe of origin/destination pairs to JSON file for PC Miler API
#'
#' @param df A dataframe containing origin/destination column(s).
#' @param ReportType The PC*MILER report type.
#' @param lst_origins Names of the origin address fields and the columns that
#'   hold them.
#' @param lst_destinations The same for the destination.
#'
#' @return A JSON request body.
#' @export
pcm_Route_json <- function(df, ReportType = "MileageReportType",
                           lst_origins = lst_origins,
                           lst_destinations = lst_destinations) {
  tl <- list(ReportRoutes = lapply(1:nrow(df), function(i) {
    list(
      ReportTypes = list(
        list(`__type` = paste0(ReportType, ":http://pcmiler.alk.com/APIs/v1.0"))
      ),
      RouteId = df$RouteId[i],
      Stops = list(
        list(
          Address = convert_row_to_list(df, lst_origins, i),
          Label = "Origin",
          Region = "4"
        ),
        list(
          Address = convert_row_to_list(df, lst_destinations, i),
          Label = "Destination",
          Region = "4"
        )
      )
    )
  }))

  pcm_JSON_out <- jsonlite::toJSON(tl, auto_unbox = TRUE, pretty = TRUE)
}

