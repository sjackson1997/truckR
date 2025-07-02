#' Title
#'
#' @param report_list
#'
#' @return
#' @export
#'
#' @examples
pcm_MileageReport_to_df <- function(report_list) {
  # Initialize an empty list to store extracted information
  extracted_info <- list()

  # Iterate over each report in the report list
  for (report in report_list) {
    # Iterate over each report line within a report
    for (report_line in report$ReportLines) {
      # Extract Stop and Coords info
      stop_info <- report_line$Stop
      coords_info <- stop_info$Coords

      # Extract error description, handling NULL and empty cases
      if (!is.null(stop_info$Errors) && length(stop_info$Errors) > 0) {
        error_code <- stop_info$Errors[[1]]$Code
        error_legacycode <- stop_info$Errors[[1]]$LegacyErrorCode
        error_description <- stop_info$Errors[[1]]$Description
      } else {
        error_code <- NA_character_
        error_legacycode <- NA_character_
        error_description <- NA_character_
      }

      # Compile the extracted information into a list
      info <- list(
        RouteId = report$RouteID,
        Label = stop_info$Label,
        TimeZone = stop_info$TimeZoneAbbreviation,
        StreetAddress = stop_info$Address$StreetAddress,
        City = stop_info$Address$City,
        County = stop_info$Address$County,
        State = stop_info$Address$StateAbbreviation,
        Zip = stop_info$Address$Zip,
        Country = stop_info$Address$Country,
        Lat = coords_info$Lat,
        Lon = coords_info$Lon,
        Err_Code = error_code,
        Err_LegacyCode = error_legacycode,
        Err_Desc = error_description,
        LMiles = report_line$LMiles,
        TMiles = report_line$TMiles,
        LCostMile = report_line$LCostMile,
        TCostMile = report_line$TCostMile,
        LHours = report_line$LHours,
        THours = report_line$THours,
        LTolls = report_line$LTolls,
        TTolls = report_line$TTolls,
        LEstghg = report_line$LEstghg,
        TEstghg = report_line$TEstghg
      )

      # Append the extracted information list to the main list
      extracted_info <- append(extracted_info, list(info))
    }
  }

  # Convert the list of extracted information into a dataframe
  df <- do.call(rbind, lapply(extracted_info, data.frame, stringsAsFactors = FALSE))

  return(df)
}
