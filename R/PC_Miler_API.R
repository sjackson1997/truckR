
PC_MILER_API_KEY <- Sys.getenv("PC_MILER_API_KEY")

base_url <- "https://pcmiler.alk.com/apis/rest/v1.0/Service.svc/route/routeReports"


# GET Examples ----
req <- c(
  "?stops=-75.173297%2C39.942892%3B-74.83153%2C39.61703%3B-74.438942%2C39.362469&reports=Mileage",
  "?stops=-75.173297%2C39.942892%3B-74.83153%2C39.61703%3B-74.438942%2C39.362469&reports=Detail",
  "?stops=-75.173297%2C39.942892%3B-74.83153%2C39.61703%3B-74.438942%2C39.362469&reports=CalcMiles"
)

lst_out <- list()

for (i in seq_along(req)) {
  THE_REQUEST <- paste0(base_url, req[i])

  THE_RESPONSE <- httr::GET(THE_REQUEST, httr::add_headers(Authorization = PC_MILER_API_KEY))
  THE_RESPONSE
  THE_RESPONSE$content
  lst_out <- append(lst_out, httr::content(THE_RESPONSE))
}

lst_out

# Route Reports (POST) -----

route_url_post <- "https://pcmiler.alk.com/apis/rest/v1.0/Service.svc/route/routeReports?dataVersion=Current"

# Example #1 ----
# df_lanes <- readRDS("Data/pcmiler/sample_lanes2.rds")
df_lanes <- readRDS("C:/Users/KQV2764/OneDrive - MDLZ/AdvancedDataAnalyst/Projects_in_R/Resource_Files/Data/pcmiler/sample_lanes2.rds")
# df_lanes <- read_csv("Data/pcmiler/Kenny.csv",
# 										 col_names = TRUE,
# 										 locale = locale(encoding = "latin1"))
#
# df_lanes %>%
# 	as_tibble() %>%
# 	print(n=30)







# Function to convert the complex list structure into a dataframe
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

# Example usage with your list (assuming it's stored in a variable named 'report_list')
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

# df <- df_lanes %>%
# 	pcm_prep_df() %>%
# 	print()
#
# df_final <- pcm_process_request(df$Unique, df$Labeled, df$Origins, df$Destinations)#
