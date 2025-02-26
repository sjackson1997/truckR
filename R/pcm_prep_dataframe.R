pcm_prep_dataframe <- function(dataframe){

  srchNames <- colnames(dataframe)

  vOrigins <- grep("orig", srchNames, ignore.case = TRUE, value = TRUE)
  vDestinations <- grep("dest", srchNames, ignore.case = TRUE, value = TRUE)

  lst_origins <- match_columns(vOrigins, pcm_lst_address)
  lst_destinations <- match_columns(vDestinations, pcm_lst_address)

  v_names <- unname(unlist(c(lst_origins, lst_destinations)))

  df_labeled <- dataframe %>%
    arrange(!!!syms(v_names)) %>%
    group_by(across(all_of(v_names))) %>%
    mutate(RouteId = paste(
      "RTE",
      str_pad(
        cur_group_id(),
        width = 6,
        side = "left",
        pad = "0"
      ),
      sep = "-"
    ),
    .before = 1,
    ) %>%
    ungroup()

  df_unique <- df_labeled %>%
    group_by(RouteId, across(all_of(v_names))) %>%
    summarise(n = n(), .groups = "drop")

  lst_out <- list("Labeled" = df_labeled,
                  "Unique" = df_unique,
                  "Origins" = lst_origins,
                  "Destinations" = lst_destinations)

}


# Define a function to create the JSON structure for each row
pcm_lst_address <- list(
  Country = "ctry",
  CountryAbbreviation = "ctryabbrev",
  CountryPostalFilter = "cpf",
  State = "state",
  StateName = "statename",
  County = "county",
  City  = "city",
  Zip = "zip",
  StreetAddress = "address",
  SPLC = "splc",
  AbbreviationFormat = "isoformat",
  StateAbbreviation = "stabbrev"
)

match_columns <- function(column_names, regex_list) {
  matched_columns <- lapply(regex_list, function(regex, cols) {
    # Use grepl to find matches for the current regex in the column names
    matches <- grepl(regex, cols, ignore.case = TRUE)
    # Return the matched column names, or NULL if none matched
    if(any(matches)) cols[matches] else NULL
  }, cols = column_names)

  # Filter out NULL entries from the result
  matched_columns <- Filter(Negate(is.null), matched_columns)

  # Return the list with matched column names
  return(matched_columns)
}
