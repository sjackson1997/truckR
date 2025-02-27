match_columns <- function(column_names, regex_list) {
  matched_columns <- lapply(regex_list, function(regex, cols) {
    # Use grepl to find matches for the current regex in the column names
    matches <- grepl(regex, cols, ignore.case = TRUE)
    # Return the matched column names, or NULL if none matched
    if (any(matches)) cols[matches] else NULL
  }, cols = column_names)

  # Filter out NULL entries from the result
  matched_columns <- Filter(Negate(is.null), matched_columns)

  # Return the list with matched column names
  return(matched_columns)
}
