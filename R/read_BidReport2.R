#' Read a single bid report Excel file into a structured list
#'
#' Reads one exported bid report workbook, cleans its column names, and returns
#' the bids together with the scenario metadata and a report timestamp parsed
#' from the file name.
#'
#' Column names are first standardized with [janitor::make_clean_names()] and
#' then run through \code{name_replacements} (a named vector of
#' regex-to-replacement pairs, see [stringr::str_replace_all()]) to apply
#' project-specific abbreviations.
#'
#' @param file Path to the bid report \code{.xlsx} file.
#' @param sht Sheet containing the bids, passed to [readxl::read_xlsx()]. Either
#'   a sheet name or a 1-based index. Defaults to the first sheet.
#' @param name_replacements Named character vector of regex patterns (names) and
#'   their replacements (values) applied to the cleaned column names. Defaults to
#'   the package's internal \code{cso_name_replacements}.
#'
#' @return A list with elements \code{bid_round}, \code{scenario}, \code{bids}
#'   (a tibble of the bid rows), \code{file} (the source path), and
#'   \code{reportDate} (a \code{POSIXct} parsed from the file name).
#' @export
read_BidReport2 <- function(file, sht = 1, name_replacements = cso_name_replacements) {
  header_names <- readxl::read_xlsx(
    path = file,
    skip = 2,
    col_types = "text",
    col_names = FALSE,
    sheet = sht,
    n_max = 3
  )

  dt <- as.POSIXct(
    stringr::str_extract(
      file,
      pattern = "[:digit:]{10}\\.[:digit:]{2}"
    ),
    format = "%m%d%y%H%M.%S"
  )

  scenario <- readxl::read_xlsx(
    path = file,
    range = "Selected Scenarios!A3:B3",
    col_names = c("bid_round", "scenario"),
    col_types = "text"
  )

  nm_out <- stringr::str_replace_all(
    janitor::make_clean_names(
      as.character(header_names[1, ])
    ),
    pattern = name_replacements
  )

  df_out <- readxl::read_xlsx(
    path = file,
    skip = 3,
    col_names = nm_out,
    sheet = sht,
    guess_max = 125000
  )

  list_out <- list(
    "bid_round" = scenario[[1, 1]],
    "scenario" = scenario[[1, 2]],
    "bids" = df_out,
    "file" = file,
    "reportDate" = dt
  )

  return(list_out)
}
