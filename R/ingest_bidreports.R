#' Ingest all matching bid reports from a download folder
#'
#' Finds bid report workbooks in \code{download_path} whose file names match the
#' expected pattern, reads each one with [read_BidReport2()], and writes the
#' resulting list to an \code{.rds} file in
#' \code{processed_file_location_relative_path}. The output file name is built
#' from the bid round, report timestamp, and scenario.
#'
#' @param download_path Folder to search for bid report files. Defaults to the
#'   user's Downloads folder.
#' @param bidreport_pattern Substring used to build the file-matching regex
#'   (matches names like \code{All+bids-<pattern>...xlsx} or
#'   \code{Standing+bids-<pattern>...xlsx}).
#' @param sht_name Sheet containing the bids, passed through to
#'   [read_BidReport2()].
#' @param processed_file_location_relative_path Directory where the processed
#'   \code{.rds} files are written. Must already exist.
#' @param delete_source Logical; if \code{TRUE}, delete each source file after it
#'   has been processed. Defaults to \code{FALSE} (sources are kept).
#'
#' @return Invisibly, a character vector of the source files that were
#'   processed.
#' @export
ingest_bidreports <- function(download_path = "C:/Users/KQV2764/Downloads/",
                              bidreport_pattern = "MDLZNATran",
                              sht_name = "Bids",
                              processed_file_location_relative_path = "Data/Processed/BidReports/",
                              delete_source = FALSE) {
  bidreports <- list.files(
    path = download_path,
    full.names = TRUE,
    pattern = paste0("((All)|(Standing))\\+(bids-", bidreport_pattern, ").*\\.xlsx$")
  )

  for (f in bidreports) {
    message(f)

    lst <- read_BidReport2(file = f, sht = sht_name)

    saveRDS(
      lst,
      paste0(
        processed_file_location_relative_path,
        janitor::make_clean_names(paste(
          lst$bid_round,
          format(lst$reportDate, "%y%m%d_%H%M"),
          lst$scenario,
          sep = "_"
        )),
        ".rds"
      )
    )

    message("  bid_round: ", lst$bid_round)
    message("  scenario:  ", lst$scenario)
    message("  reportDate: ", format(lst$reportDate))

    if (isTRUE(delete_source)) {
      file.remove(f)
    }
  }

  return(invisible(bidreports))
}
