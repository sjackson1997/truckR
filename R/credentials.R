#' Name of the environment variable that holds a provider's API key
#'
#' The convention is \code{TRUCKR_<PROVIDER>_API_KEY}, for example
#' \code{TRUCKR_ORS_API_KEY}.
#'
#' @param provider A provider name.
#'
#' @return The variable name.
#' @noRd
key_env_var <- function(provider) {
  validate_provider_name(provider)
  paste0("TRUCKR_", toupper(provider), "_API_KEY")
}


#' Is an API key configured for a provider?
#'
#' Checks whether the environment variable \code{TRUCKR_<PROVIDER>_API_KEY} is
#' set to something other than blank. The key itself is never shown. For
#' example, the key for the \code{"ors"} provider is read from
#' \code{TRUCKR_ORS_API_KEY}.
#'
#' To store a key, add a line such as \code{TRUCKR_ORS_API_KEY=your_key} to your
#' user \code{.Renviron} file (open it with \code{usethis::edit_r_environ()}) and
#' restart R.
#'
#' @param provider A provider name, for example \code{"ors"}.
#'
#' @return \code{TRUE} or \code{FALSE}.
#' @export
#'
#' @examples
#' truckr_has_key("ors")
truckr_has_key <- function(provider) {
  nzchar(trimws(Sys.getenv(key_env_var(provider))))
}


#' Read a provider's API key
#'
#' Internal. Stops with a clear message, naming the variable and how to set it,
#' when no key is configured. The message never contains the key.
#'
#' @param provider A provider name.
#'
#' @return The key, as a string.
#' @noRd
get_api_key <- function(provider) {
  var <- key_env_var(provider)
  key <- trimws(Sys.getenv(var))

  if (!nzchar(key)) {
    stop(
      sprintf(
        paste0(
          "No API key found for provider `%s`. The key is read from the environment variable %s. ",
          "To set it, add the line %s=your_key to your user .Renviron file ",
          "(open it with usethis::edit_r_environ()) and restart R. ",
          "For the current session only, use Sys.setenv(%s = \"your_key\")."
        ),
        provider, var, var, var
      ),
      call. = FALSE
    )
  }

  key
}


#' Remove a provider's API key from text
#'
#' Internal. Use on any text that came from, or goes to, a place where the key
#' might appear (for example a request URL in an error message) before it is
#' shown or stored.
#'
#' @param text A character vector.
#' @param provider A provider name.
#'
#' @return \code{text} with every occurrence of the key replaced by
#'   \code{"[API key removed]"}. Returned unchanged when no key is set.
#' @noRd
redact_key <- function(text, provider) {
  key <- trimws(Sys.getenv(key_env_var(provider)))
  if (!nzchar(key)) {
    return(text)
  }
  gsub(key, "[API key removed]", text, fixed = TRUE)
}
