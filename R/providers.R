# Provider registry ---------------------------------------------------------
#
# A provider is a named list of functions that can answer "route from point A
# to point B". Providers are registered by name and one of them is the active
# provider for the session. The state lives in an environment so it can be
# changed from package functions.

.truckr_state <- new.env(parent = emptyenv())
.truckr_state$providers <- list()
.truckr_state$active <- NULL


#' Register a routing provider
#'
#' Adds a routing provider so it can be selected with
#' \code{\link{truckr_set_provider}}. A provider is a named list that must
#' contain these functions:
#' \itemize{
#'   \item \code{geocode(location)}: takes one location (a list with whatever
#'     of \code{street}, \code{city}, \code{state}, \code{zip}, \code{country}
#'     is known) and returns its coordinates, or a reason it failed;
#'   \item \code{route_segment(from, to)}: takes two sets of coordinates and
#'     returns the driving distance in meters and duration in seconds, or an
#'     error message;
#'   \item \code{validate_credentials()}: checks that the provider can be used
#'     (for example that an API key is present).
#' }
#' It may also contain \code{limits}, a list with \code{requests_per_minute}
#' (a positive number) used to throttle requests; \code{profile}, a single
#' string naming the routing profile (for example \code{"driving-hgv"}), which
#' is part of the cache key; and \code{remaining_quota}, a function taking the
#' kind of request (\code{"route"} or \code{"geocode"}) and returning how many
#' requests are left today, or \code{NA} if that is not known. It lets a run
#' tell you up front whether it will fit and stop cleanly before the quota is
#' used up. Other elements are ignored.
#'
#' The registration is checked: each function must exist and accept enough
#' arguments, so a mistake is reported here and not in the middle of a long run.
#'
#' @param name A short lower-case name made of letters, digits and underscores,
#'   starting with a letter (for example \code{"ors"}). It is also used to build
#'   the name of the API key environment variable,
#'   \code{TRUCKR_<NAME>_API_KEY}.
#' @param provider The provider: a named list as described above.
#' @param overwrite If \code{FALSE} (the default), registering a name that is
#'   already registered is an error.
#'
#' @return The provider name, invisibly.
#' @seealso \code{\link{truckr_set_provider}}, \code{\link{truckr_list_providers}}
#' @export
#'
#' @examples
#' \dontrun{
#' register_provider("example", list(
#'   geocode = function(location) NULL,
#'   route_segment = function(from, to) NULL,
#'   validate_credentials = function() TRUE
#' ))
#' }
register_provider <- function(name, provider, overwrite = FALSE) {
  validate_provider_name(name)
  validate_provider(provider, name)

  if (name %in% names(.truckr_state$providers) && !isTRUE(overwrite)) {
    stop(
      sprintf(
        "A provider named `%s` is already registered. Use `overwrite = TRUE` to replace it.",
        name
      ),
      call. = FALSE
    )
  }

  .truckr_state$providers[[name]] <- provider
  invisible(name)
}


#' Choose the active routing provider
#'
#' Sets the provider that \code{route()} uses for the rest of the R session.
#'
#' @param provider_name The name of a registered provider.
#'
#' @return The provider name, invisibly.
#' @seealso \code{\link{truckr_list_providers}}, \code{\link{register_provider}}
#' @export
#'
#' @examples
#' \dontrun{
#' truckr_set_provider("ors")
#' }
truckr_set_provider <- function(provider_name) {
  if (!is.character(provider_name) || length(provider_name) != 1 || is.na(provider_name)) {
    stop("`provider_name` must be a single string.", call. = FALSE)
  }
  if (!provider_name %in% names(.truckr_state$providers)) {
    stop(unknown_provider_message(provider_name), call. = FALSE)
  }

  .truckr_state$active <- provider_name
  invisible(provider_name)
}


#' Get the name of the active routing provider
#'
#' @return The name of the active provider, or \code{NULL} if none has been
#'   chosen with \code{\link{truckr_set_provider}}.
#' @seealso \code{\link{truckr_set_provider}}
#' @export
#'
#' @examples
#' truckr_get_provider()
truckr_get_provider <- function() {
  .truckr_state$active
}


#' List the registered routing providers
#'
#' @return A sorted character vector of provider names (empty if none are
#'   registered).
#' @seealso \code{\link{register_provider}}
#' @export
#'
#' @examples
#' truckr_list_providers()
truckr_list_providers <- function() {
  sort(as.character(names(.truckr_state$providers)))
}


#' Find a registered provider by name
#'
#' @param name The provider name.
#'
#' @return A list with \code{name} and \code{provider}.
#' @noRd
get_provider <- function(name) {
  if (!is.character(name) || length(name) != 1 || is.na(name) ||
      !name %in% names(.truckr_state$providers)) {
    stop(unknown_provider_message(name), call. = FALSE)
  }
  list(name = name, provider = .truckr_state$providers[[name]])
}


#' Work out which provider to use
#'
#' @param provider A provider name, or \code{NULL} to use the active provider.
#'
#' @return A list with \code{name} and \code{provider}. Stops with a clear
#'   message when no provider is configured.
#' @noRd
resolve_provider <- function(provider = NULL) {
  if (is.null(provider)) provider <- .truckr_state$active

  if (is.null(provider)) {
    stop(
      paste0(
        "No routing provider is configured. Choose one with ",
        "truckr_set_provider(\"<name>\"). ",
        available_providers_text(), "."
      ),
      call. = FALSE
    )
  }

  get_provider(provider)
}


available_providers_text <- function() {
  available <- truckr_list_providers()
  if (length(available) == 0) {
    "Available providers: (none registered)"
  } else {
    paste0("Available providers: ", paste(available, collapse = ", "))
  }
}


unknown_provider_message <- function(name) {
  shown <- if (is.character(name) && length(name) == 1 && !is.na(name)) name else "?"
  sprintf("Provider `%s` is not registered. %s.", shown, available_providers_text())
}


validate_provider_name <- function(name) {
  if (!is.character(name) || length(name) != 1 || is.na(name) ||
      !grepl("^[a-z][a-z0-9_]*$", name)) {
    stop(
      paste0(
        "A provider name must be a single lower-case string of letters, digits ",
        "and underscores, starting with a letter (for example \"ors\")."
      ),
      call. = FALSE
    )
  }
  invisible(name)
}


validate_provider <- function(provider, name) {
  fail <- function(msg) {
    stop(sprintf("Cannot register provider `%s`: %s", name, msg), call. = FALSE)
  }

  if (!is.list(provider) || is.data.frame(provider)) {
    fail("a provider must be a named list of functions.")
  }

  required <- c(geocode = 1L, route_segment = 2L, validate_credentials = 0L)
  missing <- setdiff(names(required), names(provider))
  if (length(missing) > 0) {
    fail(sprintf(
      paste0(
        "missing required element(s): %s. A provider needs geocode(location), ",
        "route_segment(from, to) and validate_credentials()."
      ),
      paste(missing, collapse = ", ")
    ))
  }

  for (element in names(required)) {
    f <- provider[[element]]
    if (!is.function(f)) {
      fail(sprintf("`%s` must be a function.", element))
    }
    args <- names(formals(f))
    if (!"..." %in% args && length(args) < required[[element]]) {
      fail(sprintf("`%s` must accept at least %d argument(s).", element, required[[element]]))
    }
  }

  limits <- provider[["limits"]]
  if (!is.null(limits)) {
    msg <- "`limits` must be a list, and `limits$requests_per_minute` a single positive number."
    if (!is.list(limits)) fail(msg)
    rpm <- limits[["requests_per_minute"]]
    if (!is.null(rpm) && (!is.numeric(rpm) || length(rpm) != 1 || is.na(rpm) || rpm <= 0)) {
      fail(msg)
    }
  }

  remaining_quota <- provider[["remaining_quota"]]
  if (!is.null(remaining_quota)) {
    if (!is.function(remaining_quota)) {
      fail("`remaining_quota` must be a function.")
    }
    args <- names(formals(remaining_quota))
    if (!"..." %in% args && length(args) < 1) {
      fail("`remaining_quota` must accept at least 1 argument (the kind of request: \"route\" or \"geocode\").")
    }
  }

  profile <- provider[["profile"]]
  if (!is.null(profile)) {
    if (!is.character(profile) || length(profile) != 1 || is.na(profile) || !nzchar(profile)) {
      fail("`profile` must be a single non-empty string.")
    }
  }

  invisible(provider)
}
