# Named character vector of regex -> replacement pairs used to normalize
# bid-report column names after janitor::make_clean_names(). Names are the
# patterns, values are the replacements (see stringr::str_replace_all).
#
# Stored as INTERNAL package data (R/sysdata.rda) so read_BidReport2() can use
# it without the caller having to supply it. Because sysdata.rda holds every
# internal object together, this script rebuilds it alongside the other
# internal data object (pcm_address_regexes) in a single use_data() call so
# nothing gets dropped.

cso_name_replacements <- c(
  "origin_" = "orig_",
  "destination_" = "dest_",
  "_description" = "_desc",
  "postal_code" = "zip",
  "_postal" = "_zip",
  "(_country)|(_cntry)" = "_ctry",
  "(protection_class_condition)|(protection_class)|(condition)" = "temp",
  "intrmdl" = "imdl",
  "shipments" = "shpts"
)

# Rebuild pcm_address_regexes here too so both internal objects are written
# together (use_data(internal = TRUE) overwrites all of R/sysdata.rda).
pcm_address_regexes <- list(
  StreetAddress = "(address)|(addr)",
  City = "city",
  State = "state",
  Zip = "(zip)|(postal)",
  County = "county",
  Country = "(ctry)|(country)",
  SPLC = "splc",
  CountryPostalFilter = "cpf",
  AbbreviationFormat = "isoformat",
  StateName = "statename",
  StateAbbreviation = "stabbrev",
  CountryAbbreviation = "ctryabbrev"
)

usethis::use_data(pcm_address_regexes,
                  cso_name_replacements,
                  overwrite = TRUE,
                  internal = TRUE)
