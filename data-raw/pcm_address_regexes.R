#  https://developer.trimblemaps.com/restful-apis/location/geocoding-api/geocoding/
#  https://developer.trimblemaps.com/restful-apis/appendix/shared-objects/#address
# This list is from the Address portion of the geocoding api

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
                  overwrite = TRUE,
                  internal = TRUE)
