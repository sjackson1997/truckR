
pcm_address_regexes <- list(
  Country = "ctry",
  CountryAbbreviation = "ctryabbrev",
  CountryPostalFilter = "cpf",
  State = "state",
  StateName = "statename",
  County = "county",
  City  = "city",
  Zip = "(zip)|(postal)",
  StreetAddress = "address",
  SPLC = "splc",
  AbbreviationFormat = "isoformat",
  StateAbbreviation = "stabbrev"
)

usethis::use_data(pcm_address_regexes, internal = TRUE)
