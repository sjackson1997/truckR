fix.ZIP <- function(zip){
  zip <- stringr::str_replace_all(zip, " ", "")
  zip <- dplyr::case_when(zip=="926" ~ "PrRico",
                         zip=="0" ~ "IMPEXP",
                         stringr::str_length(zip) == 4 ~ stringr::str_c("0",zip),
                         TRUE ~ zip)
  return(zip)
}

fix.CTRY <- function(ctry){
  ctry <-  dplyr::case_when(stringr::str_detect(ctry, "US")~ "USA",
                          stringr::str_detect(ctry, "MX")~ "MEX",
                          stringr::str_detect(ctry, "CA")~ "CAN",
                          TRUE ~ ctry)
  return(ctry)
}

fix.MODE <- function(mode){
  mode <- stringr::str_to_upper(mode)
  mode <- dplyr::case_when(stringr::str_detect(mode, "(OTR)|([^L]TL)")~ "TL",
                          stringr::str_detect(mode, "^I")~"INTRMDL",
                          TRUE ~ mode)
  return(mode)
}
