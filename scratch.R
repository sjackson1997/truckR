lst <- get_location_columns1(df_route_unclean)
# lst <- list(Origins = list(
#   City = "orig_city", State = "orig_state", Country = "orig_ctry"
# ), Destinations = list(
#   City = "dest_city", State = "dest_state", Country = "dest_ctry"
# ), v_names = c(
#   "orig_city", "orig_state",
#   "orig_zip", "orig_ctry", "dest_city", "dest_state", "dest_zip",
#   "dest_ctry"
# ))

zip_vector <- unlist(sapply(lst[c("Origins", "Destinations")], function(x) x$Zip))
zip_vector
str(zip_vector)
df_route_samples[,zip_vector] <- lapply(df_route_samples[, zip_vector], fix.ZIP)
df_route_samples

df_route_unclean[,zip_vector] <- lapply(df_route_unclean[, zip_vector], fix.ZIP)
df_route_unclean %>%
  filter(!is.na(bad_introduced)) %>%
  print(n=80)
