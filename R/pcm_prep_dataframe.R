pcm_prep_dataframe <- function(df) {
  lst <- get_location_columns1(df)
  v_names <- lst$v_names



  df_labeled <- df %>%
    arrange(!!!syms(v_names)) %>%
    group_by(across(all_of(v_names))) %>%
    mutate(
      RouteId = paste(
        "RTE",
        str_pad(
          cur_group_id(),
          width = ceiling(log(dim(.)[1], base = 10)) + 1,
          side = "left",
          pad = "0"
        ),
        sep = "-"
      ),
      .before = 1,
    ) %>%
    ungroup()

  df_unique <- df_labeled %>%
    group_by(RouteId, across(all_of(v_names))) %>%
    summarise(n = n(), .groups = "drop")

  lst_out <- list(
    "Labeled" = df_labeled,
    "Unique" = df_unique,
    "Origins" = lst$Origins,
    "Destinations" = lst$Destinations
  )

  return(lst_out)
}
