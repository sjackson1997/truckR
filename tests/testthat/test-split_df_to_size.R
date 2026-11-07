test_that("split_df_to_size splits into chunks no larger than rows_per_chunk", {
  df <- data.frame(x = 1:45)

  chunks <- split_df_to_size(df, rows_per_chunk = 20L)

  expect_length(chunks, 3)
  expect_equal(vapply(chunks, nrow, integer(1), USE.NAMES = FALSE), c(20L, 20L, 5L))
  expect_equal(do.call(rbind, unname(chunks)), df)
})

test_that("split_df_to_size returns a single chunk when df is small", {
  df <- data.frame(x = 1:3)

  chunks <- split_df_to_size(df, rows_per_chunk = 20L)

  expect_length(chunks, 1)
  expect_equal(chunks[[1]], df)
})
