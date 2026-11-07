test_that("fix.CTRY maps common spellings to 3-letter codes", {
  expect_equal(
    fix.CTRY(c("US", "USA", "U.S.", "us", "United States")),
    c("USA", "USA", "USA", "USA", "UNITEDSTATES")
  )
  expect_equal(fix.CTRY(c("MX", "MEX", "Mexico")), c("MEX", "MEX", "MEX"))
  expect_equal(fix.CTRY(c("CA", "CAN", "CAD", "Canada")), c("CAN", "CAN", "CAN", "CAN"))
})

test_that("fix.CTRY keeps NA and passes unknown values through upper-cased", {
  expect_equal(fix.CTRY(c(NA, "FRA", "fr")), c(NA, "FRA", "FR"))
})

test_that("fix.ZIP pads 4-digit zips and spaces Canadian postal codes", {
  expect_equal(fix.ZIP(c("1234", "78040", "L8E2B4", "L8E 2B4")), c("01234", "78040", "L8E 2B4", "L8E 2B4"))
})
