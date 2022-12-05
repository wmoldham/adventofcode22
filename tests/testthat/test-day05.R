test_that("day 5", {
  x <- example_data_05(1)
  y <- example_data_05(2)
  expect_equal(f05(x, y, rev = TRUE), "CMZ")
  expect_equal(f05(x, y, rev = FALSE), "MCD")
})
