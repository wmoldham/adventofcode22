test_that("day 9", {
  x <- example_data_09(1)
  y <- example_data_09(2)
  expect_equal(f09a(x), 13)
  expect_equal(f09b(x), 1)
  expect_equal(f09b(y), 36)
})
