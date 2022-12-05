test_that("day 3", {
  x <- example_data_03()
  expect_equal(f03a(x), 157)
  expect_equal(f03b(x), 70)
})
