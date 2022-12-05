test_that("day 2", {
  x <- example_data_02()
  expect_equal(f02a(x), 15)
  expect_equal(f02b(x), 12)
})
