test_that("day 8", {
  x <- example_data_08()
  expect_equal(f08a(x), 21)
  expect_equal(f08b(x), 8)
})
