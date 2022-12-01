test_that("day 1", {
  x <- example_data_01()
  expect_equal(f01a(x), 24000)
  expect_equal(f01b(x), 45000)
})
