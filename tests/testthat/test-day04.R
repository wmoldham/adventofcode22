test_that("day 4", {
  x <- example_data_04()
  expect_equal(f04a(x), 2)
  expect_equal(f04b(x), 4)
})

