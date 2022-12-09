test_that("day 7", {
  x <- example_data_07()
  expect_equal(f07a(x), 95437)
  expect_equal(f07b(x), 24933642)
})
