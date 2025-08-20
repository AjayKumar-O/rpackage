test_that("make_filename returns correct format", {
  expect_equal(make_filename(2015), "accident_2015.csv.bz2")
})
