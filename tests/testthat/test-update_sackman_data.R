test_that("filter_from_year internal", {
  x <- c("file2001", "file2002", "file2003")
  expect_equal(filter_from_year(x, 2002), c("file2002", "file2003"))
  expect_equal(filter_from_year(x, 2003), "file2003")
})
