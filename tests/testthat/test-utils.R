context("has_cols utility function")
test_that("has_cols happy flow", {
  expect_null(has_cols(mtcars))
  expect_null(has_cols(mtcars, "cyl"))
  expect_null(has_cols(mtcars, "cyl", "mpg"))
})

test_that("has_cols error", {
  expect_error(has_cols(mtcars, "adriaantje"),
               "Following columns not in x: adriaantje")
  expect_error(has_cols(mtcars, "adriaantje", "bassie"),
               "Following columns not in x: adriaantje")
  expect_error(has_cols(mtcars, "adriaantje", "mpg"),
               "Following columns not in x: adriaantje")
  expect_error(has_cols(mtcars, "mpg", "adriaantje"),
               "Following columns not in x: adriaantje")
})
