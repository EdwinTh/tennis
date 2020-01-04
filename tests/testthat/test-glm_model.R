context("create_surface_function")

test_that("create_surface_function throws error with wrong input", {
  expect_error(create_surface_function(tibble(surface = "Hard", "Clay", "Grass")),
               "is.character(surface_vec) is not TRUE", fixed = TRUE)
})

test_that("create_surface_function gives correct output", {
  expected_output <- matrix(c(0, 1, 0, 0, 0, 1), nrow = 3)
  colnames(expected_output) <- c("Clay", "Grass")
  expect_true(create_surface_function(c("Hard", "Clay", "Grass")) %>% is.function())
  func_output <- create_surface_function(c("Hard", "Clay", "Grass"))
  expect_equal(func_output(c("Hard", "Clay", "Grass")), expected_output)
  expect_equal(func_output(c("Hard", "Grass")), expected_output[, 2, drop = FALSE])
})
