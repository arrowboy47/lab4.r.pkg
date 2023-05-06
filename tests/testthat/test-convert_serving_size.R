# Test that the function returns a list
test_that("convert_serving_size returns a list", {

  ingredients <- c("flour", "sugar", "eggs", "milk")
  amounts <- c("1.5", "1", "2", "1.25")
  units <- c("cups", "cups", "NA", "cups")

  result <- convert_serving_size(old = 4, new = 6, ingredients, amounts, units)
  expect_type(result, "list")
})



# Test that the function calculates the correct new amounts
test_that("convert_serving_size calculates the correct new amounts", {

  ingredients <- c("flour", "sugar", "eggs", "milk")
  amounts <- c("1.5", "1", "2", "1.25")
  units <- c("cups", "cups", "NA", "cups")

  result <- convert_serving_size(old = 4, new = 6, ingredients, amounts, units)
  expected_amounts <- c(2.25, 1.5, 3, 1.875)
  expect_equal(result$amount, expected_amounts)
})



# Test that the function handles NULL units correctly
test_that("convert_serving_size handles NULL units correctly", {

  ingredients <- c("flour", "sugar", "eggs", "milk")
  amounts <- c("1.5", "1", "2", "1.25")
  units <- c("cups", "cups", "NA", "cups")

  result <- convert_serving_size(old = 4, new = 6, ingredients, amounts, units = NULL)
  expect_identical(result$units, NULL)
})
