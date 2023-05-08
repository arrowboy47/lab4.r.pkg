library(testthat)

source(here::here("R/","sideorder.R"))

# Test cases for the get_recipes function
test_that("get_sidedish function works correctly", {
  # Test with a valid main_course_id and optional q parameter
  main_course_id <- "343176318943d0a5062f5a8f2e851485" # Replace with a valid main course ID
  test_df1 <- get_sidedish(main_course_id, "salad")
  expect_true(nrow(test_df1) > 0) # Expect at least one result
  
  # Test with an invalid main_course_id
  invalid_main_course_id <- "421lkh5vbwwk418542982nbdwkcdwn" # Replace with an invalid main course ID
  test_df2 <- get_sidedish(invalid_main_course_id)
  expect_null(test_df2) # Expect NULL when main_course_id is invalid
  
  # Test with a valid main_course_id without providing q parameter
  test_df3 <- get_sidedish(main_course_id)
  expect_true(nrow(test_df3) > 0) # Expect at least one result
})
