test_that("get_recipe_names returns the correct form of data", {
  
  my_result <- get_recipe_names(q = "Oatmeal")
  
  expect_equal(names(my_result), c("label", "url"))
  
  expect_s3_class(my_result, "data.frame")
  
  expect_equal(ncol(my_result), 2)
  
})
