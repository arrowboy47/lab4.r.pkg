test_that("convert_units works", {

  correct_result <- "1.7 gal is equal to 13.6 pt."

  my_result <- convert_units(1.7, from_unit = "gal", to_unit = "pt")

  expect_equal(my_result, correct_result)
})

test_that("convert_units gives helpful error message ", {

  correct_result <- "Invalid input: Please specify valid units, such as
  'tsp', 'tbsp', 'fl oz', 'cup', 'pt', 'qt', or 'gal'."

  my_result <- convert_units(2, from_unit = "cm", to_unit = "gal")
})
