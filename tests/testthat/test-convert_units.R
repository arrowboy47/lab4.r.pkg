test_that("convert_units works", {

  correct_result <- "1.7 gal is equal to 13.6 pt."

  my_result <- convert_units(1.7, from_unit = "gal", to_unit = "pt")

  expect_equal(my_result, correct_result)
})
