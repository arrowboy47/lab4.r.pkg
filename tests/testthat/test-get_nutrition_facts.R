  test_that("get_nutrition_facts returns the correct form of data", {
    
    my_result <- get_nutrition_facts(q = "Oatmeal")
    
    expect_equal(names(my_result), c( "url", "recipe", "Energy.kcal",
                                      "Fat.g", "Saturated.Fat.g", "Trans.Fat.g",
                                      "Monounsaturated.Fat.g", "Polyunsaturated.Fat.g",
                                      "Net.Carbs.g", "Fiber.g", "Sugar.g", "Protein.g",
                                      "Cholesterol.mg", "Sodium.mg", "Calcium.mg",
                                      "Magnesium.mg", "Potassium.mg", "Iron.mg"))
    
    expect_s3_class(my_result, "data.frame")
    
    expect_equal(ncol(my_result), 35)
    
  })
