  test_that("get_nutrition_facts returns the correct form of data", {
    
    my_result <- get_nutrition_facts(q = "Oatmeal")
    
    expect_equal(names(my_result), c( "ENERC_KCAL", "FAT", "FASAT", "FATRN", "FAMS",
                                      "FAPU", "CHOCDF", "CHOCDF.net", "FIBTG", 
                                      "SUGAR", "SUGAR.added", "PROCNT", "CHOLE",
                                      "NA", "CA", "MG", "K", "FE", "ZN", "P", 
                                      "VITA_RAE", "VITC", "THIA", "RIBF", "NIA",
                                      "VITB6A", "FOLDFE", "FOLFD", "FOLAC",
                                      "VITB12", "VITD",  "TOCPHA", "VITK1",
                                      "Sugar.alcohol", "WATER"))
    
    expect_s3_class(my_result, "data.frame")
    
    expect_equal(ncol(my_result), 35)
    
  })
