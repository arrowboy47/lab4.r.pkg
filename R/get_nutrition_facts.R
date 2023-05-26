
#' Get nutrition facts
#' 
#' @param app_id A string
#' @param app_key A string
#' @param q A string
#' @param diet A string
#' @param health A string
#' @param cuisineType A string
#' @param mealType A string
#' @param dishType A string
#' @param type A string
#' @param beta A string
#' 
#' @import dplyr
#' 
#' @return A dataframe with recipe link and nutrition facts
#' 
#' @export
#' 



get_nutrition_facts <- function (q, app_id = "1690fc80", app_key = 
                                   "ea4faf1611e1ec77faf0b15cebc7356a", 
                                 diet = "balanced", health = "", cuisineType = "", 
                                 mealType = "", dishType = "", type = "public", 
                                 beta = "false", public = "true") {
  
  stopifnot(is.character(q))
  stopifnot(is.character(app_id))
  stopifnot(is.character(app_key))
  stopifnot(is.character(diet))
  stopifnot(is.character(health))
  stopifnot(is.character(cuisineType))
  stopifnot(is.character(mealType))
  stopifnot(is.character(dishType))
  stopifnot(is.character(type))
  stopifnot(is.character(beta))
  stopifnot(is.character(public))
  
  url_start <- "https://api.edamam.com/api/recipes/v2?"
  
  q <- stringr::str_replace_all(q, "[&]", "%26")
  q <- stringr::str_replace_all(q, " ", "%20")
  
  if(health != ""){
    
    health = glue::glue("&health=", health)
    
  }
  
  if(diet != ""){
    
    diet = glue::glue("&diet=", diet)
    
  }
  
  if(cuisineType != ""){
    
    cuisineType = glue::glue("&cuisineType=", cuisineType)
    
  }
  
  if(mealType != ""){
    
    mealType = glue::glue("&mealType=", mealType)
    
  }
  
  if(dishType != ""){
    
    dishType = glue::glue("&dishType=", dishType)
    
  }
  
  type <- glue::glue("type=", type)
  
  q <- glue::glue("&q=", q)
  
  beta <- glue::glue("&beta=", beta)
  
  public <- glue::glue("&public=", public)
  
  app_id <- glue::glue("&app_id=", app_id)
  
  app_key <- glue::glue("&app_key=", app_key)
  
  
  url <- glue::glue(url_start, type, beta, q, app_id, app_key, diet, health,
                    cuisineType, mealType)
  
  # use this code to see if it correctly builds the URL
  # return(url)
  
  rawJSON <- httr::GET(url)

  JSON_list <- jsonlite::fromJSON(rawToChar(rawJSON$content))

  JSON_hits <- JSON_list$hits

  JSON_recipe <- JSON_hits$recipe
  
  JSON_nutrients <- JSON_recipe$totalNutrients
  
  result <- data.frame('url' = JSON_recipe$url,
                       'recipe' = JSON_recipe$label,
                       'Energy kcal' = JSON_nutrients$ENERC_KCAL$quantity,
                       'Fat g' =  JSON_nutrients$FAT$quantity,
                       'Saturated Fat g' = JSON_nutrients$FASAT$quantity,
                       'Trans Fat g' = JSON_nutrients$FATRN$quantity,
                       'Monounsaturated Fat g' = JSON_nutrients$FAMS$quantity,
                       'Polyunsaturated Fat g' = JSON_nutrients$FAPU$quantity,
                       'Net Carbs g' = JSON_nutrients$CHOCDF.net$quantity,
                       'Fiber g' = JSON_nutrients$FIBTG$quantity,
                       'Sugar g' = JSON_nutrients$SUGAR$quantity,
                       'Protein g' = JSON_nutrients$PROCNT$quantity,
                       'Cholesterol mg' = JSON_nutrients$CHOLE$quantity,
                       'Sodium mg' = JSON_nutrients$`NA`$quantity,
                       'Calcium mg' = JSON_nutrients$CA$quantity,
                       'Magnesium mg' = JSON_nutrients$MG$quantity,
                       'Potassium mg' = JSON_nutrients$K$quantity,
                       'Iron mg' = JSON_nutrients$FE$quantity
  )
  
  return(results)

}