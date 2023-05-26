#' Get recipe info
#' 
#' @param name a string
#' @param app_id a string
#' @param app_key a string
#' 
#' @import dplyr
#' 
#' @return A dataframe with the ingredients to a specific recipe
#' 
#' @export
#' 


# 3 parameters: 1. string that has name of main course u search for 2. app_id 3. app_key
# return a df containing a column of api request links (col called "uri"), a string of ingredients(col name "ingredientLines"), link to recipes(col name is "url"), name of dishes (col name "label"),image urls (col name is "image") 
get_recipe_info <- function (q, app_id = "1690fc80", app_key = 
                                   "ea4faf1611e1ec77faf0b15cebc7356a", 
                                 diet = "balanced", health = "", cuisineType = "", 
                                 mealType = "", dishType = "", type = "public", 
                                 beta = "false", public = "true") {
  
  stopifnot(is.character(q))
  
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
  
  ingredients <- mapply(paste, JSON_recipe$ingredientLines, collapse = '')
  
  results <- data.frame('uri' = JSON_recipe$uri,
                        'url' = JSON_recipe$url,
                        'ingredients' = ingredients,
                        'label' = JSON_recipe$label,
                        'image' = JSON_recipe$image
                        
  )
  
  
  return(results)
  
}


