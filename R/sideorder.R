#' Gives you an appropriate side dish to accompany a main course
#' 
#' @param app_id A string of your application id for edamam (required)
#' @param app_key A string of app key for edamam (required)
#' @param q A string of the side dish to search for (optional)
#' @param maincourse edamam url to maincourse meal (required)
#' 
#' @return A dataframe with recipe names and urls
#' 
#' @export
#' 

get_sidedish <- function(main_course_id, q = NULL, app_id = "6f567a3a", app_key = "aec97451eec00326ae7fedab93b7c250") {
  base_url <- "https://api.edamam.com/search"
  main_url <- "https://api.edamam.com/api/recipes/v2"
  
  # Fetch the main course recipe details
  main_course_request_url <- paste0(main_url, '/', main_course_id,
                                    "?type=public&app_id=", app_id, "&app_key=", app_key)
  main_course_response <- httr::GET(main_course_request_url)
  
  # Check if the main course request was successful
  if (httr::http_status(main_course_response)$category == "Success") {
    main_course_json <- jsonlite::fromJSON(httr::content(main_course_response, as = "text", encoding = "UTF-8"))
    
    # Extract the cuisineType field 
    cuisine_type <- main_course_json$recipe$cuisineType
    cuisine_type_cap <- tools::toTitleCase(cuisine_type)
    
    # Build API request URL for side dish search
    request_url <- if (!is.null(q)) {
      paste0(base_url,"?q=", q,
             "&app_id=", app_id,
             "&app_key=", app_key,
             "&cuisineType=", cuisine_type_cap, 
             # the side dish filter wasnt really working so im adding others to get more results
             "&dishType=Bread&dishType=Preps&dishType=Preserve&dishType=Salad&dishType=Sandwiches&dishType=Side dish&dishType=Soup&dishType=Starter")
    }else{
      paste0(main_url,
             "?type=public",
             "&app_id=", app_id,
             "&app_key=", app_key,
             "&cuisineType=", cuisine_type_cap,
             "&dishType=Bread&dishType=Preps&dishType=Preserve&dishType=Salad&dishType=Sandwiches&dishType=Side dish&dishType=Soup&dishType=Starter")
    }
    
    # Send GET request to the API
    response <- httr::GET(utils::URLencode(request_url))
    
    # Check if the request was successful
    if (httr::http_status(response)$category == "Success") {
      # Parse the JSON response
      json_data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
      
      # Extract the 'hits' (recipes) and convert to a dataframe
      recipe_df <- as.data.frame(json_data$hits$recipe)
      
      return(recipe_df)
    } else {
      # Print an error message if the request failed
      cat("Error:", httr::http_status(response)$message, "\n")
      return(NULL)
    }
  } else {
    # Print an error message if the main course ID is not valid
    cat("Error: Invalid main_course_id\n")
    return(NULL)
  }
}

# Example usage
main_course_id <- "343176318943d0a5062f5a8f2e851485"
recipe_88df <- get_sidedish(main_course_id)
