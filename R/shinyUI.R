#' Gives you an appropriate side dish to accompany a main course
#' 
#' @param app_id A string of your application id for edamam (required)
#' @param app_key A string of app key for edamam (required)
#' 
#' @return shiny app
#' 
#' @export
#' 

shinyUI <- function(app_id = "6f567a3a", app_key = "aec97451eec00326ae7fedab93b7c250") {
  
  ui

  server <- function(input, output) {
    
  }
  
  return(shinyApp(ui = ui, server = server))

}