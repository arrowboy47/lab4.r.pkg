#' Gives you an appropriate side dish to accompany a main course
#' 
#' @param app_id A string of your application id for edamam (required)
#' @param app_key A string of app key for edamam (required)
#' 
#' @return shiny app
#' 
#' @import shiny
#' 
#' @export
#' 

shinyUI <- function(app_id = "6f567a3a", app_key = "aec97451eec00326ae7fedab93b7c250") {

  
  # Define UI for application
  ui <- fluidPage(
    
    # Application title
    titlePanel("EdaMeal"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
      
      # Sidebar panel for inputs
      sidebarPanel(
        
        # Text input prompt
        textInput("text", "Enter text:"),
        
        # Checkbox input for yes/no
        checkboxInput("checkbox", "Yes or No", value = FALSE)
      ),
      
      # Main panel for displaying outputs
      mainPanel(
        
        # Output panels
        textOutput("output1"),
        textOutput("output2"),
        textOutput("output3")
      )
    )
  )
  
  # Define server logic for random text generation
  server <- function(input, output) {
    
    # You can define the output for each panel here
    # For example:
    output$output1 <- renderText({
      # Some processing here...
    })
    
    output$output2 <- renderText({
      # Some processing here...
    })
    
    output$output3 <- renderText({
      # Some processing here...
    })
  }
  
  # Run the application 
  return(shinyApp(ui = ui, server = server))
  
}