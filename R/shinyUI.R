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
    # Define server logic for random text generation
    server <- function(input, output) {
      
      # Main course 
      
      mainquery <- reactive({ input$maindish })
      
      maindf <- reactiveVal()  # Initialize a reactive value for maindf
      
      observeEvent(mainquery(), {
        # helperfunciton1 notes
        # 3 parameters: 1. string that has name of main course u search for 2. app_id 3. app_key
        # return a df containing a column of api request links (col called "uri"), a string of ingredients(col name "ingredientLines"), link to recipes(col name is "url"), name of dishs (col name "label"),image urls(col name is "image") 
        maindf(get_recipe_info(mainquery(), app_id, app_key))  # Update maindf when mainquery changes
      })
      
      maincourse <- reactive({ maindf()[1,] })  # Make maincourse reactive
      
      output$output1 <- renderUI({
        # Some processing here...
        tagList(
          h2(maincourse()$label),
          img(src = maincourse()$image, height = 200, width = 200),
          h3("Ingredients"),
          HTML(paste(maincourse()$ingredientLines, collapse = "<br>")),  # Display ingredients as HTML
          a("Click here for the recipe", href = maincourse()$url)
        )
      })
      
      
      # Side dish 
      output$output2 <- renderTable({
        idobj <- maincourse()$uri
        # Find the position of the '_' character
        underscore_position <- str_locate(idobj, "_")[1, 1]
        
        # Extract the substring from the '_' character to the end of the string
        theident <- str_sub(idobj, start = underscore_position)
        if (input$"SDinput") {
          side <- lab4.r.pkg::get_sidedish(main_course_id = theident, app_id, app_key)
          side <- side[, c("label", "url")]
          return(side)
        } else {
          # If Side Dish is FALSE, return NULL to display no table
          return(NULL)
        }
      })
      
      
      # nutrients
      output$output3 <- renderPlot({
        # Some processing here...
        dish <- input$maindish
        
        if (dish != "") {
          nutrition_data <- get_nutrition_facts(dish)
          plot_data <- nutrition_data |>
            filter(recipe == dish) |>
            pivot_longer(cols = Energy.kcal:Iron.mg,
                         names_to = "Nutrient",
                         values_to = "Value") |>
            mutate(Nutrient = fct_reorder(Nutrient, Value)) |>
            ggplot(aes(x = Nutrient, y = Value)) +
            geom_segment(aes(x = Nutrient, xend = Nutrient,
                             y = Value, yend = 0),
                         color = "seagreen", alpha = 0.6) +
            geom_point(color = "#006D5B",
                       size = 4,
                       alpha = 1) +
            theme_bw() +
            theme_minimal() +
            coord_flip() +
            labs(x = "",
                 y = "Quantity",
                 title = "Quantity of Each Nutrient",
                 subtitle = "Nutrient")
            
        }
      })
    
      
      
      }
    
    
    
    
    
    
    # Define UI for application
    ui <- fluidPage(
      
      # Application title
      titlePanel("EdaMeal"),
      
      # Sidebar layout with input and output definitions
      sidebarLayout(
        
        # Sidebar panel for inputs
        sidebarPanel(
          
          # Text input prompt
          textInput("maindish", "Search for a Main Course:"),
          
          # Checkbox input for yes/no
          checkboxInput("SDinput", "Include a Side Dish?", value = FALSE)
        ),
        
        # Main panel for displaying outputs
        mainPanel(
          
          # Output panels
          uiOutput("output1"),
          tableOutput("output2"),
          plotOutput("output3"),
        )
      )
    )
    
    # Run the application 
    return(shinyApp(ui = ui, server = server))
    
  }
  