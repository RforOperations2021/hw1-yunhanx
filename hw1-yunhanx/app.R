#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(readr)

police = read_csv("police.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Theme selector --------------------------------------------------
    shinythemes::themeSelector(),
    theme = shinythemes::shinytheme("united"),
    # Application title
    titlePanel("Pittsburgh Police Arrests Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Select variable for y-axis ----------------------------------
            selectInput(inputId = "y", 
                      label = "Y-axis:",
                      choices = c("Age" = "AGE", 
                                  "Incident Zone" = "INCIDENTZONE", 
                                  "Council District" = "COUNCIL_DISTRICT", 
                                  "Public Works Division" = "PUBLIC_WORKS_DIVISION",
                                  "Longitude" = "X",
                                  "Latitude" = "Y"), 
                      selected = "INCIDENTZONE"),
            
            # Select variable for x-axis ----------------------------------
            selectInput(inputId = "x", 
                      label = "X-axis:",
                      choices = c("Age" = "AGE", 
                                  "Incident Zone" = "INCIDENTZONE", 
                                  "Council District" = "COUNCIL_DISTRICT", 
                                  "Public Works Division" = "PUBLIC_WORKS_DIVISION",
                                  "Longitude" = "X",
                                  "Latitude" = "Y"), 
                      selected = "AGE"),
            
            # Select variable for color -----------------------------------
            selectInput(inputId = "z", 
                      label = "Color by:",
                      choices = c("Gender" = "GENDER", 
                                  "Race" = "RACE"),
                      selected = "RACE"),
            # Show data table ---------------------------------------------
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE),
            # Set alpha level ---------------------------------------------
            sliderInput(inputId = "alpha",
                        label = "Alpha:",
                        min = 0, max = 1,
                        value = 0.5),
            
            # Set point size ----------------------------------------------
            sliderInput(inputId = "size",
                        label = "Size:",
                        min = 0, max = 5,
                        value = 2),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          # Show scatterplot --------------------------------------------
          plotOutput(outputId = "scatterplot"),
          br(),     
          # Show data table ---------------------------------------------
          DT::dataTableOutput(outputId = "policetable")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Create a subset of data filtering for selected title types ------
  police_subset <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(police, title_type %in% input$selected_type)
  })


  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = police, aes_string(x = input$x, y = input$y,
                                              color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(x = input$x,
           y = input$y,
           color = input$z,
           title = ""
      )
  })
  # Print data table if checked -------------------------------------
  output$policetable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = police[, 1:5], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )

}

# Run the application 
shinyApp(ui = ui, server = server)
