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
            # Select sample size ----------------------------------------------------
            numericInput(inputId = "n_samp", 
                         label = "Sample size:", 
                         min = 1, max = nrow(police), 
                         value = 50),
            
            # Set point size ----------------------------------------------
            sliderInput(inputId = "size",
                        label = "Size:",
                        min = 0, max = 5,
                        value = 2),
            # Select which types of movies to plot ------------------------
            checkboxGroupInput(inputId = "selected_type",
                               label = "Select year(s):",
                               choices = c("2021", "2020"),
                               selected = "2020"),
            downloadLink('downloadData', 'Download'),
            
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

# Define server logic required
server <- function(input, output, session) {
  # Create a subset of data filtering for selected title types ------
  police_subset <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(police, YEAR %in% input$selected_type)
  })

  # Update the maximum allowed n_samp for selected type movies ------
  observe({
    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(50, nrow(police_subset())),
                       max = nrow(police_subset())
    )
  })
  # Create new df that is n_samp obs from selected type movies ------
  police_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(police_subset(), input$n_samp)
  })
  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = police_sample(), aes_string(x = input$x, y = input$y,
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
      DT::datatable(data = police[, 2:6], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(police, con)
    }
  )

}

# Run the application 
shinyApp(ui = ui, server = server)
