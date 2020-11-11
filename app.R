#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Call Packages
library(tidyverse)
library(shiny)

#Place into Vectors
numerical <- c("price", "carat", "depth", "table", "x", "y", "z")
categorical <- c("clarity", "color", "cut")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Diamonds Dataset Scatterplot"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #Generate x-axis
            selectInput(inputId = "x",
                        label = "X-axis:",
                        choices = numerical,
                        selected = numerical[2]),
            
            #Generate y-axis
            selectInput(inputId = "y",
                        label = "Y-axis:",
                        choices = numerical,
                        selected = numerical[1]),
            
            #Generate plot color
            selectInput(inputId = "z",
                        label = "Plot Color:",
                        choices = categorical,
                        selected = categorical[1]),
            
            uiOutput("selected_z")
        ),
        
        # Show the plot 
        mainPanel(
            plotOutput("scatterplot")
        )
    )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
    
    #Generate Checkbox
    output$selected_z <- renderUI({
        checkboxGroupInput(inputId = "col_cat",
                           label = "Color Categories:",
                           choices = choices_z(),
                           selected = choices_z())
    })
    
    choices_z <- reactive({
        df <- select(diamonds, input$z)
        return(levels(df[[1]]))
    })
    
    #Connect Checkbox to Plot
    output$scatterplot <- renderPlot({
        data <- select(diamonds, input$x, input$y, input$z) %>% 
            filter(!!(as.name(input$z)) %in% input$col_cat)
        
        #Generate Plot
        ggplot(data, aes_string(x = input$x, y = input$y, color = input$z)) +
            geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
