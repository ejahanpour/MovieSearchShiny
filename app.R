#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(doParallel)
library(foreach)

source('helper_functions/bad.R')
source('helper_functions/good.R')
source('helper_functions/ugly.R')
source('helper_functions/get_movie_name.R')


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),

    # Application title
    titlePanel("The good, the bad, the ugly in R"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = 'keywords_tbx', 
                      label = 'What topic(s) you like to watch today?',
                      value = 'love, world war'),
            actionButton(inputId = 'search_btn',
                         label = 'Search')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("movie_tbl")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$search_btn, {
        kw <- unlist(strsplit(input$keywords_tbx, ','))
        output$movie_tbl <- renderTable(
            extract_text_features_g(file_address = 'datasets/plot_summaries.txt',
                                    keywords = kw)
        )
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
