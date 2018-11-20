#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)

black_voters <- read_rds("black_voters.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Race/Education in the election"),
  
  
  sidebarLayout(
    sidebarPanel(
       selectInput(inputId = "race",
                   label = "Race:",
                   choices = c("black_college_grad",
                              "black_hs",
                              "black_postgrad",
                              "black_some_college"),
                   selected = "black_hs"),
      checkboxInput(inputId = "line", 
                    label = "Show Best Fit Line", 
                    value = FALSE)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
  
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    

    
    # Filter type based on input$type from ui.R
    # I use the type of election, as selected by the user, to filter which election polls/results are shown.
    # I go through the motion of a ggplot by setting labels, titles, etc. to useful/descriptive text. 
    
   #if(input$eth == "Black") {
      
      black_plot <- black_voters %>% 
        #filter(!is.na(input$type)) %>% 
        ggplot(aes_string(x = input$race, y = "dem_win")) + geom_point() + ylab("Dem Win")
      
      #black_plot <- black_voters %>% 
      #  filter(!is.na(black_college_grad)) %>% 
      #  ggplot(aes(x = black_college_grad, y = dem_win)) + geom_point() + ylab("Dem Win")
      
      if (input$line == TRUE) {
        black_plot <- black_plot + geom_smooth(method = lm, se = FALSE)
      }
      
      black_plot
      
    #}
    
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)