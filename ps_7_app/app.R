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
black_voters <- na.omit(black_voters)

all_voters <- read_rds("all_voters.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Race/Education in the election"),
  
  
  sidebarLayout(
    sidebarPanel(
       selectInput(inputId = "educ",
                   label = "Education:",
                   choices = c("Postgraduate Studies" = "black_postgrad",
                               "College Graduate" = "black_college_grad",
                               "Some College" = "black_some_college", 
                               "High School or Less" = "black_hs"),
                   selected = "black_postgrad"),
       selectInput(inputId = "race",
                   label = "Race:",
                   choices = c("Asian",
                               "Black",
                               "Hispanic", 
                               "White",
                               "Other"),
                   selected = "black_postgrad"),
       # selectInput(inputId = "race",
       #             label = "Education:",
       #             choices = c("Black" = "black",
       #                         "Hispanic" = "hisp",
       #                         "Asian" = "asian", 
       #                         "White" = "white"),
       #             selected = "black"),
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
    
    # all_plot <- all_voters %>% 
    #   #filter(!is.na(input$type)) %>% 
    #   ggplot(aes_string(x = paste(input$race, input$educ, sep = ""), y = "dem_error")) + geom_point() + ylab("Dem Error")
    
    if(input$race == "Black") {
       black_plot <- black_voters %>% 
        #filter(!is.na(input$type)) %>% 
        ggplot(aes_string(x = input$educ, y = "dem_error")) + geom_point() + ylab("Dem Error")
      
      if (input$line == TRUE) {
        black_plot <- black_plot + geom_smooth(method = lm, se = FALSE)
      }
      
      black_plot
    }
    
    else if(input$race == "Asian") {
      
    }
    
    else if(input$race == "Hispanic") {
      
    }
    
    else if(input$race == "White") {
      
    }
    
    else if(input$race == "Other") {
      
    }
    
    
    
   
      
    
    
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)