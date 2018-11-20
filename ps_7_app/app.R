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
library(ggrepel)

black_voters <- read_rds("black_voters.rds")
asian_voters <- read_rds("asian_voters.rds")
white_voters <- read_rds("white_voters.rds")
hisp_voters <- read_rds("hisp_voters.rds")
other_voters <- read_rds("other_voters.rds")

all_voters <- read_rds("all_voters.rds")

education_choices <- c("Postgraduate Studies" = "postgrad",
  "College Graduate" = "college_grad",
  "Some College" = "some_college", 
  "High School or Less" = "hs")

race_choices <- c("Asian" = "asian",
                  "Black" = "black",
                  "Hispanic" = "hispanic", 
                  "White" = "white",
                  "Other" = "other")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Race/Education in the election"),
  
  
  sidebarLayout(
    sidebarPanel(
      p("Use the selections below to see how Race and Education Levels Correlate with Errors in Polling Predictions"),
      
       selectInput(inputId = "educ",
                   label = "Education:",
                   choices = education_choices,
                   selected = "black_postgrad"),
       selectInput(inputId = "race",
                   label = "Race:",
                   choices = race_choices,
                   selected = "black_postgrad"),

      checkboxInput(inputId = "line", 
                    label = "Show Best Fit Line", 
                    value = FALSE),
      
      checkboxInput(inputId = "district", 
                    label = "Show District Labels", 
                    value = FALSE)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"), 
      br(),
      h4("Summary:"),
      p("[will paste from google docs later]")
    )
  )
)
  
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
  
    if(input$race == "black") {
       black_plot <- black_voters %>% 
        #filter(!is.na(input$type)) %>% 
        ggplot(aes_string(x = paste(input$race,input$educ, sep = "_"), y = "dem_error", color = "dem_error")) + 
         geom_point() + 
         ylab("Dem Error") + 
         xlab(c(paste(names(race_choices[which(race_choices == input$race)]), "+", 
                      names(education_choices[which(education_choices == input$educ)])))) +
         guides(color=guide_legend("Democrat Error"))
      
      if (input$line == TRUE) {
        black_plot <- black_plot + geom_smooth(method = lm, se = FALSE)
      }
       
       if (input$district == TRUE) {
         black_plot <- black_plot + geom_label_repel(aes(label = district), size = 3, force = 3)
      }
      
      black_plot
    }
    
    else if(input$race == "asian") {
      asian_plot <- asian_voters %>% 
        #filter(!is.na(input$type)) %>% 
        ggplot(aes_string(x = paste(input$race,input$educ, sep = "_"), y = "dem_error", color = "dem_error")) + geom_point() + 
        ylab("Dem Error") + 
        xlab(c(paste(names(race_choices[which(race_choices == input$race)]), "+", 
                     names(education_choices[which(education_choices == input$educ)])))) +
        guides(color=guide_legend("Democrat Error"))
      
      if (input$line == TRUE) {
        asian_plot <- asian_plot + geom_smooth(method = lm, se = FALSE)
      }
      
      if (input$district == TRUE) {
        asian_plot <- asian_plot + geom_label_repel(aes(label = district), size = 3, force = 3)
      }
      
      
      asian_plot
      
    }
    
    else if(input$race == "hispanic") {
      hisp_plot <- hisp_voters %>% 
        #filter(!is.na(input$type)) %>% 
        ggplot(aes_string(x = paste(input$race,input$educ, sep = "_"), y = "dem_error", color = "dem_error")) + geom_point() + 
        ylab("Dem Error") + 
        xlab(c(paste(names(race_choices[which(race_choices == input$race)]), "+", 
                     names(education_choices[which(education_choices == input$educ)])))) +
        guides(color=guide_legend("Democrat Error"))
      
      if (input$line == TRUE) {
        hisp_plot <- hisp_plot + geom_smooth(method = lm, se = FALSE)
      }
      
      if (input$district == TRUE) {
        hisp_plot <- hisp_plot + geom_label_repel(aes(label = district), size = 3, force = 3)
      }
      
      hisp_plot
    }
    
    else if(input$race == "white") {
      white_plot <- white_voters %>% 
        #filter(!is.na(input$type)) %>% 
        ggplot(aes_string(x = paste(input$race,input$educ, sep = "_"), y = "dem_error", color = "dem_error")) + geom_point() + 
        ylab("Dem Error") + 
        xlab(c(paste(names(race_choices[which(race_choices == input$race)]), "+", 
                     names(education_choices[which(education_choices == input$educ)]))))+
        guides(color=guide_legend("Democrat Error"))
      
      if (input$line == TRUE) {
        white_plot <- white_plot + geom_smooth(method = lm, se = FALSE)
      }
      
      if (input$district == TRUE) {
        white_plot <- white_plot + geom_label_repel(aes(label = district), size = 3, force = 3)
      }
      
      white_plot
    }
    
    else if(input$race == "other") {
      other_plot <- other_voters %>% 
        #filter(!is.na(input$type)) %>% 
        ggplot(aes_string(x = paste(input$race,input$educ, sep = "_"), y = "dem_error", color = "dem_error")) + 
        geom_point() + 
        ylab("Dem Error") + 
        xlab(c(paste(names(race_choices[which(race_choices == input$race)]), "+", 
             names(education_choices[which(education_choices == input$educ)])))) +
        guides(color=guide_legend("Democrat Error"))
      
      if (input$line == TRUE) {
        other_plot <- other_plot + geom_smooth(method = lm, se = FALSE)
      }
      
      if (input$district == TRUE) {
        other_plot <- other_plot + geom_label_repel(aes(label = district), size = 3, force = 3)
      }
      
      other_plot
    }
    
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)