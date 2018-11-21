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

# Here, I am reading in the data with the file path to read from
black_voters <- read_rds("black_voters.rds")
asian_voters <- read_rds("asian_voters.rds")
white_voters <- read_rds("white_voters.rds")
hisp_voters <- read_rds("hisp_voters.rds")
other_voters <- read_rds("other_voters.rds")
all_voters <- read_rds("all_voters.rds")

# I globally set education choices for clarity and ease when/if I need to edit the choices. Thanks to Albert for this good tip.
# I don't want to user to see/select options that are the same as the variable names I need later. As a result, I set what user sees 
# and then set that equal to the variable name that I'll need later

education_choices <- c("Postgraduate" = "postgrad",
  "College" = "college_grad",
  "Some College" = "some_college", 
  "High School (or Less)" = "hs")

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
      
      # I have two selectInputs: one for education, and one for race.
      # The choices, which is the list of values to select from, are set equal lists created earlier.
      # Here the user selects with a drop down menu. I could have also had different tabs per ethnicity, but this was
      # more seamless. 
      
       selectInput(inputId = "educ",
                   label = "Education:",
                   choices = education_choices,
                   selected = "black_postgrad"),
       selectInput(inputId = "race",
                   label = "Race:",
                   choices = race_choices,
                   selected = "black_postgrad"),
      
      # I have two checkboxInputs: one for the best fit line and the other for district labels.
      # When the user clicks on either checkbox, it is set equal to true and calculations done later on alter the graph shown.
    
      
      checkboxInput(inputId = "line", 
                    label = "Show Best Fit Line", 
                    value = FALSE),
      
      checkboxInput(inputId = "district", 
                    label = "Show District Labels", 
                    value = FALSE)
      
    ),
    
    # Show a plot of the generated distribution
    # Here, I provide a summary of the graphs shown based on my limited understanding of data analysis.
    
    mainPanel(
      plotOutput("distPlot"), 
      br(),
      h3("Summary:"),
      h4("Asian:"),
      p("[will paste from google docs later]"),
      h4("Black:"),
      p("[will paste from google docs later]"),
      h4("Hispanic:"),
      p("[will paste from google docs later]"),
      h4("White:"),
      p("[will paste from google docs later]"),
      h4("Other:"),
      p("[will paste from google docs later]"),
      h4("Final Thoughts:"),
      p("[will paste from google docs later]")
      
    )
  )
)
  
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
  
    # If the user selects this ethnicity, the code below will run. I could have streamlined this into one table (all_voters)
    # but I had some trouble with concantenating strings and setting them to aesthetic variables. 
    # I need to use aes_string (thanks for your help on this, Nick) because the chosen values from the selectInput function
    # in the ui transmits a string to the server. That initially did not work to get the variable name but does with aes_string. An
    # important thing to remember here is to put quotes around the non-string variable (in this case, dem_error) because aes_string
    # won't be able to find it if you don't. 
    
    if(input$race == "black") {
       black_plot <- black_voters %>% 
        ggplot(aes_string(x = paste(input$race,input$educ, sep = "_"), y = "dem_error", color = "dem_error")) + 
         geom_point() + 
         ylab("Dem Error") + 
         xlab(c(paste(names(race_choices[which(race_choices == input$race)]), "+", 
                      names(education_choices[which(education_choices == input$educ)]), "(%)"))) +
         guides(color=guide_legend("Democrat Error"))
      
      # The data before before the plot is printed changes based on the checkboxInput in the UI
      # If the checkbox is selected, the input$inputId is set to TRUE and the plot is updated accordingly
      # For the plots, I used geom_label_repel which adds text to the plot; here, it's the district.
      # My best fit line code is based off of the code of Ms. Gayton and Mr. Arellano/Ms. Fridkin but different to fit 
      # my different usage/code. 
      # The district labels pulls inspiration from the work of Mr. Schroeder (ever helpful) and Mr. Cordeiro
      
      if (input$line == TRUE) {
        black_plot <- black_plot + geom_smooth(method = lm, se = FALSE)
      }
       
       if (input$district == TRUE) {
         black_plot <- black_plot + geom_label_repel(aes(label = toupper(district)), size = 3, force = 3)
      }
      
      black_plot
    }
    
    # If the user selects this ethnicity, the code below will run. I could have streamlined this into one table (all_voters)
    # but I had some trouble with concantenating strings and setting them to aesthetic variables. 
    # I need to use aes_string (thanks for your help on this, Nick) because the chosen values from the selectInput function
    # in the ui transmits a string to the server. That initially did not work to get the variable name but does with aes_string. An
    # important thing to remember here is to put quotes around the non-string variable (in this case, dem_error) because aes_string
    # won't be able to find it if you don't. 
    
    else if(input$race == "asian") {
      asian_plot <- asian_voters %>% 
        #filter(!is.na(input$type)) %>% 
        ggplot(aes_string(x = paste(input$race,input$educ, sep = "_"), y = "dem_error", color = "dem_error")) + geom_point() + 
        ylab("Dem Error") + 
        xlab(c(paste(names(race_choices[which(race_choices == input$race)]), "with", 
                     names(education_choices[which(education_choices == input$educ)]), "Education (%)"))) +
        guides(color=guide_legend("Democrat Error"))
      
      # The data before before the plot is printed changes based on the checkboxInput in the UI
      # If the checkbox is selected, the input$inputId is set to TRUE and the plot is updated accordingly
      # For the plots, I used geom_label_repel which adds text to the plot; here, it's the district.
      # My best fit line code is based off of the code of Ms. Gayton and Mr. Arellano/Ms. Fridkin but different to fit 
      # my different usage/code. 
      # The district labels pulls inspiration from the work of Mr. Schroeder (ever helpful) and Mr. Cordeiro
      
      if (input$line == TRUE) {
        asian_plot <- asian_plot + geom_smooth(method = lm, se = FALSE)
      }
      
      if (input$district == TRUE) {
        asian_plot <- asian_plot + geom_label_repel(aes(label = toupper(district)), size = 3, force = 3)
      }
      
      
      asian_plot
      
    }
    
    # If the user selects this ethnicity, the code below will run. I could have streamlined this into one table (all_voters)
    # but I had some trouble with concantenating strings and setting them to aesthetic variables. 
    # I need to use aes_string (thanks for your help on this, Nick) because the chosen values from the selectInput function
    # in the ui transmits a string to the server. That initially did not work to get the variable name but does with aes_string. An
    # important thing to remember here is to put quotes around the non-string variable (in this case, dem_error) because aes_string
    # won't be able to find it if you don't. 
    
    else if(input$race == "hispanic") {
      hisp_plot <- hisp_voters %>% 
        #filter(!is.na(input$type)) %>% 
        ggplot(aes_string(x = paste(input$race,input$educ, sep = "_"), y = "dem_error", color = "dem_error")) + geom_point() + 
        ylab("Dem Error") + 
        xlab(c(paste(names(race_choices[which(race_choices == input$race)]), "with", 
                     names(education_choices[which(education_choices == input$educ)]), "Education (%)"))) +
        guides(color=guide_legend("Democrat Error"))
      
      # The data before before the plot is printed changes based on the checkboxInput in the UI
      # If the checkbox is selected, the input$inputId is set to TRUE and the plot is updated accordingly
      # For the plots, I used geom_label_repel which adds text to the plot; here, it's the district.
      # My best fit line code is based off of the code of Ms. Gayton and Mr. Arellano/Ms. Fridkin but different to fit 
      # my different usage/code. 
      # The district labels pulls inspiration from the work of Mr. Schroeder (ever helpful) and Mr. Cordeiro
      
      if (input$line == TRUE) {
        hisp_plot <- hisp_plot + geom_smooth(method = lm, se = FALSE)
      }
      
      if (input$district == TRUE) {
        hisp_plot <- hisp_plot + geom_label_repel(aes(label = toupper(district)), size = 3, force = 3)
      }
      
      hisp_plot
    }
    
    # If the user selects this ethnicity, the code below will run. I could have streamlined this into one table (all_voters)
    # but I had some trouble with concantenating strings and setting them to aesthetic variables. 
    # I need to use aes_string (thanks for your help on this, Nick) because the chosen values from the selectInput function
    # in the ui transmits a string to the server. That initially did not work to get the variable name but does with aes_string. An
    # important thing to remember here is to put quotes around the non-string variable (in this case, dem_error) because aes_string
    # won't be able to find it if you don't. 
    
    else if(input$race == "white") {
      white_plot <- white_voters %>% 
        #filter(!is.na(input$type)) %>% 
        ggplot(aes_string(x = paste(input$race,input$educ, sep = "_"), y = "dem_error", color = "dem_error")) + geom_point() + 
        ylab("Dem Error") + 
        xlab(c(paste(names(race_choices[which(race_choices == input$race)]), "with", 
                     names(education_choices[which(education_choices == input$educ)]), "Education (%)")))+
        guides(color=guide_legend("Democrat Error"))
      
      # The data before before the plot is printed changes based on the checkboxInput in the UI
      # If the checkbox is selected, the input$inputId is set to TRUE and the plot is updated accordingly
      # For the plots, I used geom_label_repel which adds text to the plot; here, it's the district.
      # My best fit line code is based off of the code of Ms. Gayton and Mr. Arellano/Ms. Fridkin but different to fit 
      # my different usage/code. 
      # The district labels pulls inspiration from the work of Mr. Schroeder (ever helpful) and Mr. Cordeiro
      
      if (input$line == TRUE) {
        white_plot <- white_plot + geom_smooth(method = lm, se = FALSE)
      }
      
      if (input$district == TRUE) {
        white_plot <- white_plot + geom_label_repel(aes(label = toupper(district)), size = 3, force = 3)
      }
      
      white_plot
    }
  
    # If the user selects this ethnicity, the code below will run. I could have streamlined this into one table (all_voters)
    # but I had some trouble with concantenating strings and setting them to aesthetic variables. 
    # I need to use aes_string (thanks for your help on this, Nick) because the chosen values from the selectInput function
    # in the ui transmits a string to the server. That initially did not work to get the variable name but does with aes_string. An
    # important thing to remember here is to put quotes around the non-string variable (in this case, dem_error) because aes_string
    # won't be able to find it if you don't. 
  
      else if(input$race == "other") {
      other_plot <- other_voters %>% 
        #filter(!is.na(input$type)) %>% 
        ggplot(aes_string(x = paste(input$race,input$educ, sep = "_"), y = "dem_error", color = "dem_error")) + 
        geom_point() + 
        ylab("Dem Error") + 
        xlab(c(paste(names(race_choices[which(race_choices == input$race)]), "with", 
             names(education_choices[which(education_choices == input$educ)]), "Education (%)"))) +
        guides(color=guide_legend("Democrat Error"))
      
      # The data before before the plot is printed changes based on the checkboxInput in the UI
      # If the checkbox is selected, the input$inputId is set to TRUE and the plot is updated accordingly
      # For the plots, I used geom_label_repel which adds text to the plot; here, it's the district.
      # My best fit line code is based off of the code of Ms. Gayton and Mr. Arellano/Ms. Fridkin but different to fit 
      # my different usage/code. 
      # The district labels pulls inspiration from the work of Mr. Schroeder (ever helpful) and Mr. Cordeiro
      
      if (input$line == TRUE) {
        other_plot <- other_plot + geom_smooth(method = lm, se = FALSE)
      }
      
      if (input$district == TRUE) {
        other_plot <- other_plot + geom_label_repel(aes(label = toupper(district)), size = 3, force = 3)
      }
      
      other_plot
    }
    
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)