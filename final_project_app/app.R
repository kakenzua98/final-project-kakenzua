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
library(datasets)


census_2014 <- read_rds("census_2014.rds")
hate_crimes <- read_rds("hate_crimes.rds")
lpr_dest <- read_rds("lpr_dest.rds")
unemployment <- read_rds("unemployment.rds")
unemployment <- read_rds("unemployment.rds")
demo_all <- read_rds("demo_all.rds")


# Define UI for application that draws a histogram
state_choices <- census$state
analysis_choices <- c("Gini Index" = "gini_index",
                      "Percentage of High School Graduates" = "share_population_with_high_school_degree",
                      "Median Household Income" = "median_household_income",
                      "Percentage of Hate Crimes" = "hate_crimes_per_100k_splc", 
                      "Share of Pop in Metro Areas" = "share_population_in_metro_areas")

ui <- fluidPage(
   
   # Application title
   titlePanel("Correlations with Legal Permanent Residents"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "y_var",
                    label = "Compare With:",
                    choices = analysis_choices,
                    selected = "Gini Index"),
        
        checkboxInput(inputId = "line", 
                      label = "Show Best Fit Line", 
                      value = FALSE),
        
        checkboxInput(inputId = "district", 
                      label = "Show District Labels", 
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
  
     first_plot <- demo_all %>% 
       #mutate(state = state.abb[match(demo_all,state.name)])
       ggplot(aes_string(x = "avg", y = input$y_var)) + 
       geom_point() 
     
     if (input$line == TRUE) {
       first_plot <- first_plot + geom_smooth(method = lm, se = FALSE)
     }
     
     if (input$district == TRUE) {
       first_plot <- first_plot + geom_label_repel(aes(label = toupper(state)), size = 3, force = 3)
     }
     
     first_plot
    
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

