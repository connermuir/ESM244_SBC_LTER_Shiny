library(shiny)
library(tidyverse)
library(bslib)


# Read in the raw data 

#kelp_factors <- read_csv

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Dynamics of Santa Barbara Kelp Forests"),
  theme = 'kelp.css',
                navbarPage(
                           tabPanel(
                             "Factors Influencing Kelp Productivity", #start panel 1
                                    sidebarLayout(
                                      sidebarPanel("WIDGETS",
                                                   checkboxGroupInput(inputId = "pick_indicator",
                                                                      label = "Choose Indicator:",
                                                                      choices = unique(starwars$species)
                                                   ) # end checkboxGroupInput
                                      ), # end sidebarPanel
                                      
                                      mainPanel("Fill in Output",
                                                )
   
                                      ), # end tabPanel 1
                                      
                                      tabPanel("Kelp Cover Over Time"),
                                      tabPanel("Kelp Forest Community")
                                    ) # end navbarPage
                           ) # end ui



# Run the application 
shinyApp(ui = ui, server = server)
