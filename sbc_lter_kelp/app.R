library(shiny)
library(tidyverse)
library(bslib)


# Read in the raw data 

#kelp_factors <- read_csv

# Define UI for application that draws a histogram
ui <- fluidPage(
        navbarPage("Dynamics of Santa Barbara Kelp Forests",
            tabPanel("Project Overview", # Setting project description page
                     mainPanel("Fill in Description")
                     ), # End tab panel 1
            
            tabPanel("Factors Influencing Kelp Productivity", #start panel 2
                      sidebarLayout(# Adding sidebar selector for factors
                                    sidebarPanel("WIDGETS",
                                        checkboxGroupInput(inputId = "pick_indicator",
                                                           label = "Choose Indicator:",
                                                           choices = unique(starwars$species) # placeholder
                                                   ) # end checkboxGroupInput
                                      ), # end sidebarPanel
                                    
                                      mainPanel("OUTPUT!",
                                                plotOutput("sw_plot") # placeholder
                                      ) # end main panel 1 
                                      ) #end sidebar layout 2
                      ), # end tabpanel 2
            
            tabPanel("Kelp Cover Over Time",
                     mainPanel("Fill in Output") # end main panel 3
                     ), # end tab panel 3
            tabPanel("Kelp Forest Community",
                     mainPanel("Fill in Output") # end main panel 4
                     ) # end tab panel 4
                     ) # end navbarPage
                     ) # end ui

server <- function(input, output) { # placeholder from lab
  sw_reactive <- reactive({
    starwars %>%
      filter(species %in% input$pick_species)
  }) # end sw_reactive
  
  output$sw_plot <- renderPlot(
    ggplot(data = sw_reactive(), aes(x = mass, y = height)) +
      geom_point(aes(color = species))
  ) # end output$sw_plot
      }

# Run the application 
shinyApp(ui = ui, server = server)
