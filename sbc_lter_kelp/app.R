library(shiny)
library(tidyverse)
library(bslib)

# Read in the raw data 

#kelp_factors <- read_csv

# Define UI for application
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
        navbarPage("Dynamics of Santa Barbara Kelp Forests",
            tabPanel("Project Overview", # Setting project description page
                     mainPanel(
                       fluidRow(
                       column(6,
                              "This app seeks to visualize the key factors influencing 
                               kelp forest health in Santa Barbara"),
                       column(5,
                            img(src = 'garbaldi.jpg', 
                                     height =200, width = 300))
                       ) # end fluidRow 1
                       )# end main panel 1
                       ), # End tab panel 1
            
            tabPanel("Factors Influencing Kelp Productivity", #start panel 2
                      sidebarLayout(# Adding sidebar selector for factors
                                    sidebarPanel(
                                        checkboxGroupInput(inputId = "pick_site",
                                                           label = "Choose Site:",
                                                           choices = unique(kelp_abund_sub$site) # drawing sites as filter option
                                                   ) # end checkboxGroupInput
                                      ), # end sidebarPanel
                                    
                                      mainPanel(
                                                plotOutput("abund_plot") # placeholder
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

# Create server object 

server <- function(input, output) { # placeholder from lab
  abund_reactive <- reactive({
    kelp_abund_sub %>%
      filter(site %in% input$pick_site)
  }) # end abund_reactive
  
  output$abund_plot <- renderPlot(
    ggplot(data = abund_reactive(), 
           aes(x = year, y = fronds)) +
      geom_col(aes(fill = site)) +
      theme_minimal() +
      labs(title = "Kelp Abundance Over Time", 
           x = "Year", y = "Kelp Fronds (number > 1m)")) # end output$abund_plot
      }

# Run the application 
shinyApp(ui = ui, server = server)
