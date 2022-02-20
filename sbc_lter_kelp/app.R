library(shiny)
library(tidyverse)
library(bslib)
library(tmap)

# Set up a custom theme 

my_theme <- bs_theme(
  bg = "#F0FFF0",
  fg = 'black',
  primary = 'black',
  base_fonts = font_google('Poppins')
)

# Define UI for application
ui <- fluidPage(
  theme = my_theme,
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
            tabPanel("Kelp Cover Over Time", # start tab panel 2
                     mainPanel(
                       tmapOutput("tmap_kelp")
                       )# end main panel 2
                     ), # end tab panel 2
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
                      ), # end tabpanel 3
            
          
            tabPanel("Kelp Forest Community",
                     sidebarLayout( # Adding sidevar selector for factors
                       sidebarPanel(
                         checkboxGroupInput(inputId = "pick_species",
                                            label = "Choose Species",
                                            choices = unique(fish_sub$sp                       ))) # end main panel 4
                     ) # end tab panel 4
                     ) # end navbarPage
                     ) # end ui

# Create server object 

server <- function(input, output) { # 
  
  # Function for LTER Site Kelp Surveys 
  
  abund_reactive <- reactive({
    kelp_abund_sub %>%
      filter(site %in% input$pick_site)
  }) # end abund_reactive
  
  # Output for tmap kelp plot 
  
  output$tmap_kelp <- renderTmap({
    tm_shape(kelp_sb_sf) +
      tm_dots()
  })
  
  # Output for LTER Site Kelp Surveys 
  
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
