library(shiny)
library(tidyverse)
library(bslib)
library(tmap)
library(sf)
library(here)
library(janitor)
library(thematic)

## DATA 
#App fails unless this data set is included here 

kelp_factors <- read_csv(here("data", "kelp_no3_waves.csv"))
kelp_abund <- read_csv(here('data', 'annual_kelp.csv'))

kelp_abund_sub <- kelp_abund %>%
  clean_names() %>% 
  mutate(site = case_when(site == 'CARP' ~ 'Carpinteria',
                          site == 'NAPL' ~ 'Naples',
                          site == 'MOHK' ~ 'Mohawk',
                          site == 'IVEE' ~ 'Isla Vista',
                          site == 'AQUE' ~ 'Arroyo Quemado',
                          site == 'ABUR' ~ 'Arroyo Burro',
                          site == 'AHND' ~ 'Arroyo Hondo',
                          site == 'SCTW' ~ 'Santa Cruz - Harbor',
                          site == 'SCDI' ~ 'Santa Cruz - Diablo',
                          site == 'BULL' ~ 'Bulito',
                          site == 'GOLB' ~ 'Goleta Bay')) %>% 
  group_by(site) %>% 
  na_if(-99999) %>% 
  drop_na()

kelp_factors_sub <- kelp_factors %>% 
  filter(site_id %in% c(267:298)) %>% 
  group_by(site_id, year)

# Set up a custom theme 

my_theme <- bs_theme(
  bg = "#F0FFF0",
  fg = 'black',
  primary = 'black',
  base_fonts = font_google('Poppins'),
  version = 4, bootswatch = "sandstone")


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
            tabPanel("Factors Influencing Kelp Productivity", #start panel 3
                     sidebarLayout(# Adding sidebar selector for factors
                                    sidebarPanel(
                                        checkboxGroupInput(inputId = "pick_site",
                                                           label = "Choose Site:",
                                                           choices = unique(kelp_abund_sub$site),
                                                           selected = "Naples" # This is what is selected automatically
                                                   ) # end checkboxGroupInput
                                      ), # end sidebarPanel
                                    
                                      mainPanel(
                                        selectInput("plotnumber", "Select Plot:",
                                                    c("Abundance by Site",
                                                      "Nitrate Concentration"),
                                                    selected = "Abundance by Site"), #trying multiple options, end select
                                                plotOutput('whichplot')
                                      ) # end main panel 2
                                      ) #end sidebar layout 2
                      ), # end tabpanel 3
            
          
            tabPanel("Kelp Forest Community",
                     mainPanel("Fill in Output") # end main panel 4
                     ) # end tab panel 4
                     ) # end navbarPage
                     ) # end ui

# Create server object 

server <- function(input, output) { # 
  
# Need data frames here !!! 
  
# Data for panel 1
kelp_raw_sites <- read_csv(here('data', 'kelp_no3_waves.csv'))
sites <- read_csv(here('data', 'site_locations.csv'))
combined_kelp <- merge(sites, kelp_raw_sites, by = "site_id")  
 
combined_kelp_sb <- combined_kelp %>% 
  filter(site_id %in% c(267:298)) %>% 
  group_by(site_id)

kelp_sb_sf <- combined_kelp_sb %>% 
  st_as_sf(coords = c('lon', 'lat'))

# End Panel 1 Data 

# Additional panel 2 data

coeff <- 10^7
#This is the best scaling factor for the nitrate graph after trying a few 

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
  
output$whichplot <- renderPlot({
  if(input$plotnumber == "Abundance by Site"){
    plot = ggplot(data = abund_reactive(), 
           aes(x = year, y = fronds)) +
      geom_col(aes(fill = site)) +
      theme_minimal() +
      labs(title = "Kelp Abundance Over Time", 
           x = "Year", y = "Kelp Fronds (number > 1m)")} # end abund_plot option
  if(input$plotnumber == "Nitrate Concentration"){
    plot = ggplot(data = kelp_factors_sub, 
           aes(x = year, y = no3)) +
      # now integrate the nitrogen curve with the kelp
      geom_smooth(color = "coral") + #now we need to scale the kelp axis  
      geom_col(aes(y = kelp/coeff), fill = "darkseagreen", alpha = 0.7) +
      scale_y_continuous(
        # Features of the first axis
        name = "NO3 Concentration (uM/L)",
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name=" Kelp Biomass (kg)")
      ) +
      theme_bw()
  } # end second option 
  plot # call the option 
  }) # end this function for selecting factor graphs 
  
} # end all sever 

thematic_shiny()

# Run the application 
shinyApp(ui = ui, server = server)
