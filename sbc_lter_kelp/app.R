library(shiny)
library(tidyverse)
library(bslib)
library(tmap)
library(sf)
library(here)
library(janitor)
library(thematic)
library(plotly)

## DATA 
#App fails unless this data set is included here 

# Map data first:

santa_cruz_kelp <- read_csv(here("data", "santa_cruz_kelp_2021.csv"))

#This is a subset I made of only Santa Cruz island for 2021 

# try to make a sf and fixed map from this 

santa_cruz_kelp_sf <- santa_cruz_kelp %>% 
  st_as_sf(coords = c('lon', 'lat'))

st_crs(santa_cruz_kelp_sf) <- 4326

#End subset map data

#Start other tabs data

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

# Biodiversity Data
## Fish:
fish <- read_csv(here("data", "fish_abund.csv"))

fish_clean <- fish %>%
  clean_names() %>%
  select(year, site, sp_code, count, scientific_name, common_name, 13:21) %>%
  mutate(across(c(1:13), na_if, -99999))

fish_sub <- fish_clean %>%
  group_by(year, site, common_name) %>%
  summarise(total_count = sum(count)) %>%
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
                          site == 'GOLB' ~ 'Goleta Bay'))

## Invert:
inverts <- read_csv(here("data", "inverts_abund.csv"))

inverts_clean <- inverts %>%
  clean_names() %>%
  select(year, site, sp_code, count, scientific_name, common_name, 13:20) %>%
  mutate(across(c(1:13), na_if, -99999))

inverts_sub <- inverts_clean %>%
  group_by(year, site, common_name) %>%
  summarise(total_count = sum(count)) %>%
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
                          site == 'GOLB' ~ 'Goleta Bay'))

## Join both fish and inverts together into one:
biodiversity <- fish_sub %>% full_join(inverts_sub)

total_bio_subset <- biodiversity %>%
  group_by(common_name, year) %>%
  summarise(total_count = sum(total_count))

total_bio_subset$site <- "All Sites"

# end Biodiversity data

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
                       column(10,
                              strong("This app seeks to visualize the key factors influencing kelp forest health in Santa Barbara. It includes the following tabs:"),
                              br(),
                              br(),
                              h3("Kelp Canopy:"), 
                              "This is a visualization of quarterly kelp biomass estimates since 1984. Data is collected based on satellite imaging estimating giant kelp canopy biomass. Biomass data (wet weight, kg) are given for individual 30 x 30 meter pixels in the coastal areas extending from near Ano Nuevo, CA through the southern range limit in Baja California (including offshore islands), representing the range where giant kelp is the dominant canopy forming species.",
                              br(),
                              br(),
                              strong("Data Citation:"),
                              em("Bell, T, K. Cavanaugh, D. Siegel. 2022. SBC LTER: Time series of quarterly NetCDF files of kelp biomass in the canopy from Landsat 5, 7 and 8, since 1984 (ongoing) ver 15. Environmental Data Initiative."),
                              tags$a(href="https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.74", 
                                     "Link"),
                              br(),
                              br(),
                              h3("Abiotic Factors:"), 
                              "This combines several SBC LTER datasets to visualize kelp abundance across the region and at specific sites compared to abiotic factors influencing productivity. Selected factors include temperature, nitrate concentrations, and wave height.",
                              br(),
                              br(),
                              strong("Data Citations:"),
                              br(),
                              strong("Kelp Anundance at LTER Sites:"),
                              em("Reed, D, R. Miller. 2022. SBC LTER: Reef: Kelp Forest Community Dynamics: Abundance and size of Giant Kelp (Macrocystis Pyrifera), ongoing since 2000 ver 25. Environmental Data Initiative."),
                              tags$a(href="https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.18", 
                                     "Link"),
                              br(),
                              br(),
                              strong("Temperature at LTER Sites:"),
                              em("Reed, D, R. Miller. 2022. SBC LTER: Reef: Bottom Temperature: Continuous water temperature, ongoing since 2000 ver 26. Environmental Data Initiative."),
                              tags$a(href="https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.13", 
                                     "Link"),
                              br(),
                              br(),
                              strong("Abiotic Factors Regionally:"),
                              em("Bell, T, K. Cavanaugh, D. Reuman, M. Castorani, L. Sheppard, J. Walter. 2021. SBC LTER: REEF: Macrocystis pyrifera biomass and environmental drivers in southern and central California ver 1. Environmental Data Initiative."),
                              tags$a(href="https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.144", 
                                     "Link"),
                              br(),
                              br(),
                              h3("Kelp Forest Community:"), 
                              "This combines several additional SBC LTER datasets to visualize abundance of species dependent on kelp across the region and at specific sites compared to overall kelp forest health. Selected species include .... PANEL 3 TBD.",
                              br(),
                              br(),
                              strong("Data Citations:"),
                              br(),
                              strong("Fish Abundance at LTER Sites:"),
                              em("Reed, D, R. Miller. 2022. SBC LTER: Reef: Kelp Forest Community Dynamics: Fish abundance ver 36. Environmental Data Initiative."),
                              tags$a(href="https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.17", 
                                     "Link"),
                              br(),
                              br(),
                              strong("Invertebrate Abundance at LTER Sties:"),
                              em("Reed, D, R. Miller. 2022. SBC LTER: Reef: Kelp Forest Community Dynamics: Invertebrate and algal density ver 28. Environmental Data Initiative."),
                              tags$a(href="https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.19", 
                                     "Link")
                              ),
                       column(2,
                            img(src = 'garbaldi.jpg', 
                                     height =200, width = 300))
                       ) # end fluidRow 1
                       )# end main panel 1
                       ), # End tab panel 1
            tabPanel("Kelp Canpoy", # start tab panel 2
                     mainPanel(
                       tmapOutput("tmap_kelp_sc")
                       )# end main panel 2
                     ), # end tab panel 2
            tabPanel("Abiotic Factors", #start panel 3
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
                                                      "Nitrate Concentration",
                                                      "Wave Height"),
                                                    selected = "Abundance by Site"), #trying multiple options, end select
                                        plotOutput('whichplot'),
                                        sliderInput("year_selector", "Select Year Range",
                                                    min = min(kelp_factors_sub$year),
                                                    max = max(kelp_factors_sub$year),
                                                    value = c(1987,2019),
                                                    step = 1,
                                                    sep = ""
                                                    )
                                      ) # end main panel 2
                                      ) #end sidebar layout 2
                      ), # end tabpanel 3
            
            tabPanel("Kelp Forest Community", # Start panel 4
                     sidebarLayout(# Adding sidebar selector for factors
                       sidebarPanel(
                         
                           selectInput(inputId = "pick_species",
                                            label = "Choose Species: \n (to remove species selection, click and press 'delete')",
                                            choices = unique(biodiversity$common_name),
                                     selected = "Aggregating anemone",
                                     multiple = TRUE), # end selectInput
                           
                         # input selector panels for second graph
                         selectInput(inputId = "pick_location",
                                     label = "Choose Site:",
                                     choices = unique(biodiversity$site),
                                     selected = "Naples",
                                     multiple = FALSE
                         ), # end selectInput
                         ),# end sidebarPanel

                       mainPanel(
                         
                         h3("Explore species counts by year and site:"),
                        
                         sliderInput("biodiversity_year_selector", "Select Year:",
                                     min = min(biodiversity$year),
                                     max = max(biodiversity$year),
                                     value = 2021,
                                     step = 1,
                                     sep = ""), # end slider input
                         
                         plotOutput('biodiversityplot'),
                      
                         plotOutput('timeseries'),
                         
                         h3("Explore total species counts over all sites:"),
                         
                         selectInput(inputId = "species",
                                     label = "Choose Species:",
                                     choices = unique(total_bio_subset$common_name),
                                     selected = "Aggregating anemone",
                                     multiple = TRUE), # end selectInput
                                     
                         plotOutput('statictotals')
                      
                       ) # end mainPanel
                     ) # end sidebarLayout
                     ) # end tab panel 4
                     ) #end navbarPage
                    )# end ui

# Create server object 

server <- function(input, output) { 
  
# Need data frames here !!! 
  
# Data for panel 1
kelp_raw_sites <- read_csv(here('data', 'kelp_no3_waves.csv'))
sites <- read_csv(here('data', 'site_locations.csv'))
combined_kelp <- merge(sites, kelp_raw_sites, by = "site_id")  
 
combined_kelp_sb <- combined_kelp %>% 
  filter(site_id %in% c(267:298)) %>% 
  group_by(site_id)


# End Panel 1 Data 

# Additional panel 2 data

coeff <- 10^7
#This is the best scaling factor for the nitrate and wave graph after trying a few 

# Function for LTER Site Kelp Surveys 
  
  abund_reactive <- reactive({
    kelp_abund_sub %>%
      filter(site %in% input$pick_site)
  }) # end abund_reactive
  
  factors_reactive <- reactive({
    kelp_factors_sub %>% 
      filter(year %in% input$year_selector[1]:input$year_selector[2])
  })
  # output for date slider
  
  # Functions for Kelp Biodiversity  
  
  ## Biodiversity Plot: 
  bio_reactive <- reactive({
    biodiversity %>% 
      filter(common_name %in% input$pick_species) %>%
      filter(year == input$biodiversity_year_selector)
  }) # end plot
  
  ## Timeseries Plot:
  time_reactive <- reactive({
    biodiversity %>%
      filter(common_name %in% input$pick_species) %>%
      filter(site == input$pick_location)
  }) # end plot

  ## Static Plot:
  total_reactive <- reactive({
    total_bio_subset %>%
      filter(common_name %in% input$species)
  }) # end plot
  
  # Output for tmap kelp plot 
  
  output$tmap_kelp_sc <- renderTmap({
      tm_shape(santa_cruz_kelp_sf) +
      tm_legend(title = "Santa Cruz Kelp Biomass in 2021 (kg)") +
      tm_dots('biomass', palette = 'BuGn')
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
    plot = ggplot(data = factors_reactive(), 
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
      theme_minimal()
  } # end second option (nitrate)
  
  if(input$plotnumber == "Wave Height"){
    plot = ggplot(data = factors_reactive(), 
                  aes(x = year, y = waves)) +
      # now integrate the wave curve with the kelp
      geom_smooth(color = "cadetblue3") + #now we need to scale the kelp axis  
      geom_col(aes(y = kelp/coeff), fill = "darkseagreen", alpha = 0.7) +
      scale_y_continuous(
        # Features of the first axis
        name = "Average Wave Height (m)",
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name=" Kelp Biomass (kg)")
      ) +
      theme_minimal()
  }
  plot # call the option 
  }) # end this function for selecting factor graphs 

#Biodiversity plot

## Species/Location Plot
output$biodiversityplot <- renderPlot({
  plot = ggplot(data = bio_reactive())+
    geom_col(aes(x = site, y = total_count, fill = common_name), position = "dodge") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45)) +
    labs(title = "Yearly Observations At All Sites", x = "Site", y = "\nCount\n")
  plot
  })

## Timeseries plot
output$timeseries <- renderPlot({
  plot = ggplot(data = time_reactive()) +
    geom_line(aes(x = year, y = total_count, color = common_name))+
    labs(title = "Site-Specific Time Series", x = "Year", y = "Count")
  plot
})

## Static Totals
output$statictotals <- renderPlot({
  plot = ggplot(data = total_reactive()) +
    geom_line(aes(x = year, y = total_count, color = common_name)) +
    labs(title = "Aggretgate Counts Over All Sites",
         x = "Year",
         y = "Count")
  plot
})
} # end all sever 

thematic_shiny()

# Run the application 
shinyApp(ui = ui, server = server)
