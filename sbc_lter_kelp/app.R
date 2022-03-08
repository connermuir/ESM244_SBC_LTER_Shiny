library(shiny)
library(tidyverse)
library(bslib)
library(tmap)
library(sf)
library(here)
library(janitor)
library(thematic)
library(plotly)
library(cowplot)


########
## DATA 

# ALL MAP DATA GOES HERE 

#These are 3 subsets for SR island 

#first 2010
santa_rosa_kelp_2010 <- read_csv(here("data", "santa_cruz_kelp_2010.csv"))

santa_rosa_kelp_2010_sf <- santa_rosa_kelp_2010 %>% 
  st_as_sf(coords = c('lon', 'lat'))
st_crs(santa_rosa_kelp_2010_sf) <- 4326

# then 2015
santa_rosa_kelp_2015 <- read_csv(here("data", "santa_cruz_kelp_2015.csv"))

santa_rosa_kelp_2015_sf <- santa_rosa_kelp_2015 %>% 
  st_as_sf(coords = c('lon', 'lat'))
st_crs(santa_rosa_kelp_2015_sf) <- 4326

#then 2020
santa_rosa_kelp_2020 <- read_csv(here("data", "santa_cruz_kelp_2020.csv"))

santa_rosa_kelp_2020_sf <- santa_rosa_kelp_2020 %>% 
  st_as_sf(coords = c('lon', 'lat'))
st_crs(santa_rosa_kelp_2020_sf) <- 4326

# END MAP DATA 

#########
# ALL ABIOTIC FACTORS DATA GOES HERE 

temp_day_sub <- read_csv(here("data", "daily_avg_temp.csv"))
kelp_density <- read_csv(here('data', 'Annual_All_Species_Biomass_at_transect_20210108.csv'))

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

kelp_density_sub <- kelp_density %>%
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
  filter(coarse_grouping == "GIANT KELP") %>%
  group_by(site, date) %>% 
  na_if(-99999) %>% 
  summarise(density=mean(density,na.rm=T)) %>% 
  ungroup()

site_list <- unique(kelp_density_sub$site)

kelp_factors_sub <- kelp_factors %>% 
  filter(site_id %in% c(267:298)) %>% 
  group_by(site_id, year)

kelp_raw_sites <- read_csv(here('data', 'kelp_no3_waves.csv'))
sites <- read_csv(here('data', 'site_locations.csv'))
combined_kelp <- merge(sites, kelp_raw_sites, by = "site_id")  

combined_kelp_sb <- combined_kelp %>% 
  filter(site_id %in% c(267:298)) %>% 
  group_by(site_id)

#########
# ALL COMMUNITY DATA GOES HERE 
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

## Kelp Totals/Site for biodiversity tab
kelp_bio <- kelp_abund_sub %>%
  select(year, site, fronds) %>%
  group_by(year, site) %>%
  summarize(total_fronds = sum(fronds))

total_all <- plyr::join(kelp_bio, biodiversity, by= c("site","year"), type = "full")

# end Biodiversity data

# END ALL DATA 

##########
# THEME GOES HERE 

my_theme <- bs_theme(
  bg = "#F0FFF0",
  fg = 'black',
  primary = 'black',
  base_fonts = font_google('Poppins'),
  version = 4, bootswatch = "sandstone")

############
# Define UI for application
ui <- fluidPage(
  theme = my_theme,
        navbarPage("Dynamics of Santa Barbara Kelp Forests",
            
# START OPENING OVERVIEW TAB PANEL  

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
                      ), #start picture column (SHOULD ADD MORE)
            column(2,
              img(src = 'garbaldi.jpg', height =200, width = 300))
                       ) # end fluidRow 1
                       )# end main panel 1
                       ), # END TAB PANEL 1

# START MAPPING PANEL     
      tabPanel("Kelp Canpoy", # start tab panel 2
        mainPanel(
        selectInput("mapyear", "Select Year:",
                    c("2010", "2015","2020"), selected = "2020"),
                  tmapOutput('whichmap')
                       )# end main panel 2
                     ), # end tab panel 2

# START ABIOTIC FACTORS PANEL 

      tabPanel("Abiotic Factors", #start panel 3
          sidebarLayout(# Adding sidebar selector for factors
                        sidebarPanel(
                          conditionalPanel( #Start conditional widgets here 
                          condition = "input.plotnumber == 'Abundance by Site'", 
                          checkboxGroupInput(inputId = "pick_site",
                                             label = "Choose Site:",
                                             choices = unique(kelp_abund_sub$site),
                                             selected = "Naples" # This is what is selected automatically
                                            ) # end checkboxGroupInput
                                           ), # end conditional widget (it works)
                                       #add another conditional 
                          conditionalPanel( #Start other conditional widgets here 
                          condition = "input.plotnumber == 'Density and Temperature by Site'", 
                          checkboxGroupInput(inputId = "pick_site_density",
                                             label = "Choose Site:",
                                             choices = unique(kelp_density_sub$site),
                                             selected = site_list) # This is what is selected automatically
                          ) # end other conditional
                        ), # end sidebar panel 
                 
                            mainPanel(
                              selectInput("plotnumber", "Select Plot:",
                                          c("Abundance by Site",
                                            "Density and Temperature by Site",
                                            "Nitrate Concentration (Regional)"
                                           ),
                                            selected = "Abundance by Site"), #trying multiple options, end select
                                plotOutput('whichplot'),
                               
                          
                              conditionalPanel( #Start other conditional widgets here 
                                condition = "input.plotnumber == 'Density and Temperature by Site'", 
                                plotOutput('temp_plot') 
                              ), # end other conditional
                              
                                conditionalPanel( #another conditional panel
                                  condition = 
                                    "input.plotnumber == 'Nitrate Concentration (Regional)'",
                                  sliderInput("year_selector", "Select Year Range",
                                            min = min(kelp_factors_sub$year),
                                            max = max(kelp_factors_sub$year),
                                            value = c(1987,2019),
                                            step = 1,
                                            sep = ""
                                                    )
                                      ) # End conditional panel
                                      ) # end main panel 2
                                      ) #end sidebar layout 2
                      ), # end tabpanel 3

# START COMMUNITY TAB
            
tabPanel("Kelp Forest Community", # Start panel 4
        sidebarLayout(# Adding sidebar selector for factors
        sidebarPanel(
              

        # input selector panel for species (graph 1/2)
        selectInput(inputId = "pick_species",
                    label = "Choose Species: \n (to remove species selection, click and press 'delete')",
                    choices = unique(biodiversity$common_name),
                    selected = "Aggregating anemone",
                    multiple = TRUE), # end selectInput
                           
          # input selector panel for location
          selectInput(inputId = "pick_location",
                      label = "Choose Site:",
                      choices = unique(biodiversity$site),
                      selected = "Naples",
                      multiple = FALSE),

                       # input selector panel for species (graph 1/2)
                           selectInput(inputId = "pick_species",
                                            label = "Choose Species: \n (to remove species selection, click and press 'delete')",
                                            choices = unique(total_all$common_name),
                                     selected = "Aggregating anemone",
                                     multiple = TRUE), # end selectInput
                           
                         # input selector panel for location
                         selectInput(inputId = "pick_location",
                                     label = "Choose Site:",
                                     choices = unique(total_all$site),
                                     selected = "Naples",
                                     multiple = FALSE

                         ), # end selectInput
                        
                        # kelp on/off switch (graph 1/2/3)
                           checkboxInput(
                             inputId = "kelp_viewer",
                             label = "Kelp", 
                             value = FALSE
                           ) # end kelp switch
                        
                         ), # end sidebarPanel

                       mainPanel(
                         
                         h3("Explore species counts by year and site:"),
                        
                         # year selector for first 2 graphs
                         sliderInput("biodiversity_year_selector", "Select Year:",
                                     min = min(total_all$year),
                                     max = max(total_all$year),
                                     value = 2021,
                                     step = 1,
                                     sep = ""), # end slider input
                         
                         plotOutput('biodiversityplot'),
                      
                         plotOutput('timeseries'),
                         
                         h3("Explore total species counts over all sites:"),
                         
                         # input selector for third graph 
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
                     ) # end ui


############
# SERVER CODE 

server <- function(input, output) { 
  
###########
# MAP PANEL OUTPUTS ALL GO HERE 
  
  output$whichmap <- renderTmap({
    if(input$mapyear == "2010"){
      map = tm_shape(santa_rosa_kelp_2010_sf) +
        tm_legend(title = "Santa Rosa Kelp Biomass in 2010 (kg)") +
        tm_dots('biomass', palette = 'BuGn')} # end 2010 option
    
    if(input$mapyear == "2015"){
     map = tm_shape(santa_rosa_kelp_2015_sf) +
        tm_legend(title = "Santa ROsa Kelp Biomass in 2015 (kg)") +
        tm_dots('biomass', palette = 'BuGn')} # end 2015 option 
    
    if(input$mapyear == "2020"){
      map = tm_shape(santa_rosa_kelp_2020_sf) +
        tm_legend(title = "Santa Rosa Kelp Biomass in 2020 (kg)") +
        tm_dots('biomass', palette = 'BuGn')
      } # end 2020 option
    map # call the option 
  }) # end this function for selecting year maps 
  
################  
# ABIOTIC FACTORS OUTPUTS ALL GO HERE  

coeff <- 10^7
#This is the best scaling factor for the nitrate and wave graph after trying a few 
 

  abund_reactive <- reactive({
    kelp_abund_sub %>%
      filter(site %in% input$pick_site)
  }) # end abund_reactive
  

  density_reactive <- reactive({
    kelp_density_sub %>%
      filter(site %in% input$pick_site_density)
  })
  
  temp_reactive <- reactive({
    kelp_density_sub %>%
      filter(site %in% input$pick_site_density)
  })
  
  factors_reactive <- reactive({
    kelp_factors_sub %>% 
      filter(year %in% input$year_selector[1]:input$year_selector[2])
  })
  # output for date slider
  
# Standalone temp plot 
  
output$temp_plot <- renderPlot({
    plot = ggplot(data = temp_day_sub, aes(x = date, y = day_avg_temp)) +
      geom_smooth(color = "coral") +
      labs(y = "Average Temperature - All Sites (Degrees Celsius)") +
      theme_minimal() 
    plot
  })
  
output$whichplot <- renderPlot({
  if(input$plotnumber == "Abundance by Site"){
    plot = ggplot(data = abund_reactive(), 
           aes(x = year, y = fronds)) +
      geom_col(aes(fill = site)) 
      theme_minimal() +
      labs(title = "Kelp Abundance Over Time", 
           x = "Year", y = "Kelp Fronds (number > 1m)")} # end abund_plot option
 
  if(input$plotnumber == "Density and Temperature by Site"){
    plot =  ggplot(data = density_reactive(), aes(x = date, y = density)) +
      geom_col(aes(fill = site), width = 50) +
      theme_minimal() +
      labs(y = "Average Density (Coverage Per Square Meter)")}
  
   if(input$plotnumber == "Nitrate Concentration (Regional)"){
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
  }
  plot # end second option (nitrate)
  }) # end this function for selecting factor graphs 



############
# COMMUNITY TAB OUTPUTS ALL GO HERE 

kelp_reactive <- reactive({
  kelp_bio %>%
    filter(fronds == input$kelp_selector)
})


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

## Species/Location Plot
output$biodiversityplot <- renderPlot({
  plot <- ggplot(data = bio_reactive()) +
    geom_col(aes(x = site, y = total_count, fill = common_name)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1)) +
    labs(title = "Yearly Observations At All Sites", x = "Site", y = "\nCount\n")
  plot
  })
 
## Timeseries plot
output$timeseries <- renderPlot({
  plot <- ggplot() +
    geom_line(data = time_reactive(), aes(x = year, y = total_count, color = common_name)) +
    theme_minimal() +
    labs(title = "Site-Specific Time Series", x = "Year", y = "Count") +
    scale_fill_manual("Species")
  
  if(input$kelp_viewer == TRUE) {
    plot <- plot +
      geom_line(aes(x = year, y = total_fronds, color = total_fronds)) 
  }
  plot
})

## Static Totals
output$statictotals <- renderPlot({
  plot = ggplot(data = total_reactive()) +
    geom_line(aes(x = year, y = total_count, color = common_name)) +
    theme_minimal() +
    labs(title = "Aggretgate Counts Over All Sites",
         x = "Year",
         y = "Count")
  plot
})
} 

# END ALL SERVER 

thematic_shiny()

# Run the application 
shinyApp(ui = ui, server = server)
