# Kelp Dynamics Shiny App 

## Overview

The purspose of this project was to use data colelcted by the Santa Barba Coastal LTER to show some features around kelp forest dynamics in Santa Barbara. The app (located in the 'sbc_lter_kelp' subfolder) is broken up into three primary tabs:
- Kelp Canopy: Spatial visualization of kelp cover over time based on subsets of ongoing landsat imagery.
- Abiotic Factors: Comparisons of kelp density by site based on diver surveys including information on temperture and regional nitrate concentrations.
- Kelp Forest Community: Visualizations of abundance of every species surveyed by LTER divers and kelp density across survey sites. 

All data is credited to the SBC LTER and can be accessed here: 
https://sbclter.msi.ucsb.edu/

### File Structure 

Data wrangling was piloted in R Markdown ('.Rmd') files before transporting into the Shiny App ('app.R). These are all included in the 'code' subfolder and draw on raw datasets organized in the 'data' subfolder. 

* A note on spatial data: ongoing landsat data was recorded in a ".nc" file form that proved difficult to manipulate initially. Due to the massive size of this file, we were unable to upload this into the repo directly. All of these individual csv files are included in the 'data' folder and within the 'biomass_files' subfolder. Others who would like to use this code to explore other areas of the region or coast more broadly should download the following file to their local machines: https://sbclter.msi.ucsb.edu/data/catalog/package/?package=knb-lter-sbc.74. Example code of how subsets can be made are incldued in the 'kelp_sat.Rmd' document.*

The following are the Rmd documents that correspond to the primary tabs
- Kelp Canopy: 'kelp_sat.Rmd'
- Abiotic Factors: 'kelp_npp.Rmd'
- Kelp Forest Community: 'kelp_biodiversity.Rmd'



### Acklowledgements
We would like to thank Dr. Li Kui of the UCSB Marine Science Institute for valuable input and code examples using various data sets which were instrumental in our understanding of the metadata and the design of this app. 

We would also like to thank Casey Ohara and Nathan Grimes at the UCSB Bren School of Environmental Science and Management for criticial help along the way throughout the execution of this project. 
