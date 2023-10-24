# GPHY 504 Project 1 - eBird Observation Map
This project is for a Shiny web page/Leaflet web map that displays eBird Observation data for Gallatin County from 2021-2023. The code and data is stored in the Demo folder.

app.R has both R code for processing eBird observation data and Shiny R code to format the website. 

The side panel has filtering options, as well as a histogram displaying the distribution of observation hour times. 

The gallatin_county_boundary.geojson and FWP.geojson files will be added to the map in the future. For now, I was having trouble displaying them properly on the Leaflet map. 

## Data source 
eBird Basic Dataset. Version: EBD_relJun-2023. Cornell Lab of Ornithology, Ithaca, New York. Jun 2023.

## Website (Hosted by Shinyio)
https://justindmau.shinyapps.io/eBird_Observation_Map/