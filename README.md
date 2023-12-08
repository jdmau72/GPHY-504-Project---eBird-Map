# GPHY 504 Final Project - <br />
eBird Observation Map for Gallatin County, MT

This project is for a Shiny web page/Leaflet web map that displays eBird Observation data for Gallatin County from 2021-2023. The code and data is stored in the Demo folder.

app.R has both R code for processing eBird observation data and Shiny R code to format the website. 

The side panel has filtering options, as well as a histogram displaying the distribution of observation hour times. 
The month slider allows a user to view observation month by month, as well as view monthly observation sequentially.

The gallatin_county_boundary.geojson and FWP.geojson files have been added to the map for reference. 


## Data Sources
**eBird Observation Data:** <br />
eBird Basic Dataset. Version: EBD_relJun-2023. Cornell Lab of Ornithology, Ithaca, New York. Jun 2023.

**FWP Lands Sites - Polygons:** <br />
https://gis-mtfwp.hub.arcgis.com/datasets/c7ad2cc2f8ec4195967abe6db90fe481_0/explore?location=46.754805%2C-109.688161%2C7.04

**Gallatin County Boundary:** <br />
https://hub.arcgis.com/datasets/montana::county-boundaries/about



## Website (Hosted by Shinyio)
https://justindmau.shinyapps.io/eBird_Observation_Map/
