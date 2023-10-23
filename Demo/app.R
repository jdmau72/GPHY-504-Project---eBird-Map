#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(auk)
library(lubridate)
library(ggplot2)
library(leaflet)
library(geojsonio)
library(sf)
library(jsonlite)
library(geojsonlint)

ebd_filename = "ebd_US-MT-031_relJun-2023.txt"
geojson_filename = "observations.geojson"
FWP = geojson_read("FWP.geojson")

# set up working directory -----------------------
cwd <- getwd()
ebird_data <- auk_ebd(ebd_filename)
f_out <- "ebd_filtered.txt"

# Define UI for application that draws a histogram ---
ui <- fluidPage( 

    # Application title
    titlePanel("Observation Distributions"),


    sidebarLayout(          
      mainPanel(
        leafletOutput("map", width="100%", height=800)
      ),
      
  
      absolutePanel(id="settings", fixed=TRUE, draggable=TRUE, 
                      top=60, left="auto", right=20, bottom="auto", width=330, height="auto",
                      
                      h2(""),
                      
                      #textInput("species_name", h4("Bird Name"), value = "Belted Kingfisher"),
                      selectInput("species_name", h4("Bird Name"), c("Belted Kingfisher", "Ruffed Grouse", "Dusky Grouse", "Western Tanager")),
                      dateInput("start_date", h4("Start Date"), value="2023-05-01"),
                      dateInput("end_date", h4("End Date"), value="2023-07-31"),
                      
                      plotOutput("distPlot")
                    )
    )
)


  
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # renders the basic leaflet map 
  output$map <- renderLeaflet({
      leaflet() %>%
      addTiles() %>%
      setView(lng = -111.0, lat = 45.7, zoom = 9.5)
  })
  
  # when inputs change, updates the reactive obs_sf value that gets used by other functions
  obs_sf <- reactive({
    ebd_df <- filteredData()
    observed_df <- subset(ebd_df, select = c('checklist_id', 'common_name', 'locality', 'observation_count', 'observation_date', 'time_observations_started', 'longitude', 'latitude'))
    
    # converts from data.frame to a sf object
    obs_sf <- st_as_sf(
      observed_df,
      coords = c("longitude", "latitude"),
      crs = 4326
    )
    return(obs_sf)
  })
  
  
  # observe function, so when inputs change, the map layer with observations will update
  observe({
    print(obs_sf()$checklist_id)
    leafletProxy("map") %>%
      clearGroup('observations') %>%
      addTiles() %>%
      addMarkers(
        data = obs_sf(), 
        group = 'observations',
        label = lapply(obs_sf()$locality, HTML),
        layerId = ~obs_sf()$checklist_id
        ,clusterOptions = markerClusterOptions()
        ) %>% 
      addGeoJSON(geojson = 'FWP', data = FWP)
  })
  
  
  showObservationPopup <- function(id, lat, lng){
    obs <- obs_sf()[obs_sf()$checklist_id == id, ]  # gets that specific observation
    content <- as.character(tagList(
      tags$h4(paste("Location: ", obs$locality, sep="")),
      tags$h4(paste("Count: ", obs$observation_count, sep="")),
      tags$h4(paste("Date: ", obs$observation_date, sep="")),
      tags$h4(paste("Time Start: ", obs$time_observations_started, sep="")),
      tags$h4(paste("ID: ", id, sep=""))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = "observationPopups")
  }
  
  
  # observe for displaying popups on clicking observations
  observe({
    leafletProxy("map") %>% clearPopups()
    clicked_marker <- input$map_marker_click
    if (is.null(clicked_marker)){
      return()
    }
    isolate({
      clicked_id <- clicked_marker$id
      showObservationPopup(clicked_id, clicked_marker$lat, clicked_marker$lng)
    })
  })
  
  
  # reactive function, anytime input updates it will reprocess data and filter
  # as well as save to geojson format
  filteredData <- reactive({
    # define the filters -----------------------
    ebd_filters <- auk_species(ebird_data, species = input$species_name)
    ebd_filters <- auk_date(ebd_filters, date = c(input$start_date, input$end_date))
    
    # M Trail bbox: -110.9771565,  45.7098916, -110.9769621, 45.7100700
    # E. Bozeman Area bbox:  -111.11, 45.77, -110.93, 45.59
    ebd_filters <- auk_bbox(ebd_filters, bbox = c(-111.11, 45.59, -110.93,  45.77))
    
    
    # filter data using filters and load in dataframe ------------------
    ebd_filtered <- auk_filter(ebd_filters, file = f_out, overwrite = TRUE)
    ebd_df <- read_ebd(ebd_filtered)
  })
  
  
  # code to render histogram, calculates avg observation hour
  output$distPlot <- renderPlot({

      # call call filterData() function to get filtered
      # ebd_df = filterData(input$species_name, input$start_date, input$end_date, c(-111.11, 45.59, -110.93,  45.77))
    
      ebd_df <- filteredData()

      observedTime <- subset(ebd_df, select = c('time_observations_started'))

      hours = c()
      for(i in 1:nrow(observedTime)){
        h <- hour(hms(as.character(observedTime[i,1])))
        hours <- append(hours, h)
      }

      counts_df <- data.frame(
        hour=seq(1,24,1),
        count=tabulate(hours, nbins = 24)
      )

      # plot the frequencies
      plt <- ggplot(counts_df, aes(x=hour, y=count)) +
        geom_bar(stat="identity") +
        scale_x_continuous(limits = c(1, 24), breaks = seq(1, 24, 1))
      print(plt)

    })
}




# Run the application 
shinyApp(ui = ui, server = server)
