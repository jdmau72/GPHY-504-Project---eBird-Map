#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Author: Justin Mau


# --------------------------------------------------------------------------------------------------------------------------------
# Initial code - libraries, globals, basic setup of data, read in data from file  ---------------------------------------------------------- \\
# --------------------------------------------------------------------------------------------------------------------------------
library(shiny)
library(auk)
library(lubridate)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(sf)
library(jsonlite)
library(shinyjs)


# globals ----------------------------------
ebd_filename = "ebd_2021_2023.txt"
geojson_filename = "observations.geojson"

bin_size <- 12
currentMonth <- 1

# set up working directory -----------------------
cwd <- getwd()
ebird_data <- auk_ebd(ebd_filename)
f_out <- "ebd_filtered.txt"

# get all species from the ebd_file to load them into the selectInput panel
ebd_speciesList <- ebd_filename %>% 
  read_ebd(rollup = FALSE)
ebd_speciesList <- unique(ebd_speciesList$common_name)

# reads in the geojsons for the Gallatin County Border and FWP polygons
gallatin_border <- geojson_read("CountyBorder_Gallatin.geojson")
FWP <- geojson_read("FWP_Gallatin.geojson", what = "sp")
# --------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------ //
# --------------------------------------------------------------------------------------------------------------------------------





# --------------------------------------------------------------------------------------------------------------------------------
# User Interface - inputs, map, etc -------------------------------------------------------------------------------------------------------------- \\
# --------------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
#ui <- fillPage( 

    includeCSS("style.css"), # honestly not sure if this CSS actually affects anything unfortunately
    leafletOutput("map", width="100%", height="99vh"),
    
    #sidebarLayout( 
    fluidRow( 
     column(9,
        mainPanel(
          
        )
      ),
      
     column(3,
      absolutePanel(
        id="settings", fixed=TRUE, draggable=FALSE, 
        top="1%", left="auto", right=10, bottom="1%", width=360, height="auto", style = "background: rgba(255, 255, 255, 0.8); padding: 10px",
          
          h2("eBird Observations Map"),
        
          selectInput("species_name", h4("Bird Name"), choices = ebd_speciesList),
        
          checkboxInput("displayClusters_checkbox", "Display Clustered Observations"),
          
          sliderInput("monthSlider", "Month", min = 1, max = 12, value = 1, step = 1, animate = animationOptions(interval = 1000, loop = TRUE)),
          sliderInput("yearSlider", "Years", min = 2002, max = year(Sys.Date()), value = c(2021, 2023), step = 1), 
          
          h4("Hourly Distribution of Observations"),
          plotOutput("distPlot", width = "95%", height="40%"),
          h2(""),
          img(src="north_arrow.png", right=0, top=20, width = 50),
        )
     )
    )
)
# --------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------//
# --------------------------------------------------------------------------------------------------------------------------------





# --------------------------------------------------------------------------------------------------------------------------------
# Server Logic - processing inputs, generating sf, etc ------------------------------------------------------------------------------------------- \\
# --------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  
  # renders the basic leaflet map + FWP + Gallatin County Border 
  output$map <- renderLeaflet({
      leaflet(FWP) %>%
        addProviderTiles(provider = "Esri.WorldTopoMap") %>%
        addScaleBar(position = "topleft") %>%
        addGeoJSON(geojson = gallatin_border, color = "black", weight = 1, fillOpacity = 0) %>%
        addPolygons(stroke = FALSE, fillOpacity = 0.5, fillColor = "orange", 
                    label = ~paste0(PUBNAME, " ", PUBTYPE)) %>%
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
  
  
  
  # variable that is set to the value of the checkbox, so you can turn off clusters
  displayCluster <- reactive({
    input$displayClusters_checkbox
  })
  
  
  
  # observe function, so when inputs change, the map layer with observations will update
  observe({
    leafletProxy("map") %>%
      clearGroup('observations')
    
    # when display cluster checked, uses clusterOptions
    if (displayCluster()){     
      leafletProxy("map") %>%
        addMarkers(
          data = obs_sf(), 
          group = 'observations',
          label = lapply(obs_sf()$locality, HTML),
          layerId = ~obs_sf()$checklist_id,
          clusterOptions = markerClusterOptions()
        )
    } 
    
    # adds the heatmap regardless
    leafletProxy("map") %>%
      clearGroup('heatmap') %>%
      addHeatmap(
        data= obs_sf(),
        group = 'heatmap',
        radius= 30,
        blur = 10,
        max= 50
      )
  })
  
  
  
  # code for displaying info about the hotspot when you click on it
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

  
  
  # observe to visualize monthly observations/migration patterns
  observe({
    # gets current month from the input
    currentMonth <- input$monthSlider
    
    # first hides all the observations
    leafletProxy("map") %>%
      clearGroup('observations') %>%
      clearGroup('heatmap')

    # hides every monthly map
    for (i in 1:bin_size){
      leafletProxy("map") %>%
        hideGroup(paste('animated_observations_', as.character(i)))
    }
    
    # displays the current month's map
    leafletProxy("map") %>%
      showGroup(paste('animated_observations_', as.character(currentMonth)))
  })
    
  
  
  # reactive function, anytime input updates it will reprocess data and filter
  filteredData <- reactive({
    # uses start and end date from the years slider
    startDate = as.Date(paste(input$yearSlider[1], 1, 1, sep="-"))
    endDate = as.Date(paste(input$yearSlider[2], 12, 31, sep="-"))
    
    # define the filters -----------------------
    ebd_filters <- auk_species(ebird_data, species = input$species_name)
    ebd_filters <- auk_date(ebd_filters, date = c(startDate, endDate))
    
    # filter data using filters and load in dataframe ------------------
    ebd_filtered <- auk_filter(ebd_filters, file = f_out, overwrite = TRUE)
    ebd_df <- read_ebd(ebd_filtered)
  })
  
  
  
  # observe for updating the monthly observations layer
  observe({
    # gets filtered data, selects important columns
    ebd_df <- filteredData()
    observed_df <- subset(ebd_df, select = c('checklist_id', 'common_name', 'locality', 'observation_count', 'observation_date', 'time_observations_started', 'longitude', 'latitude'))
    
    # creates empty obs_list
    obs_list <-list()
    
    # extracts data in each observation month, saves to obs_list
    for (i in 1:bin_size){
      obs_list[[i]] <- observed_df[as.integer(format(observed_df[["observation_date"]], "%m")) == i, ]
    }  

    # converts from data.frame to a sf object for each subset of observations to be displayed
    for (i in 1:bin_size){
      sf <- st_as_sf(
        obs_list[[i]],
        coords = c("longitude", "latitude"),
        crs = 4326
      )
      
      # updates obs_list to now store the sf 
      obs_list[[i]] <- sf
    }
    
    # now it creates and hides each month's observation
    for (i in 1:bin_size){
      leafletProxy("map") %>%
        clearGroup(paste('animated_observations_', as.character(i))) %>%
        addHeatmap(
          data= obs_list[[i]],
          group = paste('animated_observations_', as.character(i)),
          radius= 30,
          blur = 10,
          max= 25
        ) %>%
        hideGroup(paste('animated_observations_', as.character(i)))
      
      # NO LONGER ADDS THE SIMPLE MARKERS, USES HEATMAP INSTEAD
      # adds the individual simple markers for the observations
      #leafletProxy("map") %>%
      #  addMarkers(
      #    data = obs_list[[i]], 
      #    group = paste('animated_observations_', as.character(i))
      #  ) %>%
      #  hideGroup(paste('animated_observations_', as.character(i)))
    }
    
    print("Observations successfully split by month...   ----------------------------------------")
  })

  
  
  # code to render histogram, calculates avg observation hour
  output$distPlot <- renderPlot({
      # call filterData() function to get filtered
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
# --------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------- //
# --------------------------------------------------------------------------------------------------------------------------------





# Run the application ---------------------------------------------------------\
shinyApp(ui = ui, server = server)
