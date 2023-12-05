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
library(leaflet.extras)
library(geojsonio)
library(sf)
library(jsonlite)
library(shinyjs)
#library(geojsonlint)


# globals ----------------------------------
ebd_filename = "ebd_2021_2023.txt"
geojson_filename = "observations.geojson"
FWP = geojson_read("FWP.geojson")

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

# reads in the csv for the Gallatin County Border
gallatin_border <- st_as_sf( st_read("gallatin_county_boundary.geojson"), as= "list")







# Define UI for application that draws a histogram ---
ui <- fluidPage(
#ui <- fillPage( 

    useShinyjs(), # got from gpt? might not work
  
    sidebarLayout(          
      mainPanel(
        leafletOutput("map", width="160%", height="1000")
      ),
      
  
      absolutePanel(id="settings", fixed=TRUE, draggable=FALSE, 
                      top="1%", left="auto", right=10, bottom="1%", width=360, height="auto", style = "background: rgba(255, 255, 255, 0.8); padding: 10px",
                      
                      h2("eBird Observations Map"),
                    
                      checkboxInput("displayClusters_checkbox", "Display Clustered Observations"),
                      
                      sliderInput("monthSlider", "Month", min = 1, max = 12, value = 1, step = 1, animate = animationOptions(interval = 1000, loop = TRUE)),
                      #textInput("species_name", h4("Bird Name"), value = "Belted Kingfisher"),
                      selectInput("species_name", h4("Bird Name"), choices = ebd_speciesList),
                      dateInput("start_date", h4("Start Date"), value="2021-01-01"),
                      dateInput("end_date", h4("End Date"), value="2023-01-01"),
                      
                      h4("Hourly Distribution of Observations"),
                      plotOutput("distPlot", width = "95%"),
                      h2(""),
                      img(src="north_arrow.png", right=0, top=20, width = 50),
                    
                      actionButton("animate", "Play")
                    )
    )
)




# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------


  
# Define server logic 
server <- function(input, output) {
  
  
  
  # renders the basic leaflet map 
  output$map <- renderLeaflet({
      leaflet() %>%
      addProviderTiles(provider = "Esri.WorldTopoMap") %>%
      addScaleBar(position = "topleft") %>%
      # addGeoJSON(geojson = 'FWP', data = FWP) %>%
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
      clearGroup('observations') %>%
      # addTiles() %>%
      addGeoJSON(geojson = gallatin_border )
    
    if (displayCluster()){  # when display cluster checked, uses clusterOptions
      leafletProxy("map") %>%
        addMarkers(
          data = obs_sf(), 
          group = 'observations',
          label = lapply(obs_sf()$locality, HTML),
          layerId = ~obs_sf()$checklist_id,
          clusterOptions = markerClusterOptions()
        )
    } else {
      # leafletProxy("map") %>%
      #   addMarkers(
      #     data = obs_sf(), 
      #     group = 'observations',
      #     label = lapply(obs_sf()$locality, HTML),
      #     layerId = ~obs_sf()$checklist_id,
      #     options = markerOptions(minZoom = 6, maxZoom = 5)
      #   )
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
  
  
  
  
  
  
  
  
  
  
  # IN PROGRESS... ---------------------------------------------------------------------
  #  ---------------------------------------------------------------------
  # --------------. \/ \/ \/ \/---------------------------------------------------------------------

  
  
  # adding code to try to visualize observations/migration patterns
  observeEvent(input$animate, {
    print("Clicked play button...")
    # first clears the map of observations
    leafletProxy("map") %>%
      clearGroup('observations') %>%
      clearGroup('heatmap')
    
    animate_observations_oneStep()
  
  })
  
  observe({
    currentMonth <- input$monthSlider
    
    # first hides all the observations
    leafletProxy("map") %>%
      clearGroup('observations') %>%
      clearGroup('heatmap')

    for (i in 1:bin_size){
      leafletProxy("map") %>%
        hideGroup(paste('animated_observations_', as.character(i)))
    }
    
    leafletProxy("map") %>%
      showGroup(paste('animated_observations_', as.character(currentMonth)))
  })
  
  # function for animating observations, STILL IS NOT WORKING, ONLY ENDS UP DISPLAYING THE LAST OF THEM!!!
  animate_observations <- function(i){
    if (i == 1){
      leafletProxy("map") %>%
        showGroup(paste('animated_observations_', as.character(i)))
    } else {
      leafletProxy("map") %>%
        hideGroup(paste('animated_observations_', as.character(i - 1))) %>%
        showGroup(paste('animated_observations_', as.character(i)))
    }
  }
  
  
  # added this to just click the button and have it advance one each time
  animate_observations_oneStep <- function(){
    print(paste("Printing out observations for ", month.name[currentMonth]))
    if (currentMonth == 1){
      leafletProxy("map") %>%
        hideGroup(paste('animated_observations_', as.character(bin_size))) %>%
        showGroup(paste('animated_observations_', as.character(currentMonth)))
    } else {
      leafletProxy("map") %>%
        hideGroup(paste('animated_observations_', as.character(currentMonth - 1))) %>%
        showGroup(paste('animated_observations_', as.character(currentMonth)))
    }
    
    currentMonth <<- currentMonth + 1
    if (currentMonth > bin_size){
      currentMonth <<- 1
    }
    
  }
  # IN PROGRESS... /\ /\ /\ /\ ---------------------------------------------------------------------
  #  ---------------------------------------------------------------------
  # --------------. ---------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  # reactive function, anytime input updates it will reprocess data and filter
  # as well as save to geojson format
  filteredData <- reactive({
    # define the filters -----------------------
    ebd_filters <- auk_species(ebird_data, species = input$species_name)
    ebd_filters <- auk_date(ebd_filters, date = c(input$start_date, input$end_date))
    
    # M Trail bbox: -110.9771565,  45.7098916, -110.9769621, 45.7100700
    # E. Bozeman Area bbox:  -111.11, 45.77, -110.93, 45.59
    # ebd_filters <- auk_bbox(ebd_filters, bbox = c(-111.11, 45.59, -110.93,  45.77))
    
    
    # filter data using filters and load in dataframe ------------------
    ebd_filtered <- auk_filter(ebd_filters, file = f_out, overwrite = TRUE)
    ebd_df <- read_ebd(ebd_filtered)
  })
  
  
  # observe for updating the monthly observations layer
  observe({
    ebd_df <- filteredData()
    observed_df <- subset(ebd_df, select = c('checklist_id', 'common_name', 'locality', 'observation_count', 'observation_date', 'time_observations_started', 'longitude', 'latitude'))
    print(as.integer(format(input$start_date, '%m')))
    print(input$end_date)
    obs_list <-list()
    
    #observed_df$Month <- as.integer(format(observed_df$observation_date, "%m"))
    for (i in 1:bin_size){
      print(paste("i = ", i, "   |?|   m = ", as.integer(format(observed_df[["observation_date"]][1], "%m"))))
      #obs_list[i] = subset(observed_df, 
      #                     subset = as.integer(format(observation_date, "%m")) == i
      obs_list[[i]] <- observed_df[as.integer(format(observed_df[["observation_date"]], "%m")) == i, ]
      #print(obs_list[[i]])
    }  

    # converts from data.frame to a sf object for each subset of observations to be displayed
    for (i in 1:bin_size){
      print(obs_list[[i]])
      sf <- st_as_sf(
        obs_list[[i]],
        coords = c("longitude", "latitude"),
        crs = 4326
      )
      obs_list[[i]] <- sf
    }
    
    # now it creates and hides each month's observation
    for (i in 1:bin_size){
      print(obs_list[[i]])
      leafletProxy("map") %>%
        addMarkers(
          data = obs_list[[i]], 
          group = paste('animated_observations_', as.character(i))
        ) %>%
        hideGroup(paste('animated_observations_', as.character(i)))
    }
    
    print("okay now it has created all of these observation layers by month, now to try to display them sequentially...   ----------------------------------------")
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
  
  # ADD CODE TO DISPLAY HISTOGRAM FOR SEASONAL OBSERVATION DISTRIBUTION
  
}




# Run the application 
shinyApp(ui = ui, server = server)
