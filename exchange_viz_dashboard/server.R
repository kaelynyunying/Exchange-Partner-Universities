
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
#library(dplyr)
library(tidyr)
library(tmap)
library(sf)
library(raster)
library(spData)
#library(leaflet)
library(readxl)
library(leaflet.extras)
library(htmltools)



# Leaflet bindings are a bit slow; for now we'll just sample to compensate
#set.seed(100)


#zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
#zipdata <- zipdata[order(zipdata$centile),]

exchange_data <- read_excel("data/Exchange Information Final.xlsx")

server<-function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    
      Europe <- filter(exchange_data, Region == "Europe")  
      Africa <- filter(exchange_data, Region == "Africa")
      Asia <- filter(exchange_data, Region == "Asia")
      Middle_East <- filter(exchange_data, Region == "Middle East")
      South_America <- filter(exchange_data, Region == "South America")
      North_America <- filter(exchange_data, Region == "North America")
      map<-exchange_data %>%
      leaflet(options = leafletOptions(zoomControl = FALSE,
                                       minZoom = 2)) %>% 
        addProviderTiles("CartoDB")%>% 
        addSearchOSM() #%>% 
        #addReverseSearchOSM() 
      map %>% 
        addCircleMarkers(data = Europe, radius = 2,  label = ~paste0(Partner_University," ", Country, " ", City), group = "Europe")  %>% 
        addCircleMarkers(data = Asia, radius = 2, label = ~paste0(Partner_University," ", Country, " ", City), group = "Asia")  %>% 
        addCircleMarkers(data = Africa, radius = 2, label = ~paste0(Partner_University," ", Country, " ", City), group = "Africa")  %>% 
        addCircleMarkers(data = Middle_East, radius = 2, label = ~paste0(Partner_University," ", Country, " ", City), group = "Middle East")  %>% 
        addCircleMarkers(data = South_America, radius = 2, label = ~paste0(Partner_University," ", Country, " ", City), group = "South America")  %>% 
        addCircleMarkers(data = North_America, radius = 2, label = ~paste0(Partner_University," ", Country, " ", City), group = "North America") %>%
        addLayersControl(overlayGroups = c("Europe", "Africa", "Asia", "Middle East", "South America", "North America"))%>%
        setView(lng = 14, lat = 20,zoom=2)
      
    
  })
  
  
  # Show a popup at the given location
  showPopup <- function(lat, lng) {
    selectedLocation <- filter(exchange_data, Latitude == lat & Longitude == lng)
    #print(oasis)
    url <- a(selectedLocation$Partner_University, href=selectedLocation$URL)
    content <- as.character(tagList(
      tags$h4(url),
      tags$strong(HTML(sprintf("%s, %s, %s",
                               selectedLocation$City, selectedLocation$Country, selectedLocation$Region
      ))), tags$br(),
      sprintf("Spring 2019 Placement: %s", as.integer(selectedLocation$Spring_2019)), tags$br(),
      sprintf("Spring 2018 10th Percentile: %s", as.double(selectedLocation$tenth_percentile)), tags$br(),
      sprintf("Spring 2018 90th Percentile: %s", as.double(selectedLocation$ninetyth_percentile)), tags$br(),
      sprintf("Programmes: %s", selectedLocation$programmes), tags$br(),
      sprintf("Remarks: %s", selectedLocation$Remarks), tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    print(event)
    if (is.null(event))
      return()
    
    isolate({
      showPopup(event$lat, event$lng)
    })
  })
} 