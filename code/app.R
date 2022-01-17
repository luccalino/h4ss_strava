library(shiny)
library(rStrava)
library(osmdata)
library(tidyverse)
library(sf)
library(raster)
library(rgdal)
library(yaml)
library(httr)
library(jsonlite)
library(Thermimage)
library(shinycssloaders)
library(shinybusy)

ui <- fluidPage(
  
  titlePanel("Strava Visuals"),
  
  p('TBD'),
  
  sidebarPanel(
    textInput("app_name", "Application name", value = "", width = NULL, placeholder = NULL),
    textInput("id", "Client ID", value = "", width = NULL, placeholder = NULL),
    textInput("secret", "Client secret", value = "", width = NULL, placeholder = NULL),
    textInput("place", "Place", value = "e.g. Zurich, Switzerland", width = NULL, placeholder = NULL),
    checkboxGroupInput("features", "Features:", 
                       c("Buildings" = "buildings",
                         "Roads" = "roads"), 
                       selected = "Buildings"),
    actionButton("gd","Visualize my journey", icon("arrow-circle-down"), 
                 style = "color: #fff; background-color: #fc4c02; border-color: #2e6da4")
  ),
  
  mainPanel(
    withSpinner(plotOutput(outputId = 'plot1', width = 250, height = 250),
                color = "#fc4c02",
                type = 7),
  )
  
)

server <- function(input, output) {
  
  observeEvent(input$gd, {
    show_modal_gif(
      src = "https://media.giphy.com/media/kUTME7ABmhYg5J3psM/giphy.gif",
      text = "Getting data. This can take a while..."
    )
    
    # Create the authentication token
    stoken <- httr::config(token = strava_oauth(input$app_name, input$id, input$secret, app_scope="activity:read_all"))
    
    # Get activities
    my_acts <- get_activity_list(stoken)
    data <- as.data.frame(compile_activities(my_acts)) 
    
    # Subset activities with coordinates
    data <- subset(data, !is.na(start_latitude) | !is.na(start_longitude))
    
    # Convert coordinates from WGS 84 to LV05  
    coordinates(data) <- c("start_longitude", "start_latitude")
    proj4string(data) <- CRS("+init=epsg:4326") # WGS 84
    CRS.new <- CRS("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
    data <- spTransform(data, CRS.new)
    data <- as.data.frame(data)
    
    # Define region (lat/lon as polygone)
    poly_bb <- getbb(place_name = input$place)
    poly_bb_flipped <- poly_bb
    poly_bb_flipped[2,1] <- poly_bb[1,2]
    poly_bb_flipped[1,2] <- poly_bb[2,1]
    colnames(poly_bb_flipped) <- c("x","y")
    rownames(poly_bb_flipped) <- c("min","max")
    poly_bb_flipped <- data.frame(poly_bb_flipped)
    coordinates(poly_bb_flipped) <- c("x", "y")
    proj4string(poly_bb_flipped) <- CRS("+init=epsg:4326") 
    poly_bb_flipped <- spTransform(poly_bb_flipped, CRS.new)
    poly_bb_flipped <- as.matrix(as.data.frame(poly_bb_flipped))
    # X dimension
    poly_bb_flipped[1,1] <- poly_bb_flipped[1,1]-7000
    poly_bb_flipped[2,1] <- poly_bb_flipped[2,1]+7000
    # Y dimension
    poly_bb_flipped[1,2] <- poly_bb_flipped[1,2]-750
    poly_bb_flipped[2,2] <- poly_bb_flipped[2,2]+750
    poly_bb_4326 <- data.frame(poly_bb_flipped)
    coordinates(poly_bb_4326) <- c("x", "y")
    proj4string(poly_bb_4326) <- CRS.new 
    poly_bb_4326 <- spTransform(poly_bb_4326, "+proj=longlat +datum=WGS84 +no_defs")
    poly_bb_4326 <- as.matrix(as.data.frame(poly_bb_4326))
    
    # Subset only eligible data
    data <- subset(data, start_longitude >= poly_bb_flipped[1,1] & start_longitude <= poly_bb_flipped[2,1] &
                     start_latitude >= poly_bb_flipped[1,2] & start_latitude <= poly_bb_flipped[2,2])
    
    # Subset rides and runs
    data <- subset(data, type == "Run" | type == "Ride")
    
    # Get polylines 
    map_list <- list()
    
    for (i in 1:nrow(data)) {
      this_activity <- get_activity(id = data$id[i], stoken = stoken)
      this_polyline <- this_activity$map$summary_polyline
      decoded_polyline <- ifelse(is.null(this_polyline), next, print(i))
      decoded_polyline <- gepaf::decodePolyline(this_polyline)
      decoded_polyline$id <- i
      map_list[[i]] <- decoded_polyline
    }
    
    # Stack geocoded data
    polyline_data <- do.call(rbind, map_list)
    
    # Change projection
    coordinates(polyline_data) <- c("lon", "lat")
    proj4string(polyline_data) <- CRS("+init=epsg:4326")
    polyline_data <- spTransform(polyline_data, CRS.new)
    polyline_data <- as.data.frame(polyline_data)
    
    # Loading features
    if (input$features == "buildings") {
      buildings <- opq(bbox = poly_bb_4326, timeout = 120) %>% 
        add_osm_feature(key = 'building') %>%
        osmdata_sf() 
      buildings_polylines <- buildings$osm_polygons
      buildings_polylines <- st_transform(buildings_polylines, crs = st_crs(CRS.new))
    } else {
      paste("Skipped buildings")
    }
    
    if (input$features == "roads") {
      roads <- opq(bbox = poly_bb_4326, timeout = 120) %>% 
        add_osm_feature(key = 'highway') %>%
        osmdata_sf() 
      roads_polylines <- roads$osm_lines
      roads_polylines <- st_transform(roads_polylines, crs = st_crs(CRS.new))
    } else {
      paste("Skipped roads")
    }
    
    remove_modal_gif()
    
    output$plot1 <- renderPlot({
        
        ## Plot dimension
        xdim <- c(poly_bb_flipped[1,1]+1250, poly_bb_flipped[2,1]-1250)
        ydim <- c(poly_bb_flipped[1,2]+1000, poly_bb_flipped[2,2]-1000)
        
        ## Plot
        p <- ggplot() +
          #geom_sf(data = scree_polygone, fill = "grey", colour = NA) +
          #geom_sf(data = landuse_polygone, fill = "grey", colour = NA) +
          #geom_raster(data = relief_reduced, aes(x = x, y = y, alpha = value)) +
          #scale_alpha(name = "", range = c(0.6, 0), guide = "none") +   
          #geom_sf(data = water_multipolygon, fill = "#a0bcdf", colour = NA) +
          #geom_sf(data = river_lines, colour = "#a0bcdf", size = ifelse(river_lines$role == "main_stream", 1, 0.25)) +
          #geom_sf(data = roads_polylines, colour = "black", size = 0.2, alpha = 0.2) +
          #geom_sf(data = rail_polylines, colour = "black", size = 0.4, alpha = 0.3) +
          #geom_sf(data = lift_polylines, colour = "black", size = 0.4, alpha = 0.3) +
          #geom_sf(data = buildings_polylines, fill = "black", colour = NA) +
          geom_path(data = polyline_data, aes(x = lon, y = lat, group = id), 
                    color = "red3", alpha = 0.5, size = 0.175, lineend = "round", show.legend = FALSE) +
          coord_sf(xlim = xdim, ylim = ydim) + 
          theme_void()
        
        if (input$features == "buildings" & input$features != "roads") {
          p + 
            geom_sf(data = buildings_polylines, fill = "black", colour = NA)
        } else if (input$features != "buildings" & input$features == "roads") {
          p + 
            geom_sf(data = roads_polylines, fill = "black", colour = NA) 
        } else if (input$features == "raods" & input$features == "buildings") {
          p + 
            geom_sf(data = roads_polylines, colour = "black", size = 0.2, alpha = 0.2) +
            geom_sf(data = buildings_polylines, fill = "black", colour = NA)
        } else {
          paste("")
        }
        
        p
        
      })
  })
}

shinyApp(ui = ui, server = server)