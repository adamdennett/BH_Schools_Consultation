library(shiny)
library(leaflet)

# ---- Basemap, no API key ---------------------------------------------
# CARTO require a key on their raster tiles, and without one the tiles
# carry an "API key required" watermark. That failed in the only way
# that really matters: silently. Any render from a shell without
# CARTO_KEY in its environment produced watermarked maps and no other
# symptom, so the fault was found by eye or not at all.
#
# So the tiles come from Esri's World Gray Canvas instead, which needs
# no key. Of the keyless alternatives it is the one that actually is
# keyless: Stadia's tiles return 401 without a key and Jawg's return
# 400, both having moved to requiring one.
#
# This still wraps addProviderTiles() so that no call site has to
# change. A CartoDB.* provider is served from Esri; anything else is
# passed straight through to leaflet. Esri splits the canvas and the
# place names into two services where CARTO bakes them into one, so the
# Positron-style variants map onto one or both of them, and the
# DarkMatter ones onto Esri's dark canvas.
#
# maxNativeZoom is set because Esri publishes these canvases to zoom 16
# and above that the server returns a grey tile reading "Map data not
# yet available" rather than a 404 - a silent failure again. Leaflet
# upsamples level 16 instead of asking for 17, so the basemap softens
# past that point and the data drawn over it stays sharp.
addProviderTiles <- function(map, provider, ...) {
  nm <- as.character(provider)[1]
  if (!startsWith(nm, "CartoDB."))
    return(leaflet::addProviderTiles(map, provider, ...))
  nm <- substring(nm, 9L)

  dark <- startsWith(nm, "DarkMatter")
  shade <- if (dark) "Dark" else "Light"
  esri <- function(part) sprintf(paste0(
    "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/",
    "World_%s_Gray_%s/MapServer/tile/{z}/{y}/{x}"), shade, part)

  want_base   <- !grepl("OnlyLabels$", nm)
  want_labels <- !grepl("NoLabels$", nm)

  dots <- list(...)
  opts <- leaflet::tileOptions(maxNativeZoom = 16, maxZoom = 20)
  if (!is.null(dots$options)) {
    opts <- utils::modifyList(opts, dots$options); dots$options <- NULL
  }
  attrib <- paste0(
    'Tiles &copy; <a href="https://www.esri.com/">Esri</a> &mdash; Esri, ',
    'HERE, Garmin, &copy; ',
    '<a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
    ' contributors')

  if (want_base)
    map <- do.call(leaflet::addTiles, c(list(
      map, urlTemplate = esri("Base"), attribution = attrib,
      options = opts), dots))
  # Only one of the two layers carries the credit, or every map would
  # print it twice in the corner.
  if (want_labels)
    map <- do.call(leaflet::addTiles, c(list(
      map, urlTemplate = esri("Reference"),
      attribution = if (want_base) "" else attrib,
      options = opts), dots))
  map
}
# ----------------------------------------------------------------------
library(sf)
library(dplyr)
library(tidyverse)
library(viridis)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
##----------------
  
#BN_pcds_to_school_dist <- st_read("data/BN_pcds_to_school_dist.gpkg") %>%   st_transform(4326)


# brighton_sec_schools <- read_csv("https://www.dropbox.com/scl/fi/fhzafgt27v30lmmuo084y/edubasealldata20241003.csv?rlkey=uorw43s44hnw5k9js3z0ksuuq&raw=1") %>% 
#   clean_names() %>% 
#   filter(la_name == "Brighton and Hove") %>% 
#   filter(phase_of_education_name == "Secondary") %>% 
#   filter(establishment_status_name == "Open") %>%
#   st_as_sf(., coords = c("easting", "northing")) %>% 
#   st_set_crs(27700) %>% 
#   st_transform(4326) %>% 
#   st_set_crs(4326)
# 
# bh_sec_sch <- brighton_sec_schools %>% 
#   select(urn, establishment_name, geometry) %>%
#   rename(id = urn)
# 
# coords <- st_coordinates(bh_sec_sch)
# bh_sec_sch$lon <- coords[, 1]
# bh_sec_sch$lat <- coords[, 2]
#   
##---------------

# # Convert dataframes to sf objects
# BN_pcds_to_school_dist <- st_as_sf(BN_pcds_to_school_dist)
# brighton_sec_schools <- st_as_sf(brighton_sec_schools)
# 
# # Perform the spatial join
# BN_pcds_to_school_dist <- st_join(BN_pcds_to_school_dist, brighton_sec_schools, join = st_nearest_feature)
# 
# # Select and rename columns
# BN_pcds_to_school_dist <- BN_pcds_to_school_dist %>%
#   select(origin_id, destination_id, entry_cost, network_cost, exit_cost, total_cost, geom, school_name = establishment_name)



# Assuming your dataframe is named df
df <- BN_pcds_to_school_dist


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Home to School Distance Finder"),
  sidebarLayout(
    sidebarPanel(
      textInput("origin", "Enter your home postcode, e.g.  BN3 3BQ:")
    ),
    mainPanel(
      h3(textOutput("originTitle")),
      leafletOutput("map"),
      tableOutput("costTable")
    )
  )
)


###map time
server <- function(input, output, session) {
  filtered_data <- reactive({
    df %>%
      filter(origin_id == input$origin)
  })
  
  output$originTitle <- renderText({
    paste("Routes from Origin ID:", input$origin)
  })
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    # Create reversed color palette
    pal <- colorNumeric(
      palette = rev(magma(256)),
      domain = data$total_cost
    )
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(
        data = st_as_sf(data, wkt = "geom"),
        color = ~pal(total_cost)
      ) %>%
      addLegend(
        pal = pal,
        values = data$total_cost,
        title = "Total Distance (metres)"
      )
  })
  
  output$costTable <- renderTable({
    data <- filtered_data() %>%
      select(school_name, total_cost) %>%
      rename(
        School = school_name,
        `Distance (metres)` = total_cost
      ) %>%
      st_set_geometry(NULL) %>%
      arrange(`Distance (metres)`)
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
