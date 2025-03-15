library(shiny)
library(leaflet)
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
