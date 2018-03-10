#####################################
#Salton Sea App
####################################

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(devtools)
library(maptools)

dust <- st_read(dsn = ".", layer = "DustEmiss")
# "." means look in the working directory

dust_df <- st_transform(dust, "+init=epsg:4326")

emiss <- dust_df %>%
  select(Emiss_Cat)

#dissolve <-unionSpatialPolygons(emiss$Emiss_Cat)

shoreline <- st_read(dsn = ".", layer = "HiResBathy")
shoreline_df <- st_transform(shoreline, "+init=epsg:4326")

playa <- shoreline_df %>%
  select(Year)



ui <- fluidPage(
  
  titlePanel("Salton Sea Future Shoreline & Emissivity"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("dusty", "Emissivity of Exposed Playa", choices = unique(emiss$Emiss_Cat)),
      sliderInput("shore", "Future Shoreline of the Sea", min = 2018, max = 2047, value = 2018, step = 5)
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("full_map")
    )
  )
)

# Define server logic required to draw an interactive map
server <- function(input, output) {
  
  output$full_map <- renderLeaflet({
    
    emiss_sub <- emiss %>%
      filter(Emiss_Cat == input$dusty)
    
    
    leaflet(emiss_sub) %>%
      addPolygons(weight = 0.5,
                  color = "red",
                  fillColor = "red",
                  fillOpacity = 1)
    
    
    shore_sub <- playa %>% 
      filter(Year == input$shore)
    
    leaflet(shore_sub) %>% 
      addTiles() %>% 
      addPolylines(weight = 1.5, color = "darkblue")
    
  })
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

