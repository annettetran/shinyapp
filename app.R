#####################################
#Salton Sea App
####################################

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(devtools)

sb_fhz <- st_read(dsn = ".", layer = "DustEmiss")
# "." means look in the working directory

sb_df <- st_transform(sb_fhz, "+init=epsg:4326")

sb_fh_class <- sb_df %>%
  select(Emiss_Cat)



# Define UI for application that draws a map
ui <- fluidPage(
  
  # Application title
  titlePanel("Santa Barbara Fire Hazard Zones"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("class", "Fire Hazard Class", choices = unique(sb_fh_class$Emiss_Cat))
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("fire_map")
    )
  )
)

# Define server logic required to draw an interactive map
server <- function(input, output) {
  
  output$fire_map <- renderLeaflet({
    
    fire_sub <- sb_fh_class %>%
      filter(Emiss_Cat == input$Emiss_Cat)
    
    leaflet(fire_sub) %>%
      addTiles() %>%
      addPolygons(weight = 0.5,
                   color = "red",
                   fillColor = "yellow",
                   fillOpacity = 0.5)
    
    
  }) 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

