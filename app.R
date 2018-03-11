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

acres <- read_csv("exposedplaya.csv")


ui <- fluidPage(
  
  titlePanel("Salton Sea Future Shoreline & Emissivity"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("shore", "Future Shoreline of the Sea", min = 2018, max = 2047, value = 2018, step = 5),
      selectInput("dusty", "Emissivity of Exposed Playa", choices = unique(emiss$Emiss_Cat))
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("full_map"),
      textOutput("shoreline")
      
      )
  )
)

# Define server logic required to draw an interactive map
server <- function(input, output) {
  
  output$full_map <- renderLeaflet({
     emiss_sub <- emiss %>%
    filter(Emiss_Cat == input$dusty)
    
  leaflet(emiss_sub) %>% 
      addProviderTiles("Stamen.Terrain") %>% 
    addPolygons(weight = 0.5,
                color = "red",
                fillColor = "red",
                fillOpacity = 1)  
   
   }) 
  
  observe({
  shore_sub <- playa %>% 
    filter(Year == input$shore)
  
  leafletProxy("full_map",data=shore_sub) %>%
      addProviderTiles("Stamen.Terrain") %>% 
       addPolylines(weight = 3, color = "white", opacity = 1)
  
  })
  acre_sub <- acres %>% 
    filter(Year == input$shore)
  
 output$shoreline <- renderText({acre_sub
   
 })
    
    
    

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

