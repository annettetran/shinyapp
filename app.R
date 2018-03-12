#####################################
#Salton Sea App
####################################

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(devtools)
library(maptools)

dust <- st_read(dsn = ".", layer = "Emiss_Dissolve")

dust_df <- st_transform(dust, "+init=epsg:4326")

emiss <- dust_df %>%
  select(Emiss_Cat)


shoreline <- st_read(dsn = ".", layer = "SeaLevel")
shoreline_df <- st_transform(shoreline, "+init=epsg:4326")


playa <- shoreline_df %>%
  select(Year)



ui <- fluidPage(
  
  titlePanel("Salton Sea Future Shoreline & Emissivity"),
  
  sidebarLayout(
    sidebarPanel(
      
      
        sliderInput("shore", "Select Year:", min = 2018, max = 2047, value = 2018, step = 5),
      #selectInput("dusty", "Emissivity of Exposed Playa", choices = unique(emiss$Emiss_Cat))
      checkboxGroupInput("dusty", "Select Emissivity of Exposed Playa:", choices = unique(emiss$Emiss_Cat), selected = "Least Emissive")
      
      
      
      
    ),
    
    mainPanel(
      leafletOutput("full_map"),
      textOutput("acres")
      
      )
  )
)


server <- function(input, output) {
  
  output$full_map <- renderLeaflet({
    
    emiss_sub <- emiss %>%
    filter(Emiss_Cat == input$dusty)
    
    shore_sub <- playa %>% 
      filter(Year == input$shore)
    
  leaflet() %>% 
    addProviderTiles("Stamen.Terrain") %>% 
    addPolygons(data = emiss_sub, weight = 0.5,
                color = "red",
                fillColor = "red",
                fillOpacity = 1) %>% 
  addPolylines(data = shore_sub, weight = 3, color = "blue", opacity = 1)  %>% 
  addLegend(colors = c("blue", "red"), labels = c("Shoreline", "Exposed Playa"))
   
   }) 
  

 output$acres <- renderText({
   acreage <- shoreline_df %>% 
     filter(Year == input$shore)
   
   acreage$Cumul_Ac})
  
  

   
 }
    


# Run the application 
shinyApp(ui = ui, server = server)

