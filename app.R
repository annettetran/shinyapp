#####################################
#Salton Sea App
####################################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(sf)
library(devtools)
library(maptools)
library(rgdal)

dust <- st_read(dsn = ".", layer = "Emiss_Dissolve")

dust_df <- st_transform(dust, "+init=epsg:4326")

emiss <- dust_df %>%
  select(Emiss_Cat)


shoreline <- st_read(dsn = ".", layer = "SeaLevel")
shoreline_df <- st_transform(shoreline, "+init=epsg:4326")


playa <- shoreline_df %>%
  select(Year)



ui <- 
  
  dashboardPage(
    
    dashboardHeader(title = "Salton Sea Future Shoreline & Emissivity", titleWidth = 450),
    dashboardSidebar(disable = TRUE),
  
    dashboardBody( 
      fluidPage(
        
      box(title = "Inputs", status = "success", 
        sliderInput("shore", "Select Year:", min = 2018, max = 2047, value = 2018, step = 5),
        checkboxGroupInput("dusty", 
                         "Select Emissivity of Future Exposed Playa:", 
                         choices = unique(emiss$Emiss_Cat), 
                         selected = "Most Emissive"), 
        
        submitButton(text = "Apply Changes")
        ),
      
      
         
      
      box(title = "Cumulative Acreage of Exposed Playa", status = "success", span(textOutput("acres"), style = 'font-weight:bold; font-size:25px; color:red;')),
    
      box(title = "Map of Predicted Shoreline and Emissivity of Exposed Playa", 
          status = "success", leafletOutput("full_map")))
    
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
    addPolygons(data = emiss_sub,
                color = "red",
                stroke = FALSE,
                smoothFactor = 0.2,
                fillOpacity = 0.6) %>% 
  addPolylines(data = shore_sub, weight = 3, color = "blue", opacity = 0.6)  %>% 
  addLegend(colors = c("blue", "red"), 
            labels = c("Predicted Shoreline", "Exposed Playa in 2047"))
   
   }) 
  

 output$acres <- renderText({
   acreage <- shoreline_df %>% 
     filter(Year == input$shore)
   
   acreage$Cumul_Ac})
  
  

   
 }
    


shinyApp(ui = ui, server = server)

