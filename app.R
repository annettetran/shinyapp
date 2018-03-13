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

dust <- st_read(dsn = ".", layer = "DustEmiss")

dust_df <- st_transform(dust, "+init=epsg:4326")

emiss <- dust_df %>%
  select(Emiss_Cat)


shoreline <- st_read(dsn = ".", layer = "SeaLevel")
shoreline_df <- st_transform(shoreline, "+init=epsg:4326")

playa <- shoreline_df %>%
  select(Year)

parcel <-st_read(dsn = ".", layer = "FutureExposedParcels")

parcel_df <- st_transform(parcel, "+init=epsg:4326")

landowners <- parcel_df %>% 
  select(OWNER_CLAS)

order <- factor(landowners$OWNER_CLAS, levels = c("Coachella Valley Water", "Federal", "IID", "Private", "Tribal", "Other"))


ui <- 
  
  dashboardPage(
    
    dashboardHeader(title = "Salton Sea Future Shoreline & Emissivity", titleWidth = 450),
    dashboardSidebar(disable = TRUE),
  
    dashboardBody( 
      fluidPage(
        
      box(title = "Inputs", status = "success", 
        sliderInput("shore", "Select Year:", min = 2018, max = 2047, value = 2018, step = 5, sep = NA),
        
        

        
        radioButtons("dusty", 
                    "Select Emissivity of Future Exposed Playa:", choices = unique(emiss$Emiss_Cat)),
        
        selectInput("owner", "Select Landowner:", choices = levels(factor(order))), 
                      
                      #unique(landowners$OWNER_CLAS)),
        
        
        
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
    
    parcel_sub <- landowners %>% 
      filter(OWNER_CLAS == input$owner)
    
  leaflet() %>% 
    addProviderTiles("Stamen.Terrain") %>% 
    addPolygons(data = emiss_sub,
                color = "red",
                stroke = FALSE,
                smoothFactor = 0.2,
                fillOpacity = 0.6) %>% 
  addPolylines(data = shore_sub, weight = 3, color = "blue", opacity = 0.6)  %>% 
  addPolygons(data = parcel_sub, weight = 1.5, fill = NA, color = "purple", opacity = 1) %>% 
  addLegend(colors = c("blue", "red", "purple"), 
            labels = c("Predicted Shoreline", "Exposed Playa in 2047", "Landowner Parcels"))
   
   }) 
  

 output$acres <- renderText({
   acreage <- shoreline_df %>% 
     filter(Year == input$shore)
   
   acreage$Cumul_Ac})
  
  

   
 }
    


shinyApp(ui = ui, server = server)

