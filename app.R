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

dust_df <- st_transform(dust, "+init=epsg:4326")

emiss <- dust_df %>%
  select(Emiss_Cat)

#dissolve <-unionSpatialPolygons(emiss$Emiss_Cat)

shoreline <- st_read(dsn = ".", layer = "HiResBathy")
shoreline_df <- st_transform(shoreline, "+init=epsg:4326")

#shoreline_df2 <- subset(shoreline_df, !(Year == "2046")) %>% 
  #arrange(Year)

#shoreline_df2$Acres<- acres$CumulativeExposed

playa <- shoreline_df %>%
  select(Year)

acres <- read_csv("exposedplaya.csv")


ui <- fluidPage(
  
  titlePanel("Salton Sea Future Shoreline & Emissivity"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("shore", "Select Year", min = 2018, max = 2047, value = 2018, step = 5),
      #selectInput("dusty", "Emissivity of Exposed Playa", choices = unique(emiss$Emiss_Cat))
      checkboxGroupInput("dusty", "Select Emissivity of Exposed Playa", choices = unique(emiss$Emiss_Cat), selected = "Least Emissive")
      
      #"sexe","Sexe:", 
      #choices = c("Masculin" = "mas", "Femenin" = "fem"),
      #selected = c("mas","fem")
      
      
    ),
    
    mainPanel(
      leafletOutput("full_map")
    #  box(textOutput("acreage"))
      
      )
  )
)


server <- function(input, output) {
  
  output$full_map <- renderLeaflet({
    emiss_sub <- emiss %>%
    filter(Emiss_Cat == input$dusty)
    
  leaflet(emiss_sub) %>% 
      addProviderTiles("Stamen.Terrain") %>% 
    addPolygons(weight = 0.5,
                color = input$dusty,
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
  
  #output$acreage <- renderText(
  # acres2 <- playa %>% 
  #  filter(Year == input$shore)
  
  #box(acres2)
  
  
  #)
   
 }
    


# Run the application 
shinyApp(ui = ui, server = server)

