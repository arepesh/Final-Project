library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(tidyverse)
library(RColorBrewer)
library(raster)


#loading in the data
birthrate <- read_csv("NationalAndStatePregnancy_PublicUse.csv") %>%
  dplyr::select("state", "year", "pregnancyrate2024" : "pregnancyrate3539") %>%
  filter(year == 2000, state != "US" & state != "DC")
birthrate <- rename(birthrate, "Age20_24" = "pregnancyrate2024")
birthrate <- rename(birthrate, "Age25_29" = "pregnancyrate2529")
birthrate <- rename(birthrate, "Age30_34" = "pregnancyrate3034")
birthrate <- rename(birthrate, "Age35_39" = "pregnancyrate3539")

state <- shapefile(here::here("apps/cb_2019_us_state_5m/cb_2019_us_state_5m.shp"))

#checking to see if both elements are names the same
is.element(birthrate$state, state$STUSPS) %>%
  all()

#merging the two data sets
statebirthrate <- merge(state, birthrate, by.x = 'STUSPS', by.y = 'state', all.x = FALSE, duplicateGeom = TRUE)

# Create palette
pal2 <- colorNumeric(palette = "Purples", domain=NULL)

#Shiny UI code

# UI
ui <- shinyUI(fluidPage(
                        titlePanel(HTML("<h1><center><font size=14> Pregnancy Rate by State in 2000</font></center></h1>")), 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("ageInput", label = h3("Age Group"),
                                        choices = c("Choose Age Group:", 
                                                    "Age20_24", 
                                                    "Age25_29", 
                                                    "Age30_34", 
                                                    "Age35_39"),
                                        selected = "Choose Age Group:"),),
                          mainPanel(leafletOutput(outputId = 'map', height = 800) 
                          )
)))



# SERVER
server <- shinyServer(function(input, output,session) {
  output$map <- renderLeaflet({
    leaflet(statebirthrate) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      setView(lng = -98.583, lat = 39.833, zoom = 4) 
    
  })
  
  selectAge <- reactive({switch(input$ageInput,
                                   "Age20_24" = statebirthrate$Age20_24, 
                                   "Age25_29" = statebirthrate$Age25_29, 
                                   "Age30_34" = statebirthrate$Age30_34,
                                   "Age35_39" = statebirthrate$Age35_39)
  })
  
  output$map <- renderLeaflet({
    leaflet(statebirthrate) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
      addPolygons(data = statebirthrate,fillColor = ~pal2(selectAge()),
                  popup = paste0("<strong> State: </strong>", 
                                 statebirthrate$STUSPS,
                                 "<br><strong> Pregnancy Rate: </strong>", 
                                 selectAge()),
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 1)
  })
})

shinyApp(ui = ui, server = server)
