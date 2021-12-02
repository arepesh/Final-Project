library(shiny)
library(leaflet)
library(tidyverse)
library(leaflet.extras)
library(rgdal)
library(geojsonio)
library(htmltools)
library(htmlwidgets)
library(stringi)
library(RColorBrewer)

#loading in the data
birthrate <- read_csv("~/STA518/Final-Project/NationalAndStatePregnancy_PublicUse.csv") %>%
  select("state", "year", "pregnancyrate2024" : "pregnancyrate3539") %>%
  filter(year >= 2000, state != "US" & state != "DC")
birthrate <- rename(birthrate, "STUSPS" = "state")
birthrate <- rename(birthrate, "Age20_24" = "pregnancyrate2024")
birthrate <- rename(birthrate, "Age25_29" = "pregnancyrate2529")
birthrate <- rename(birthrate, "Age30_34" = "pregnancyrate3034")
birthrate <- rename(birthrate, "Age35_39" = "pregnancyrate3539")



#read a shapefile
# unziping file
zipF<- file.choose()
outDir <- "C:\\Documents\\STA518"
unzip(zipF,exdir= outDir)

#this is the shape file so that leaflet can read where the states are.
states <- readOGR('C:\\Documents\\STA518\\cb_2019_us_state_5m.shp')

#checking to see if both elements are names the same
is.element(birthrate$STUSPS, states$STUSPS) %>%
  all()

#Merge the datasets
statebirthrate <- merge(birthrate, states, by = 'STUSPS', all.x = FALSE)

#ui shiny app part
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "Selector",
        label = "Please Select an Age Group",
        choices = c("Please Select:","Age20_24", "Age25_29", "Age30_34", "Age35_39"),
        selected = "Please Select:"
  ),),
mainPanel(
  leafletOutput(outputId = "map")
)
)
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(statebirthrate) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
      setView(lng = -96.25, lat = 39.50, zoom = 3)})
}

shinyApp(ui, server)
