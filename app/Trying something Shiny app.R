library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(tidyverse)
library(RColorBrewer)
library(raster)


#loading in the data
birthrate <- read_csv("app/NationalAndStatePregnancy_PublicUse.csv") %>%
  select("state", "year", "pregnancyrate2024" : "pregnancyrate3539") %>%
  filter(year >= 2000, state != "US" & state != "DC")
birthrate <- rename(birthrate, "Age20_24" = "pregnancyrate2024")
birthrate <- rename(birthrate, "Age25_29" = "pregnancyrate2529")
birthrate <- rename(birthrate, "Age30_34" = "pregnancyrate3034")
birthrate <- rename(birthrate, "Age35_39" = "pregnancyrate3539")

#read a shapefile
# unziping file
#zipF<- file.choose()
#outDir <- "C:\\Documents\\STA518"
#unzip(zipF,exdir= outDir)

#this is the shape file so that leaflet can read where the states are.

state <- shapefile(here::here("app/cb_2019_us_state_5m/cb_2019_us_state_5m.shp"))

#checking to see if both elements are names the same
is.element(birthrate$state, state$STUSPS) %>%
  all()

#merging the two data sets
statebirthrate <- merge(state, birthrate, by.x = 'STUSPS', by.y = 'state', duplicateGeoms = TRUE, all.x = FALSE)

#Shiny UI code

ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput("yearInput", 
                     "Select Year:", 
                     choices = unique(statebirthrate$year),
                     multiple = FALSE),
    selectInput(
        inputId = "ageInput",
        label = "Please Select an Age Group",
        choices = c("Age20_24", "Age25_29", "Age30_34", "Age35_39")
      ),),
    mainPanel(
      leafletOutput(outputId = 'map', height = 800)
    )
  )
))

server <- shinyServer(function(input, output, session) {
  updateSelectizeInput(session, "yearInput", choices = statebirthrate$year, server = TRUE)
  
  selectedYear <- reactive({
    statebirthrate[statebirthrate$year == input$yearInput, ]
  })
  
  selectedAge <- reactive({switch(input$ageInput,
    "20-24" = statebirthrate$Age20_24,
    "25-29" = statebirthrate$Age25_29,
    "30-34" = statebirthrate$Age30_34,
    "35-39" = statebirthrate$Age35_39)
    })
  
  pal2 <- colorNumeric(palette = "Blues", domain = NULL)
  
  output$map <- renderLeaflet({
    leaflet(statebirthrate) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
      setView(lng = -96.25, lat = 39.50, zoom = 3) %>%
      addPolygons(data = statebirthrate, fillColor = ~pal2(selectedAge()),
                  popup = paste0("<strong> Year: </strong>",
                                 selectedYear()$year),
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 1)
    })
  
  observeEvent(input$ageInput, {
    state_popup <- paste0("<strong> Year: </strong>",
                    selectedYear()$year,
                    "<br><strong>% Ages 20-24: </stronger>",
                    selectedYear()$Age20_24,
                    "<br><strong>% Ages 25-29: </stronger>",
                    selectedYear()$Age25_29,
                    "<br><strong>% Ages 30-34: </stronger>",
                    selectedYear()$Age30_34,
                    "<br><strong>% Ages 35-39: </stronger>",
                    selectedYear()$Age35_39)
    
    leafletProxy("map", data = selectedYear()) %>%
      clearGroup(c("year")) %>%
      addPolygons(group = "year", fillColor = "orange",
                  popup = state_popup,
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 5)})
  })


shinyApp(ui = ui, server = server)






