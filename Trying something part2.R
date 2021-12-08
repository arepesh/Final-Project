# Read dataset
smoking <- read.csv("US adult smoking by state1.csv", header = TRUE)


states <- shapefile(here::here("cb_2019_us_state_5m/cb_2019_us_state_5m.shp"))

# Merge data
# require(sp)! For spatial dataframe!
smoking.df <- merge(states, smoking, by.x = "NAME", by.y = "state")
class(smoking.df)

# Create palette
pal <- colorBin("Reds", c(0, 30), na.color = "#808080",
                alpha = FALSE, reverse = FALSE)


# UI
ui <- shinyUI(fluidPage(theme = shinytheme("united"),
                        titlePanel(HTML("<h1><center><font size=14> US Adult 
                                        Smoking by State in 2015-2017</font></center></h1>")), 
                        sidebarLayout(
                          sidebarPanel(
                            selectizeInput("stateInput", label = h3("State"),
                                        choices = c("Choose state",
                                                    "Alabama", "Alaska", "Arizona", "Arkansas", "California",
                                                    "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                                                    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
                                                    "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                                                    "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                                                    "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
                                                    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
                                                    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                                                    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                                                    "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
                                        selected = "Choose state"),
                            selectInput("yearInput", label = h3("Year"),
                                        choices = c("Choose year",
                                                    "2015",
                                                    "2016",
                                                    "2017"),
                                        selected = "Choose year")),
                          mainPanel(leafletOutput(outputId = 'map', height = 
                                                    800)))))


# SERVER
server <- shinyServer(function(input, output, session) {
  
  updateSelectizeInput(session, "stateInput", choices = smoking.df$NAME,
                       server = TRUE)
  # selected state
  selectedState <- reactive({
    smoking.df[smoking.df$NAME == input$stateInput, ] 
  })
  # selected year
  selectedYear <- reactive({switch(input$yearInput, 
                                   "2015"=smoking.df$adult_smoking_2015, 
                                   "2016"=smoking.df$adult_smoking_2016, 
                                   "2017"=smoking.df$adult_smoking_2017)
  })
  pal2 <- colorNumeric(palette = "Greens", domain=NULL)
  
  output$map <- renderLeaflet({
    leaflet(smoking.df) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
      addPolygons(data = smoking.df ,fillColor = ~pal2(selectedYear()),
                  popup = paste0("<strong>State: </strong>", 
                                 smoking.df$STUSPS),
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 1)
    
  })
  
  observeEvent(input$stateInput, {
    state_popup <- paste0("<strong>State: </strong>", 
                          selectedState()$NAME, 
                          "<br><strong>% of smoking adults in 2015: </strong>",
                          selectedState()$adult_smoking_2015,
                          "<br><strong>% of smoking adults in 2016: </strong>",
                          selectedState()$adult_smoking_2016,
                          "<br><strong>% of smoking adults in 2017: </strong>",
                          selectedState()$adult_smoking_2017)
    
    leafletProxy("map", data = selectedState()) %>%
      clearGroup(c("state")) %>%
      addPolygons(group ="state",fillColor = "orange",
                  popup = state_popup,
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 5)
  })
  
})


# Run app! 
shinyApp(ui = ui, server = server)