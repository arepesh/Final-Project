library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(tidyverse)
library(RColorBrewer)
library(raster)
library(here)


#loading in the data
birthrate <- read_csv(here::here("apps/NationalAndStatePregnancy_PublicUse.csv")) %>%
  dplyr::select("state", "year", "pregnancyrate2024" : "pregnancyrate3539") %>%
  filter(year >= 2000, state != "US" & state != "DC")
birthrate <- rename(birthrate, "Age20_24" = "pregnancyrate2024")
birthrate <- rename(birthrate, "Age25_29" = "pregnancyrate2529")
birthrate <- rename(birthrate, "Age30_34" = "pregnancyrate3034")
birthrate <- rename(birthrate, "Age35_39" = "pregnancyrate3539")

pivotbirthrate <- birthrate %>%
  pivot_longer(!state:year, names_to = "group", values_to = "rate")

birthrate2 <- pivotbirthrate %>%
  unite("age_year", group:year, sep = "_")

birthrate3 <- birthrate2 %>%
  pivot_wider(names_from = age_year, values_from = rate)

state <- shapefile(here::here("apps/cb_2019_us_state_5m/cb_2019_us_state_5m.shp"))

#checking to see if both elements are names the same
is.element(birthrate3$state, state$STUSPS) %>%
  all()

#merging the two data sets
statebirthrate <- merge(state, birthrate, by.x = 'STUSPS', by.y = 'state', all.x = FALSE, duplicateGeoms = TRUE)

# Create palette
pal2 <- colorNumeric(palette = "Purples", domain=NULL)

#Shiny UI code

# UI
ui <- shinyUI(fluidPage(theme = shinytheme("united"),
                        titlePanel(HTML("<h1><center><font size=14> Pregnancy Rate by State in 2000-2017</font></center></h1>")), 
                        sidebarLayout(
                          sidebarPanel(
                            selectizeInput("stateInput", label = h3("State"),
                                           choices = c("Choose state",
                                                       "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                                                       "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                                                       "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                                                       "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                                                       "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
                                           selected = "Choose State:"),
                            selectInput("ageyearInput", label = h3("year"),
                                        choices = c("Choose Age year group:", "Age20_24_2000", "Age25_29_2000", "Age30_34_2000", "Age35_39_2000",
                                                    "Age20_24_2005", "Age25_29_2005", "Age30_34_2005", "Age35_39_2005",
                                                    "Age20_24_2006", "Age25_29_2006", "Age30_34_2006", "Age35_39_2006",
                                                    "Age20_24_2007", "Age25_29_2007", "Age30_34_2007", "Age35_39_2007",
                                                    "Age20_24_2008", "Age25_29_2008", "Age30_34_2008", "Age35_39_2008",
                                                    "Age20_24_2009", "Age25_29_2009", "Age30_34_2009", "Age35_39_2009",
                                                    "Age20_24_2010", "Age25_29_2010", "Age30_34_2010", "Age35_39_2010",
                                                    "Age20_24_2011", "Age25_29_2011", "Age30_34_2011", "Age35_39_2011",
                                                    "Age20_24_2012", "Age25_29_2012", "Age30_34_2012", "Age35_39_2012",
                                                    "Age20_24_2013", "Age25_29_2013", "Age30_34_2013", "Age35_39_2013",
                                                    "Age20_24_2014", "Age25_29_2014", "Age30_34_2014", "Age35_39_2014",
                                                    "Age20_24_2015", "Age25_29_2015", "Age30_34_2015", "Age35_39_2015",
                                                    "Age20_24_2016", "Age25_29_2016", "Age30_34_2016", "Age35_39_2016",
                                                    "Age20_24_2017", "Age25_29_2017", "Age30_34_2017", "Age35_39_2017"),
                                        selected = "Choose Age year group:")),
                          mainPanel(leafletOutput(outputId = 'map', height = 800) 
                          ))
))



# SERVER
server <- shinyServer(function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(statebirthrate) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      setView(lng = -98.583, lat = 39.833, zoom = 4) 
    
  })
  # observers
  
  # selected state
  selectedState <- reactive({
    
    updateSelectizeInput(session, "stateInput", choices = statebirthrate$STUSPS,
                         server = TRUE)
  })
  # selected state
  selectedState <- reactive({
    statebirthrate[statebirthrate$STUSPS == input$stateInput, ] 
  })
  
  # selected year
  selectedYear <- reactive({switch(input$ageyearInput,
                                   "Age20_24_2000" = statebirthrate$Age20_24_2000, "Age25_29_2000" = statebirthrate$Age25_29_2000, "Age30_34_2000" = statebirthrate$Age30_34_2000, "Age35_39_2000" = statebirthrate$Age35_39_2000,
                                   "Age20_24_2005" = statebirthrate$Age20_24_2005, "Age25_29_2005" = statebirthrate$Age25_29_2005, "Age30_34_2005" = statebirthrate$Age30_34_2005, "Age35_39_2005" = statebirthrate$Age35_39_2005,
                                   "Age20_24_2005" = statebirthrate$Age20_24_2006, "Age25_29_2006" = statebirthrate$Age25_29_2006, "Age30_34_2006" = statebirthrate$Age30_34_2006, "Age35_39_2006" = statebirthrate$Age35_39_2006,
                                   "Age20_24_2007" = statebirthrate$Age20_24_2007, "Age25_29_2007" = statebirthrate$Age25_29_2007, "Age30_34_2007" = statebirthrate$Age30_34_2007, "Age35_39_2007" = statebirthrate$Age35_39_2007,
                                   "Age20_24_2008" = statebirthrate$Age20_24_2008, "Age25_29_2008" = statebirthrate$Age25_29_2008, "Age30_34_2008" = statebirthrate$Age30_34_2008, "Age35_39_2008" = statebirthrate$Age35_39_2008,
                                   "Age20_24_2009" = statebirthrate$Age20_24_2009, "Age25_29_2009" = statebirthrate$Age25_29_2009, "Age30_34_2009" = statebirthrate$Age30_34_2009, "Age35_39_2009" = statebirthrate$Age35_39_2009,
                                   "Age20_24_2010" = statebirthrate$Age20_24_2010, "Age25_29_2010" = statebirthrate$Age25_29_2010, "Age30_34_2010" = statebirthrate$Age30_34_2010, "Age35_39_2010" = statebirthrate$Age35_39_2010,
                                   "Age20_24_2011" = statebirthrate$Age20_24_2011, "Age25_29_2011" = statebirthrate$Age25_29_2011, "Age30_34_2011" = statebirthrate$Age30_34_2011, "Age35_39_2011" = statebirthrate$Age35_39_2011,
                                   "Age20_24_2012" = statebirthrate$Age20_24_2012, "Age25_29_2012" = statebirthrate$Age25_29_2012, "Age30_34_2012" = statebirthrate$Age30_34_2012, "Age35_39_2012" = statebirthrate$Age35_39_2012,
                                   "Age20_24_2013" = statebirthrate$Age20_24_2013, "Age25_29_2013" = statebirthrate$Age25_29_2013, "Age30_34_2013" = statebirthrate$Age30_34_2013, "Age35_39_2013" = statebirthrate$Age35_39_2013,
                                   "Age20_24_2014" = statebirthrate$Age20_24_2014, "Age25_29_2014" = statebirthrate$Age25_29_2014, "Age30_34_2014" = statebirthrate$Age30_34_2014, "Age35_39_2014" = statebirthrate$Age35_39_2014,
                                   "Age20_24_2015" = statebirthrate$Age20_24_2015, "Age25_29_2015" = statebirthrate$Age25_29_2015, "Age30_34_2015" = statebirthrate$Age30_34_2015, "Age35_39_2015" = statebirthrate$Age35_39_2015,
                                   "Age20_24_2016" = statebirthrate$Age20_24_2016, "Age25_29_2016" = statebirthrate$Age25_29_2016, "Age30_34_2016" = statebirthrate$Age30_34_2016, "Age35_39_2016" = statebirthrate$Age35_39_2016,
                                   "Age20_24_2017" = statebirthrate$Age20_24_2017, "Age25_29_2017" = statebirthrate$Age25_29_2017, "Age30_34_2017" = statebirthrate$Age30_34_2017, "Age35_39_2017" = statebirthrate$Age35_39_2017)
  })
  
  
  output$map <- renderLeaflet({
    leaflet(statebirthrate) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
      addPolygons(data = statebirthrate ,fillColor = ~pal2(selectedYear()),
                  popup = paste0("<strong>State: </strong>", 
                                 statebirthrate$STUSPS),
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 1) })
  
  observeEvent(input$stateInput, {
    state_popup <- paste0("<strong> State: </strong>", 
                          selectedState()$STUSPS, 
                          "<br><strong> Pregnancy rate for 20-24 in 2000: </strong>",
                          selectedState()$Age20_24_2000,
                          "<br><strong> Pregnancy rate for 25-29 in 2000: </strong>",
                          selectedState()$Age25_29_2000,
                          "<br><strong> Pregnancy rate for 30-34 in 2000: </strong>",
                          selectedState()$Age30_34_2000,
                          "<br><strong> Pregnancy rate for 35-39 in 2000: </strong>",
                          selectedState()$Age35_39_2000,
                          "<br><strong> Pregnancy rate for 20-24 in 2005: </strong>",
                          selectedState()$Age20_24_2005,
                          "<br><strong> Pregnancy rate for 25-29 in 2005: </strong>",
                          selectedState()$Age25_29_2005,
                          "<br><strong> Pregnancy rate for 30-34 in 2005: </strong>",
                          selectedState()$Age30_34_2005,
                          "<br><strong> Pregnancy rate for 35-39 in 2005: </strong>",
                          selectedState()$Age35_39_2005,
                          "<br><strong> Pregnancy rate for 20-24 in 2006: </strong>",
                          selectedState()$Age20_24_2006,
                          "<br><strong> Pregnancy rate for 25-29 in 2006: </strong>",
                          selectedState()$Age25_29_2006,
                          "<br><strong> Pregnancy rate for 30-34 in 2006: </strong>",
                          selectedState()$Age30_34_2006,
                          "<br><strong> Pregnancy rate for 35-39 in 2006: </strong>",
                          selectedState()$Age35_39_2006,
                          "<br><strong> Pregnancy rate for 20-24 in 2007: </strong>",
                          selectedState()$Age20_24_2007,
                          "<br><strong> Pregnancy rate for 25-29 in 2007: </strong>",
                          selectedState()$Age25_29_2007,
                          "<br><strong> Pregnancy rate for 30-34 in 2007: </strong>",
                          selectedState()$Age30_34_2007,
                          "<br><strong> Pregnancy rate for 35-39 in 2007: </strong>",
                          selectedState()$Age35_39_2007,
                          "<br><strong> Pregnancy rate for 20-24 in 2008: </strong>",
                          selectedState()$Age20_24_2008,
                          "<br><strong> Pregnancy rate for 25-29 in 2008: </strong>",
                          selectedState()$Age25_29_2008,
                          "<br><strong> Pregnancy rate for 30-34 in 2008: </strong>",
                          selectedState()$Age30_34_2008,
                          "<br><strong> Pregnancy rate for 35-39 in 2008: </strong>",
                          selectedState()$Age35_39_2008,
                          "<br><strong> Pregnancy rate for 20-24 in 2009: </strong>",
                          selectedState()$Age20_24_2009,
                          "<br><strong> Pregnancy rate for 25-29 in 2009: </strong>",
                          selectedState()$Age25_29_2009,
                          "<br><strong> Pregnancy rate for 30-34 in 2009: </strong>",
                          selectedState()$Age30_34_2009,
                          "<br><strong> Pregnancy rate for 35-39 in 2009: </strong>",
                          selectedState()$Age35_39_2009,
                          "<br><strong> Pregnancy rate for 20-24 in 2010: </strong>",
                          selectedState()$Age20_24_2010,
                          "<br><strong> Pregnancy rate for 25-29 in 2010: </strong>",
                          selectedState()$Age25_29_2010,
                          "<br><strong> Pregnancy rate for 30-34 in 2010: </strong>",
                          selectedState()$Age30_34_2010,
                          "<br><strong> Pregnancy rate for 35-39 in 2010: </strong>",
                          selectedState()$Age35_39_2010,
                          "<br><strong> Pregnancy rate for 20-24 in 2011: </strong>",
                          selectedState()$Age20_24_2011,
                          "<br><strong> Pregnancy rate for 25-29 in 2011: </strong>",
                          selectedState()$Age25_29_2011,
                          "<br><strong> Pregnancy rate for 30-34 in 2011: </strong>",
                          selectedState()$Age30_34_2011,
                          "<br><strong> Pregnancy rate for 35-39 in 2011: </strong>",
                          selectedState()$Age35_39_2011,
                          "<br><strong> Pregnancy rate for 20-24 in 2012: </strong>",
                          selectedState()$Age20_24_2012,
                          "<br><strong> Pregnancy rate for 25-29 in 2012: </strong>",
                          selectedState()$Age25_29_2012,
                          "<br><strong> Pregnancy rate for 30-34 in 2012: </strong>",
                          selectedState()$Age30_34_2012,
                          "<br><strong> Pregnancy rate for 35-39 in 2012: </strong>",
                          selectedState()$Age35_39_2012,
                          "<br><strong> Pregnancy rate for 20-24 in 2013: </strong>",
                          selectedState()$Age20_24_2013,
                          "<br><strong> Pregnancy rate for 25-29 in 2013: </strong>",
                          selectedState()$Age25_29_2013,
                          "<br><strong> Pregnancy rate for 30-34 in 2013: </strong>",
                          selectedState()$Age30_34_2013,
                          "<br><strong> Pregnancy rate for 35-39 in 2013: </strong>",
                          selectedState()$Age35_39_2013,
                          "<br><strong> Pregnancy rate for 20-24 in 2014: </strong>",
                          selectedState()$Age20_24_2014,
                          "<br><strong> Pregnancy rate for 25-29 in 2014: </strong>",
                          selectedState()$Age25_29_2014,
                          "<br><strong> Pregnancy rate for 30-34 in 2014: </strong>",
                          selectedState()$Age30_34_2014,
                          "<br><strong> Pregnancy rate for 35-39 in 2014: </strong>",
                          selectedState()$Age35_39_2014,
                          "<br><strong> Pregnancy rate for 20-24 in 2015: </strong>",
                          selectedState()$Age20_24_2015,
                          "<br><strong> Pregnancy rate for 25-29 in 2015: </strong>",
                          selectedState()$Age25_29_2015,
                          "<br><strong> Pregnancy rate for 30-34 in 2015: </strong>",
                          selectedState()$Age30_34_2015,
                          "<br><strong> Pregnancy rate for 35-39 in 2015: </strong>",
                          selectedState()$Age35_39_2015,
                          "<br><strong> Pregnancy rate for 20-24 in 2016: </strong>",
                          selectedState()$Age20_24_2016,
                          "<br><strong> Pregnancy rate for 25-29 in 2016: </strong>",
                          selectedState()$Age25_29_2016,
                          "<br><strong> Pregnancy rate for 30-34 in 2016: </strong>",
                          selectedState()$Age30_34_2016,
                          "<br><strong> Pregnancy rate for 35-39 in 2016: </strong>",
                          selectedState()$Age35_39_2016,
                          "<br><strong> Pregnancy rate for 20-24 in 2017: </strong>",
                          selectedState()$Age20_24_2017,
                          "<br><strong> Pregnancy rate for 25-29 in 2017: </strong>",
                          selectedState()$Age25_29_2017,
                          "<br><strong> Pregnancy rate for 30-34 in 2017: </strong>",
                          selectedState()$Age30_34_2017,
                          "<br><strong> Pregnancy rate for 35-39 in 2017: </strong>",
                          selectedState()$Age35_39_2017)
    
    leafletProxy("map", data = selectedState()) %>%
      clearGroup(c("STUSPS")) %>%
      addPolygons(group ="STUSPS",fillColor = "blue",
                  popup = state_popup,
                  color = "#BDBDC3",
                  fillOpacity = 0.8,
                  weight = 5)
  })
})

shinyApp(ui = ui, server = server)
