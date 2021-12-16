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
library(raster)

birthratemap1 <- read_csv("NationalAndStatePregnancy_PublicUse.csv")

birthratemap1 <- birthratemap1 %>%
  dplyr::select("state", "year", "pregnancyrate2024":"pregnancyrate3539") %>%
  filter(year >= 2000, state != "US" & state != "DC")

head(birthratemap1)

#this is the shape file so that leaflet can read where the states are.

statesmap1 <- shapefile(here::here("cb_2019_us_state_5m/cb_2019_us_state_5m.shp"))

#checking to see if both elements are names the same
is.element(birthratemap1$state, statesmap1$STUSPS) %>%
  all()

#merging the two data sets
statesmap1 <- merge(statesmap1, birthratemap1, by.x = 'STUSPS', by.y = 'state', all.x = FALSE, duplicateGeoms = TRUE)

#color for contunuout values
paletteNum <- colorNumeric('Blues', domain = statesmap1$pregnancyrate2024)

#making the color range
costBins <- c(0:250, Inf)
paletteBinned <- colorBin('YlGnBu', domain = statesmap1$pregnancyrate2024, bins = costBins)

#adding labels to the states
stateLabels <- sprintf('<b>%s</b><br/>%g pregnancyrate2024',
                       statesmap1$STUSPS, statesmap1$pregnancyrate2024) %>%
  lapply(function(x) HTML(x))

states <- cbind(statesmap1, matrix(stateLabels, ncol = 1, dimnames = list(c(), c('stateLabels'))))

#final map fingers crossed 
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  setView(lng = -96.25, lat = 39.50, zoom = 3.5) %>%
  addPolygons(data = statesmap1,
              color = 'white',
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum(statesmap1$pregnancyrate2024),
              label = ~stateLabels,
              labelOptions = labelOptions(
                style = list(color = 'gray30'),
                textsize = '10px'),
              highlightOptions = highlightOptions(
                weight = 3,
                color = 'dodgerblue'
              )
  ) %>%
  addLegend(pal = paletteNum, 
            values = statesmap1$pregnancyrate2024, 
            title = '<small> pregnancy rate </small>',
            position = 'bottomleft')

map