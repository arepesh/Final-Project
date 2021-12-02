
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

#loading in the data(This is were I could put my data when doing mine)
dat <- read.csv('https://static.lib.virginia.edu/statlab/materials/data/state_electricity_data_2018.csv')
head(dat)

#get the map centered over the united states (the provider$CartonDB tells what style of map we are looking at)
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
  setView(lng = -96.25, lat = 39.50, zoom = 4)
m

# unziping file
zipF<- file.choose()
outDir <- "C:\\Documents\\STA518"
unzip(zipF,exdir= outDir)

#this is the shape file so that leaflet can read where the states are.
states <- readOGR('C:\\Documents\\STA518\\cb_2019_us_state_5m.shp')

#adding lines around the map
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
  setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
  addPolygons(data = states, weight = 1)
m

#checking to see if both elements are names the same
is.element(dat$NAME, states$NAME) %>%
  all()

#merging the two data sets
states <- merge(states, dat, by = 'NAME', all.x = F)

#color for contunuout values
paletteNum <- colorNumeric('Blues', domain = states$centskWh)

#making the color range
costBins <- c(7:19, Inf)
paletteBinned <- colorBin('YlGnBu', domain = states$centskWh, bins = costBins)

#making the map
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
  setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
  addPolygons(data = states,
              
              # state border stroke color
              color = 'white', 
              
              # soften the weight of the state borders
              weight = 1, 
              
              # values >1 simplify the polygons' lines for less detail but faster loading
              smoothFactor = .3, 
              
              # set opacity of polygons
              fillOpacity = .95, 
              
              # specify that the each state should be colored per paletteNum()
              fillColor = ~paletteNum(states$centskWh))

m

#adding labels to the states
stateLabels <- sprintf('<b>%s</b><br/>%g cents/kWh',
                       states$NAME, states$centskWh) %>%
  lapply(function(x) HTML(x))

states <- cbind(states, matrix(stateLabels, ncol = 1, dimnames = list(c(), c('stateLabels'))))

#final map fingers crossed 
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  setView(lng = -96.25, lat = 39.50, zoom = 3.5) %>%
  addPolygons(data = states,
              color = 'white',
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum(states$centskWh),
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
            values = states$centskWh, 
            title = '<small>2018 Avg. Electricity Cost<br>(cents/kWh | source: US EIA)</small>',
            position = 'bottomleft')
m



