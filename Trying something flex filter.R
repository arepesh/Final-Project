library(shiny)
library(tidyverse)
library(DT)

birthrate <- read_csv("~/STA518/Final-Project/NationalAndStatePregnancy_PublicUse.csv") %>%
  select("state", "year", "pregnancyrate2024" : "pregnancyrate3539") %>%
  filter(year >= 2000, state != "US" & state != "DC")
birthrate <- rename(birthrate, "STUSPS" = "state")
birthrate <- rename(birthrate, "20_24" = "pregnancyrate2024")
birthrate <- rename(birthrate, "25_29" = "pregnancyrate2529")
birthrate <- rename(birthrate, "30_34" = "pregnancyrate3034")
birthrate <- rename(birthrate, "35_39" = "pregnancyrate3539")

pivotbirthrate <- birthrate %>%
  pivot_longer(!STUSPS:year, names_to = "group", values_to = "rate")

# unziping file
zipF<- file.choose()
outDir <- "C:\\Documents\\STA518"
unzip(zipF,exdir= outDir)

#this is the shape file so that leaflet can read where the states are.
states <- readOGR('C:\\Documents\\STA518\\cb_2019_us_state_5m.shp')

#checking to see if both elements are names the same
is.element(pivotbirthrate$STUSPS, states$STUSPS) %>%
  all()

#Merge the datasets
statebirthrate <- merge(pivotbirthrate, states, by = 'STUSPS', all.x = FALSE)
statebirthrate <- rename(statebirthrate, "state" = "STUSPS")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput("year", "Select Year:", choices = unique(pivotbirthrate$year), multiple = FALSE),
      selectizeInput("agegroup", "Select Age Group:", choices = unique(pivotbirthrate$group), multiple = FALSE)
      ),
    mainPanel(
      h2("The Pregnancy Data"),
      dataTableOutput("mytable")))
  )



server <- function(input, output, session) {
  choicevec1 <- reactive({
    statebirthrate %>%
      filter(year == input$year, group == input$agegroup ) 
  })
  output$mytable = renderDataTable({choicevec1()})
  }

shinyApp(ui, server)