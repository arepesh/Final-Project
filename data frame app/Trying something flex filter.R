library(shiny)
library(tidyverse)
library(DT)
library(rgdal)

birthratefilter <- read_csv("NationalAndStatePregnancy_PublicUse.csv") %>%
  dplyr::select("state", "year", "pregnancyrate2024" : "pregnancyrate3539") %>%
  filter(year >= 2000, state != "US" & state != "DC")
birthratefilter <- rename(birthrate, "20_24" = "pregnancyrate2024")
birthratefilter <- rename(birthrate, "25_29" = "pregnancyrate2529")
birthratefilter <- rename(birthrate, "30_34" = "pregnancyrate3034")
birthratefilter <- rename(birthrate, "35_39" = "pregnancyrate3539")

pivotbirthrate <- birthratefilter %>%
  pivot_longer(!state:year, names_to = "group", values_to = "rate")


uifilter <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput("year", 
                     "Select Year:", 
                     choices = unique(pivotbirthrate$year), 
                     multiple = FALSE),
      
      selectizeInput("agegroup", "Select Age Group:", choices = unique(pivotbirthrate$group), multiple = FALSE)
      ),
    mainPanel(
      h2("The Pregnancy Data"),
      dataTableOutput("mytable")))
  )



serverfilter <- function(input, output, session) {
  choicevec1 <- reactive({
    pivotbirthrate %>%
      filter(year == input$year, group == input$agegroup ) 
  })
  output$mytable = renderDataTable({choicevec1()})
  }

shinyApp(ui = uifilter, server = serverfilter)