library(shiny)
library(tidyverse)
library(DT)
library(rgdal)

birthratefilter <- read_csv("NationalAndStatePregnancy_PublicUse.csv") %>%
  dplyr::select("state", "year", "pregnancyrate2024" : "pregnancyrate3539") %>%
  filter(year >= 2000, state != "US" & state != "DC")
birthratefilter <- rename(birthratefilter, "Age20_24" = "pregnancyrate2024")
birthratefilter <- rename(birthratefilter, "Age25_29" = "pregnancyrate2529")
birthratefilter <- rename(birthratefilter, "Age30_34" = "pregnancyrate3034")
birthratefilter <- rename(birthratefilter, "Age35_39" = "pregnancyrate3539")


uifilter <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("ageInput", label = ("Age Group"),
                  choices = c("Choose Age Group:", 
                              "Age20_24", 
                              "Age25_29", 
                              "Age30_34", 
                              "Age35_39"),
                  selected = "Choose Age Group:"),
    selectizeInput("yearInput", 
                   "Select Year", 
                   choices = c("2000", "2005", "2006", "2007",
                               "2008", "2009", "2010", "2011",
                               "2012", "2013", "2014", "2015",
                               "2016", "2017"), 
                   multiple = FALSE),),
    mainPanel(
      h2("The Pregnancy Data"),
      dataTableOutput("mytable")))
  )



serverfilter <- function(input, output) {
  
  output$mytable = renderDataTable({
    dt <- birthratefilter[birthratefilter$year == input$yearInput, ]
    dt[, c("year", input$ageInput)]
  })
  }

shinyApp(ui = uifilter, server = serverfilter)