library(utils)
library(shiny)
library(leaflet)

shinyUI(fluidPage(
  titlePanel("try issue 2"),
  # titlePanel("try try try"),
  # titlePanel("Pipe Map"),
  leafletOutput('targetData',width=1000,height=800)
  
))

