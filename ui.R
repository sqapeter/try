library(utils)
library(shiny)
library(leaflet)

shinyUI(fluidPage(
  titlePanel("Pipe Map"),
  leafletOutput('targetData',width=1000,height=800)
  
))

