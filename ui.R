library(utils)
library(shiny)
library(leaflet)
# try issue2 in ui.R


shinyUI(fluidPage(
  titlePanel("try issue 2"),
  # titlePanel("try try try"),
  # titlePanel("Pipe Map"),
  leafletOutput('targetData',width=1000,height=800)
  
))

