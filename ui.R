library(utils)
library(shiny)
library(leaflet)
# try issue1 12/26 AM 09:35

shinyUI(fluidPage(
  titlePanel("try try try"),
  # titlePanel("Pipe Map"),
  leafletOutput('targetData',width=1000,height=800)
  
))

