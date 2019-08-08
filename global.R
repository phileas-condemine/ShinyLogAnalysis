library(shiny)
library(mongolite)
library(data.table)
library(dplyr)
library(plotly)
library(lubridate)
library(htmlwidgets)
library(dygraphs)
library(forcats)
library(shinycssloaders)
library(bsplus)
apps = c("cartoIndicateurs","distriPensions","Care-M","condition_de_vie_des_enfants")


dyCrosshair <- function(dygraph, 
                        direction = c("both", "horizontal", "vertical")) {
  dyPlugin(
    dygraph = dygraph,
    name = "Crosshair",
    path = system.file("plugins/crosshair.js", 
                       package = "dygraphs"),
    options = list(direction = match.arg(direction))
  )
}

window_width=30

palettes= c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral")


nms=c("Nombre de clics"="nb_click",
      "Durée de la session"="duration",
      "Plus longue interruption pendant la session"="pause_longue",
      "Nombre de boutons manipulés pendant la session"="buttons_div",
      "Nombre de valeurs de paramètres testées"="choices_div")