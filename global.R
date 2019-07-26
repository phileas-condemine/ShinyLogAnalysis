library(shiny)
library(mongolite)
library(data.table)
library(dplyr)
library(plotly)
library(lubridate)
library(htmlwidgets)
library(dygraphs)
library(forcats)
apps = c("cartoIndicateurs","distriPensions")


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
