## --------------------------------------------------##
## CorrelAid Project:                               ##
##    erlassjahr.de                                 ##
## --------------------------------------------------##
##  Graphics Helperfuntions
##  1. Make Icons
##  2. Selecting data functions
## --------------------------------------------------##


## --------------------------------------------------##
## 1. Make Icons                                    ##
## --------------------------------------------------##
import(leaflet)
import(shiny)

# Make IconList
pfeil_icons <- function() {
  pfeil_icons <- iconList(
    minus_one = makeIcon(
      iconUrl = "symbols/pfeil_gruen.jpg",
      iconWidth = 10, iconHeight = 10,
      iconAnchorX = 0, iconAnchorY = 0
    ),
    zero = makeIcon(
      iconUrl = "symbols/pfeil_gelb.jpg",
      iconWidth = 10, iconHeight = 10,
      iconAnchorX = 0, iconAnchorY = 0
    ),
    one = makeIcon(
      iconUrl = "symbols/pfeil_rot.jpg",
      iconWidth = 10, iconHeight = 10,
      iconAnchorX = 0, iconAnchorY = 0
    )
  )
  return(pfeil_icons)
}

feuer_icons <- function() {
  feuer_icons <- iconList(
    feuer_rot = makeIcon(
      iconUrl = "symbols/feuer_rot.jpg",
      iconWidth = 10, iconHeight = 10,
      iconAnchorX = -2, iconAnchorY = -2
    ),
    feuer_orange = makeIcon(
      iconUrl = "symbols/feuer_orange.jpg",
      iconWidth = 10, iconHeight = 10,
      iconAnchorX = -2, iconAnchorY = -2
    ),
    feuer_grau = makeIcon(
      iconUrl = "symbols/feuer_grau.jpg",
      iconWidth = 10, iconHeight = 10,
      iconAnchorX = -2, iconAnchorY = -2
    )
  )
  return(feuer_icons)
}

palette <- function() {
  return(list(
    s_kritisch = "#E61700",
    kritisch = "#FF8040",
    l_kritisch = "#FFCC99",
    n_kritisch = "#C5C7B8",
    k_daten = "#808080",
    nT_analyse = "#F0F2E8",
    risk_fact = "#3D36C7",
    label_color = "#f6f8f8"
  ))
}
## --------------------------------------------------##
## 2. Selecting data functions                      ##
## --------------------------------------------------##
