##--------------------------------------------------##
## CorrelAid Project:                               ##
##    erlassjahr.de                                 ##
##--------------------------------------------------##
##  Graphics Helperfuntions    
##  1. Make Icons
##  2. Selecting data functions
##--------------------------------------------------##


##--------------------------------------------------##
## 1. Make Icons                                    ##
##--------------------------------------------------##
import(leaflet)
import(shiny)

# Make IconList
PfeilIcons <- function() {
PfeilIcons <- iconList(
  minus_one = makeIcon(iconUrl = "symbols/pfeil_gruen.jpg",
                       iconWidth = 10, iconHeight = 10,
                       iconAnchorX = 0, iconAnchorY = 0),
  zero = makeIcon(iconUrl = "symbols/pfeil_gelb.jpg",
                  iconWidth = 10, iconHeight = 10,
                  iconAnchorX = 0, iconAnchorY = 0),
  one = makeIcon(iconUrl = "symbols/pfeil_rot.jpg",
                 iconWidth = 10, iconHeight = 10,
                 iconAnchorX = 0, iconAnchorY = 0)
)
return(PfeilIcons)
}

FeuerIcons <- function() {
  FeuerIcons <- iconList(
  feuer_rot = makeIcon(iconUrl = "symbols/feuer_rot.jpg",
                       iconWidth = 10, iconHeight = 10,
                       iconAnchorX = -2, iconAnchorY = -2),
  feuer_orange = makeIcon(iconUrl = "symbols/feuer_orange.jpg",
                          iconWidth = 10, iconHeight = 10,
                          iconAnchorX = -2, iconAnchorY = -2),
  feuer_grau = makeIcon(iconUrl = "symbols/feuer_grau.jpg",
                        iconWidth = 10, iconHeight = 10,
                        iconAnchorX = -2, iconAnchorY = -2)
)
  return(FeuerIcons)
}

##--------------------------------------------------##
## 2. Selecting data functions                      ##
##--------------------------------------------------##

