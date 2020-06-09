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
  minus_one = makeIcon(iconUrl = "pfeil_gruen.jpg",
                       iconWidth = 10, iconHeight = 10,
                       iconAnchorX = 0, iconAnchorY = 0),
  zero = makeIcon(iconUrl = "pfeil_gelb.jpg",
                  iconWidth = 10, iconHeight = 10,
                  iconAnchorX = 0, iconAnchorY = 0),
  one = makeIcon(iconUrl = "pfeil_rot.jpg",
                 iconWidth = 10, iconHeight = 10,
                 iconAnchorX = 0, iconAnchorY = 0)
)
return(PfeilIcons)
}

FeuerIcons <- function() {
  FeuerIcons <- iconList(
  feuer_rot = makeIcon(iconUrl = "feuer_1.jpg",
                       iconWidth = 10, iconHeight = 10,
                       iconAnchorX = -2, iconAnchorY = -2),
  feuer_orange = makeIcon(iconUrl = "feuer_2.jpg",
                          iconWidth = 10, iconHeight = 10,
                          iconAnchorX = -2, iconAnchorY = -2),
  feuer_grau = makeIcon(iconUrl = "feuer_grau.jpg",
                        iconWidth = 10, iconHeight = 10,
                        iconAnchorX = -2, iconAnchorY = -2)
)
  return(FeuerIcons)
}

##--------------------------------------------------##
## 2. Selecting data functions                      ##
##--------------------------------------------------##

