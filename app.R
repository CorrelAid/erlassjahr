##--------------------------------------------------##
## CorrelAid Project:                               ##
##    erlassjahr.de                                 ##
##--------------------------------------------------##
##  Shiny UI    
##  1. Prerequisits
##  2. Load Data
##  3. UI Interface
##  4. Server
##  5. Run app
##--------------------------------------------------##


##--------------------------------------------------##
## Prerequisits                                     ##
##--------------------------------------------------##

# install.packages("reactlog")
library(reactlog)
options(shiny.reactlog=TRUE) # package to track dependencies of shiny app

library(shiny)
library(DT)
library(rgdal)
library(leaflet)
library(maps) # For Capital city coordinates
library(dplyr) # For Data Manipulation
library(english) # Transform numbers to words
library(plyr) # to use the revalue function (can be edited to rely on dplyr) -> TO DO: AS
library(tidyverse)
library("tidylog", warn.conflicts = FALSE) # feedback on join functions

##--------------------------------------------------##
## Load Data                                        ##
##--------------------------------------------------##

# Map Data / Shape File
shape_path <- "TM_WORLD_BORDERS_SIMPL-0.3.shp"
encoding <- "UTF-8"

map <- readOGR(dsn=path.expand(shape_path), layer="TM_WORLD_BORDERS_SIMPL-0.3", encoding = encoding)

# Load Data
load("sr20_erlassjahr.RData")

data <- sr20_erlassjahr


data$country[c(9, 10)]  <- c("Australia", "Austria")
data$debt_sit_cat2[c(9, 10)] <- c(-1, -1)

data[data==""]<-NA

# Combine data:
map <- sp::merge(map, data, by.x="ISO3", duplicateGeoms=TRUE)
#map@data <- inner_join(data[ordercounties,], map@data, by = "ISO3")

##--------------------------------------------------##
## Functions                                        ##
##--------------------------------------------------##

# Make IconList
PfeilIcons <- iconList(
  minus_one = makeIcon(iconUrl = "pfeil_rot.jpg",
                       iconWidth = 10, iconHeight = 10,
                       iconAnchorX = 0, iconAnchorY = 0),
  zero = makeIcon(iconUrl = "pfeil_gelb.jpg",
                  iconWidth = 10, iconHeight = 10,
                  iconAnchorX = 0, iconAnchorY = 0),
  one = makeIcon(iconUrl = "pfeil_gruen.jpg",
                 iconWidth = 10, iconHeight = 10,
                 iconAnchorX = 0, iconAnchorY = 0)
)

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


##--------------------------------------------------##
## User Interface Shiny                             ##
##--------------------------------------------------##

# Install log file for debugging:
options(shiny.reactlog=TRUE)

ui <- fluidPage(
  #titlePanel(title = h1("Erlassjahr app", style = "color:#3474A7", align = "left")),
  
  #layout with main area and side bar
  sidebarLayout(
    mainPanel(
      div(class="outer",
          tags$style(type = "text/css", 
                     ".outer {position: fixed; top: 0; 
                     left: 0; right: 0; bottom: 0; padding: 0}"), # overflow:   hidden;
          
          
          
          leafletOutput(outputId = "map1", height = "100%", width = "100%"))
      
      ),
    
    # Create movable fixed (absolute) side panel
    absolutePanel(
      top = 100, left = 10, width = 250, fixed=TRUE,
      draggable = TRUE, height = "auto",
      
      # dropdown input selection Debt Indicator
      selectInput(
        inputId = "var_debtindikator",
        label = "Schuldenindikator Wählen",
        choices =  list(`Aggregierte Indikatoren` = "debt_sit_cat2",
                        `Öffentliche Schulden / BIP` = "public_debt_bip2", 
                        `Öffentliche Schulden / Staatseinnahmen` = "public_debt_state_rev2", 
                        `Auslandsschuldenstand / BIP` = "foreign_debt_bip2", 
                        `Auslandsschuldenstand / Exporteinnahmen` = "foreign_debt_exp2", 
                        `Auslandsschuldendienst / Exporteinnahmen` = "external_debt_service_exp2"
        )
      ), 
      
      # drop down input selection income category
      selectInput("var_income",
                  "Einkommenskategorie wählen",
                  choices =  list(`Alle` = "Alle",
                                  `geringes Einkommen` = "l",
                                  `mittleres Einkommen, unterer Teil` = "lm", 
                                  `mittleres Einkommen, oberer Teil` = "um",
                                  `hohes Einkommen` = "h")
      ),
      # drop down input selection Region
      selectInput("var_region", 
                  "Region wählen", 
                  choices = list(`Alle` = "Alle",
                                 `Südasien, Südostasien, Pazifik` = "Südasien, Südostasien, Pazifik", 
                                 `Europa, GUS` = "Europa, GUS", 
                                 `Lateinamerika, Karibik` = "Lateinamerika, Karibik", 
                                 `Nordafrika, Naher Osten` = "Nordafrika, Naher Osten", 
                                 `Subsahara-Afrika` = "Subsahara-Afrika")
      ), 
      
      # drop down input selection Risikofaktoren
      selectInput("var_risiko", 
                  "Risikofaktoren wählen", 
                  choices = list(`Alle` = "Alle",
                                 `Extraktivismus` = "Asia", 
                                 `politische und soziale Fragilität` = "Europe", 
                                 `Schuldenstruktur` = "Latin America", 
                                 `Naturkathastrophen / Klimawandel` = "Middle East" 
                  )
      ),
      
      # Drop down input selection Trend anzeigen
      checkboxInput("var_trend", 
                    "Verschuldungstrend anzeigen", 
                    value = FALSE
      ),
      
      # Drop down input selection Zahlungseinstellungen anzeigen
      checkboxInput("var_zahlung", 
                    "Anhaltende und zwischenzeitliche Zahlungseinstellungen anzeigen", 
                    value = FALSE
      )
    )
  )
)

##--------------------------------------------------##
## Farbcodierung Karte                              ##
##--------------------------------------------------##

s.kritisch <- "#E61700"
kritisch <- "#FF8040"
l.kritisch <- "#FFCC99"
n.kritisch <- "#C5C7B8"
k.Daten <- "#808080"
nT.Analyse <-  "#F0F2E8"
hint.grnd <- " #B3F0D4"

# helperfunction to read corrct trend data:


##--------------------------------------------------##
## Server für Shiny Map                             ##
##--------------------------------------------------##

server <- function(input, output) {
  #output$table <- renderDT(data)
  
  # Select Data according to input
  filteredData <- reactive({
    if (input$var_income == "Alle" & input$var_region == "Alle") {
      data <- map@data 
    } else if (input$var_income != "Alle" & input$var_region == "Alle") {
      data <- subset(map@data, income == input$var_income)
    } else if (input$var_income == "Alle" & input$var_region != "Alle") {
      data <- subset(map@data, region == input$var_region) 
    } else {
      data <- subset(map@data, region == input$var_region & income == input$var_income)
    }
    # add regional input when constructed columns with region input
  })
  #new.data <- data[ which( data$V1 > 2 | data$V2 < 4) , ]
  
  filteredTrend <- reactive({
    if (input$var_debtindikator == "debt_sit_cat2") {
      trend_var <- "trend_new"
    } else if (input$var_debtindikator == "public_debt_bip2") {
      trend_var <- "trend_pdb_new"
    } else if (input$var_debtindikator == "public_debt_state_rev2") {
      trend_var <- "trend_pdsr_new"
    } else if (input$var_debtindikator == "foreign_debt_bip2") {
      trend_var <- "trend_fdp_new"
    } else if (input$var_debtindikator == "foreign_debt_exp2") {
      trend_var <- "trend_fde_new"
    } else  {
      trend_var <- "trend_edse_new"
    }
  })
  
  # create reactive data output
  output$map1 <- renderLeaflet({
    
    # Create Basemap:
    leaflet(map, options = leafletOptions(minZoom = 2, maxZoom = 10)) %>%  # 
      addTiles( # urlTemplate = "//{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png"
      ) %>% addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
      setView( 0, 0, 2 )
  })
  
  # Display colored countries dependent on the output
  observe({
    
    # Choose data based on selected input
    datafiltered <- filteredData()
    
    # match selected rows in data with spacial data
    ordercounties <- match(map@data$ISO3, datafiltered$ISO3)
    map@data <- datafiltered[ordercounties, ]
    str(map@data)
   
    
    # Select indicators for polygons
    map$variableplot <- map@data[, input$var_debtindikator]
    
  
    #Select input for mouseover text
    map$mouseover <- map@data[,gsub('.$', '',input$var_debtindikator)]
    
    # Select input for markers
    trendfilter <- filteredTrend()
    
    
    trend_data <- map@data[, which(names(map@data) %in% c("LON", "LAT", trendfilter))]
    trend_data <- trend_data[complete.cases(trend_data),]
    
    trend_data$trendinput <- trend_data[, which(colnames(trend_data)==trendfilter)]
    
    # data für feuersymbole
    pay_data <- map@data[!is.na(map@data$payment_stop),]
    # Create color palette
    pal <- colorFactor(c(nT.Analyse, n.kritisch, l.kritisch, kritisch, s.kritisch
    ), levels = c(-1, 0, 1, 2, 3), na.color = "#808080" )
    
    # Create text shown when mouse glides over countries
    mytext <- paste0(
      "<b>", map@data$country, "</b>","<br/>",
      "Verschuldungssituation in %: ", "<b>", map$mouseover, "</b>" ) %>%
      lapply(htmltools::HTML)
    # Transform shapefile for poligon input
    map_ll <- spTransform(map, CRS("+init=epsg:4326"))
    # Create Map
    l <- leafletProxy("map1", data = map_ll) %>%
      clearShapes() %>% #clearMarkers() %>%
      addPolygons(
        fillColor = ~pal(variableplot),
        color = "black",
        dashArray = "1",
        weight = 0.1, 
        smoothFactor = 0.5,
        opacity = 0.7,
        fillOpacity = 0.7,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      clearControls() %>%
      # Make legend
      leaflet::addLegend(position = "bottomright",
                         # Specify colors manually b/c pal function does not show colors in legend
                         colors = c(s.kritisch, kritisch,  l.kritisch, n.kritisch, k.Daten, nT.Analyse ), 
                         #values = c("sehr kritisch", "kritisch", "leicht kritisch",
                         #           "nicht kritisch", "keine Daten vorhanden", "Nicht Teil der Betrachtung"),
                         #na.label = "keine Daten vorhanden",
                         opacity = 0.7, title = "Verschuldungssituation",
                         labels = c("sehr kritisch", "kritisch", "leicht kritisch",
                                    "nicht kritisch", "keine Daten vorhanden", "Nicht Teil der Betrachtung") 
      )
    proxy <- leafletProxy(mapId = "map1", data = trend_data) %>%
      clearMarkers()
    
    # marker für Trend
    if (input$var_trend) {
      proxy %>% addMarkers( lat = trend_data$LAT, lng = trend_data$LON,
                            icon = ~PfeilIcons[trend_data$trendinput] ,
                            label = trend_data$NAME
      )
    }
    # Marker hinzufügen für Feuer symbole
    if (input$var_zahlung) {
      proxy %>% addMarkers( lat = pay_data$LAT, lng = pay_data$LON,
                            icon = ~FeuerIcons[pay_data$payment_stop] ,
                            label = pay_data$NAME
      )
    }
  })
  
}

# shinyApp
shinyApp(ui = ui, server = server)
# runApp('./app.R')
# show log file
shiny::reactlogShow()

################# USeful Codewaste:
# c("Land mit geringem Einkommen", "Land mit mittlerem Einkommen im unteren Bereich", "Land mit mittlerem Einkommen im oberen Bereich", 
#   "Land mit hohem Einkommen oder ohne Angaben"),
# clearMarkers() %>%
#   if (input$markers) {
#     addMarkers(lat = map@data$LAT, lng = map@data$LON, icon = ~PfeilIcons[data$trend_new], label = map@data$NAME) #~PfeilIcons[map@data$trend_new]
#   } 