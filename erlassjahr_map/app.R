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
library(reactlog)
library(shiny)
library(DT)
library(rgdal)
library(leaflet)
library(maps)
library(dplyr)
library(english)
library(plyr)
library(tidyverse)
library(tidylog)
library(readxl)
library(countrycode)
library(magrittr)
library(zoo)
library(modules)
library(mapview)
library(webshot)


FindColNumber <- function(df,input){
  as.numeric(which(colnames(df)==input))
}

# IMplement modules for cleaning code:
m <- modules::use("graphics.R")
FeuerIcons <- m$FeuerIcons()
PfeilIcons <- m$PfeilIcons()
##--------------------------------------------------##
## Load Data                                        ##
##--------------------------------------------------##

# Map Data / Shape File
shape_path <- "TM_WORLD_BORDERS_SIMPL-0.3.shp"
encoding <- "UTF-8"

map <- readOGR(dsn=path.expand(shape_path), 
               layer="TM_WORLD_BORDERS_SIMPL-0.3", 
               encoding = encoding)

# Load Data
load("sr20_erlassjahr.RData")
data <- sr20_erlassjahr
data[data==""]<-NA

data$url <- paste0("<a href='",data$link,"'>",data$ISO3,"</a>")
# Combine data:
map <- sp::merge(map, data, by.x="ISO3", duplicateGeoms=TRUE)



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
      top = 7, left = 10, width = 250, fixed=TRUE,
      draggable = TRUE, height = "auto",
      
      # Logo Einbettung
      #tags$a(imageOutput("erlassjahr_logo_300col_2015.jpg"),href="https://www.google.com",width="125px", height = "40px"),
      div(img(src = "erlassjahr_logo_300col_2015.svg",width="125px", height = "40px"), style="text-align: center;", href="https://www.google.com"),
      #img(src = "erlassjahr-Logo-transparent.eps",width="100%"),
      HTML(
        paste(
          '<br/>',
          '<br/>',
          '<br/>'
        )
      ),
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
                  choices = list(`Keine Auswahl` = "None",
                                 `Extraktivismus` = "extractivism" , 
                                 `politische und soziale Fragilität` = "fragility", 
                                 `Schuldenstruktur` = "debt_prob", 
                                 `Naturkatastrophen / Klimawandel` = "vulnerability",
                                 selected = NULL
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
      ),
      downloadButton( outputId = "dl")
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
risk.fact <- "#3D36C7"
# helperfunction to read corrct trend data:

##--------------------------------------------------##
## Server für Shiny Map                             ##
##--------------------------------------------------##

server <- function(input, output, session) {
  #output$table <- renderDT(data)
  
  # Select Data according to input
  filteredData <- reactive({
    
    col.risiko <- FindColNumber(map@data, input$var_risiko)
    
    if (input$var_income == "Alle" & input$var_region == "Alle" & input$var_risiko == "None") {
      data <- map@data 
    } else if (input$var_income != "Alle" & input$var_region == "Alle" & input$var_risiko == "None") {
      data <- subset(map@data, income == input$var_income)
    } else if (input$var_income == "Alle" & input$var_region != "Alle" & input$var_risiko == "None") {
      data <- subset(map@data, region == input$var_region) 
    } else if (input$var_income != "Alle" & input$var_region != "Alle" & input$var_risiko == "None") {
      data <- subset(map@data, region == input$var_region & income == input$var_income)
    } else if (input$var_income == "Alle" & input$var_region == "Alle" & input$var_risiko != "None") {
      data <- subset(map@data, map@data[,col.risiko] == 1)
    } else if (input$var_income != "Alle" & input$var_region == "Alle" & input$var_risiko != "None") {
      data <- subset(map@data, income == input$var_income & map@data[,col.risiko] == 1)
    } else if (input$var_income == "Alle" & input$var_region != "Alle" & input$var_risiko != "None") {
      data <- subset(map@data, region == input$var_region & map@data[,col.risiko] == 1) 
    } else {
      data <- subset(map@data, region == input$var_region & income == input$var_income & map@data[,col.risiko] == 1)
    }
    # add regional input when constructed columns with region input
  })
  
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
  
  # Create Basemap:
  foundation_map <- reactive({
    
    leaflet(map, options = leafletOptions(minZoom = 2, maxZoom = 10)) %>%  # 
      addTiles( urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png'
      ) %>% addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
      setView( 0, 0, 2)
    
  })
  
  # create reactive data output
  output$map1 <- renderLeaflet({
    
    foundation_map()
    
  })
  
  
  map_conf <- reactiveValues()
  
  # Display colored countries dependent on the output
  observe({
    
    # Choose data based on selected input
    datafiltered <- filteredData()
    
    # match selected rows and columns in data with spacial data
    ordercounties <- match(map@data$ISO3, datafiltered$ISO3)
    map@data <- datafiltered[ordercounties, ]
    
    # Select Polygons for Debtindikator
    map$variableplot <- map@data[, input$var_debtindikator]
    
    #map url
    state_popup <- paste0("<a href='", map@data$link,"'>Zum Länderprofil </a>")
    
    
    # Select Polygons for Riskfactor
    #map$riskplot <- map@data[, input$var_risiko]
    
    #Select input for mouseover text
    map$mouseover <- map@data[,gsub('.$', '',input$var_debtindikator)]
    
    # Select input for markers
    trendfilter <- filteredTrend()
    trend_data <- map@data[, which(names(map@data) %in% c("LON", "LAT", trendfilter))]
    trend_data <- trend_data[complete.cases(trend_data),]
    
    trend_data$trendinput <- trend_data[, which(colnames(trend_data)==trendfilter)]
    
    # data für feuersymbole
    pay_data <- map@data[!is.na(map@data$payment_stop),]
    
    # data für links
    link_data <- map@data[!is.na(map@data$link),]
    
    
    # Create color palette for Debtindicator
    pal <- colorFactor(c(nT.Analyse, n.kritisch, l.kritisch, kritisch, s.kritisch
    ), levels = c(-1, 0, 1, 2, 3), na.color = "#808080" )
    
    # Create color palette for riskfactor
    # palit <- definePalet()
    
    # Create text shown when mouse glides over countries
    mytext <- paste0(
      "<b>", map@data$country, "</b>","<br/>",
      "Verschuldungssituation: ", "<b>", map$mouseover, "</b>", "<br/>",
      "Mit Mausklick zum Länderprofil" ) %>%
      lapply(htmltools::HTML)
    # Transform shapefile for poligon input
    map_ll <- spTransform(map, CRS("+init=epsg:4326"))
    
    map_conf$map <- map_ll
    map_conf$pal <- pal 
    map_conf$trend_data <- trend_data
    map_conf$pay_data <- pay_data
    
    # Create Map
    l <- leafletProxy("map1", data = map_ll) %>%
      clearShapes() %>% 
      addPolygons(
        fillColor = ~pal(variableplot),
        color = "black",
        dashArray = "1",
        weight = 0.1, 
        smoothFactor = 0.5,
        opacity = 1,
        fillOpacity = 1,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        popup = state_popup
      ) %>%
      
      clearControls() %>%
      # Make legend
      leaflet::addLegend(position = "bottomright",
                         # Specify colors manually b/c pal function does not show colors in legend
                         colors = c(s.kritisch, kritisch,  l.kritisch, n.kritisch, k.Daten, nT.Analyse), #, risk.fact ), 
                         #values = c("sehr kritisch", "kritisch", "leicht kritisch",
                         #           "nicht kritisch", "keine Daten vorhanden", "Nicht Teil der Betrachtung"),
                         #na.label = "keine Daten vorhanden",
                         opacity = 1, title = "Verschuldungssituation",
                         labels = c("sehr kritisch", "kritisch", "leicht kritisch",
                                    "nicht kritisch", "keine Daten vorhanden", "Nicht Teil der Betrachtung") #, "Risikofaktor trifft zu") 
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
  
  user_created_map <- reactive({
    
    # Appearently, we need to basically recreate the whole thing, as there 
    # is no obvious way to retrieve the leafletProxy changes and re apply them
    # to the leaflet widget..
    
    # However, a workaround would consist in lot of redundancy, 
    # and probably long waiting time. But it works :P 

    output_map <- 
      leaflet(map_conf$map) %>%
        addTiles( urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png') %>% 
        addProviderTiles(provider = "CartoDB.PositronNoLabels") %>% 
        addPolygons(
          fillColor    = ~map_conf$pal(variableplot),
          color        = "black",
          dashArray    = "1",
          weight       = 0.1, 
          smoothFactor = 0.5,
          opacity      = 1,
          fillOpacity  = 1
        ) %>%
        clearControls() %>%
        leaflet::addLegend(
          position = "bottomright",
          colors   = c(s.kritisch, kritisch,  l.kritisch, n.kritisch, k.Daten, nT.Analyse), #, risk.fact ), 
          opacity  = 1, title = "Verschuldungssituation",
          labels   = c("sehr kritisch", "kritisch", "leicht kritisch",
                      "nicht kritisch", "keine Daten vorhanden", "Nicht Teil der Betrachtung") #, "Risikofaktor trifft zu") 
        )  %>%
        setView(
          lng  = input$map1_center$lng,
          lat  = input$map1_center$lat, 
          zoom = input$map1_zoom
        ) 
    
    if (input$var_trend) {
      output_map <- output_map %>% 
        addMarkers(
          lat  = map_conf$trend_data$LAT, 
          lng  = map_conf$trend_data$LON,
          icon = ~PfeilIcons[map_conf$trend_data$trendinput]
      )
    }
    
  
    if (input$var_zahlung) {
      output_map <- output_map %>% 
        addMarkers(
          lat  = map_conf$pay_data$LAT,
          lng  = map_conf$pay_data$LON,
          icon = ~FeuerIcons[map_conf$pay_data$payment_stop]
      )
    }
    
    return(output_map)
    
  })
  
  output$dl <- downloadHandler(
    filename = paste0( Sys.Date()
                       , "_customLeafletmap"
                       , ".pdf"
    )
    
    , content = function(file) {
      mapshot( x = user_created_map()
               , file = file
               , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
               , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      )
    } # end of content() function
  ) # end of downloadHandler() function
}
# shinyApp
shinyApp(ui = ui, server = server)
#runApp('./app.R')
