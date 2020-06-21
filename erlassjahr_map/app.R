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
library(reactlog) # Reactivity Visualizer for 'shiny'
library(shiny) # Web Application Framework for R
library(DT) # A Wrapper of the JavaScript Library 'DataTables'
library(rgdal) # Bindings for the 'Geospatial' Data Abstraction Library
library(leaflet) # Create Interactive Web Maps with the JavaScript 'Leaflet' Library
library(maps) # Draw Geographical Maps
library(dplyr) # A Grammar of Data Manipulation
library(english) # Translate Integers into English
library(plyr) # Tools for Splitting, Applying and Combining Data
library(readxl) # Read Excel Files
library(countrycode) # Convert Country Names and Country Codes
library(magrittr) # A Forward-Pipe Operator for R
library(zoo) # S3 Infrastructure for Regular and Irregular Time Series (Z's Ordered Observations)
library(modules) # Self Contained Units of Source Code
library(mapview) # Interactive Viewing of Spatial Data in R
library(webshot) # Take Screenshots of Web Pages
library(rnaturalearth) # World Map Data from Natural Earth
library(rnaturalearthhires) # High Resolution World Vector Map Data from Natural Earth used in rnaturalearth
library(rnaturalearthdata) # World Vector Map Data from Natural Earth Used in 'rnaturalearth'

# Check for Phantom.js:
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs()
}


FindColNumber <- function(df,input){
  as.numeric(which(colnames(df)==input))
}

# Implement modules for cleaning code:
m <- modules::use("graphics.R")
FeuerIcons <- m$FeuerIcons()
PfeilIcons <- m$PfeilIcons()
##--------------------------------------------------##
## Load Data                                        ##
##--------------------------------------------------##

# Map Data / Shape File
shape_path <- "TM_WORLD_BORDERS_SIMPL-0.3.shp"
encoding <- "UTF-8"

LonLat <- readOGR(dsn=path.expand(shape_path), 
                  layer="TM_WORLD_BORDERS_SIMPL-0.3", 
                  encoding = encoding)
# Select French-Guiana to merge to shapefile (Missing in ne data)
Fr.Guiana <- LonLat[LonLat$ISO3 == "GUF",]
Fr.Guiana@data <- Fr.Guiana@data[c("ISO3", "NAME")]
Fr.Guiana@data <- rename(Fr.Guiana@data, iso_a3 = ISO3, geounit = NAME)
# Rename the Polygon ID to 255 to have unique ones
slot(slot(Fr.Guiana, "polygons")[[1]], "ID") = "255"

# Load Data
# load("sr20_erlassjahr.RData")
# data <- sr20_erlassjahr
data <- get(load(paste0("data/final_data_", year, ".RData"),e<- new.env()),e) 
rm(list=ls(envir=e), envir=e) 

data[data==""]<-NA

data$url <- paste0("<a href='",data$link,"'>",data$ISO3,"</a>")

# rename colum to match sp file
data <- rename(data, iso_a3 = ISO3)

# Use naturalearth shapefile
sp_data2 <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sp")
# take out unnecesary data
sp_data2@data <- sp_data2@data[c("featurecla", "geounit", "iso_a3")]



# merge the missing polygon to the sp object
sp_data2 <- raster::bind(sp_data2,Fr.Guiana)

# add Missing ISO3 Codes
sp_data2@data[sp_data2@data$geounit == "France", "iso_a3"] <- "FRA"
sp_data2@data[sp_data2@data$geounit == "Norway", "iso_a3"] <- "NOR"
sp_data2@data[sp_data2@data$geounit == "Kosovo", "iso_a3"] <- "RKS"

# combine data
map <- sp::merge(sp_data2, data, by.x="iso_a3", duplicateGeoms=TRUE)
map@data <- rename(map@data, ISO3 = iso_a3)
MergeColumns <- LonLat@data[, c("ISO3", "LON", "LAT")]
map <- sp::merge(map, MergeColumns, by.x="ISO3", duplicateGeoms=TRUE)


##--------------------------------------------------##
## User Interface Shiny                             ##
##--------------------------------------------------##

# CSS code
# mycss <- "
# #select ~ .selectize-control .selectize-input {
#   max-height: 100px;
#   overflow-y: auto;
# }
# "

ui <- fluidPage(
  #titlePanel(title = h1("Erlassjahr app", style = "color:#3474A7", align = "left")),
  #tags$style(mycss),
  #layout with main area and side bar
  sidebarLayout(
    mainPanel(
      div(class="outer",
          tags$style(type = "text/css", 
                     ".outer {position: fixed; top: 0; 
                     left: 0; right: 0; bottom: 0; padding: 0}", 
                     ".selectize-dropdown-content {max-height: 100px; }"
                     ), # overflow:   hidden;
          
          
          
          leafletOutput(outputId = "map1", height = "100%", width = "100%"))
      
    ),
    
    # Create movable fixed (absolute) side panel
    absolutePanel(
      top = 30, left = 45, width = 200, fixed=TRUE,
      draggable = TRUE, height = "auto",
      
      # Logo Einbettung
      #tags$a(imageOutput("erlassjahr_logo_300col_2015.jpg"),href="https://www.google.com",width="125px", height = "40px"),
      #div(img(src = "erlassjahr_logo_300col_2015.svg",width="125px", height = "40px"), style="text-align: center;", href="https://www.google.com"),
      #img(src = "erlassjahr-Logo-transparent.eps",width="100%"),
      #HTML(
      # #  paste(
      #     '<br/>',
      #     '<br/>',
      #     '<br/>'
      #   )
      # ),
      # dropdown input selection Debt Indicator
      selectizeInput(
        inputId = "var_debtindikator",
        label = "Schuldenindikator wählen",
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
                                  `niedriges mittleres Einkommen` = "lm", 
                                  `höheres mittleres Einkommen` = "um",
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
label.color <- '#FFFFFF'

# Choice vector for Mouseover
choiceVec <-  c("Aggregierte Indikatoren" = "debt_sit_cat2",
                "Öffentliche Schulden / BIP" = "public_debt_bip2", 
                "Öffentliche Schulden / Staatseinnahmen" = "public_debt_state_rev2", 
                "Auslandsschuldenstand / BIP" = "foreign_debt_bip2", 
                "Auslandsschuldenstand / Exporteinnahmen" = "foreign_debt_exp2", 
                "Auslandsschuldendienst / Exporteinnahmen" = "external_debt_service_exp2")

content <- paste0(sep = "<b>", "Quelle erlassjahr.de", "</b>"
)


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
  
  Legend.Labels <- reactive({
    if (input$var_debtindikator == "debt_sit_cat2") {
      legende <- c("sehr kritisch", "kritisch", "leicht kritisch",
                   "nicht kritisch", "keine Daten vorhanden", "nicht Teil der Betrachtung", "Quelle: erlassjahr.de")
    } else if (input$var_debtindikator == "public_debt_bip2") {
      legende <- c("> 100 %", "75% - 100%", "50% - 75%",
                   "< 50%", "keine Daten vorhanden", "nicht Teil der Betrachtung", "Quelle: erlassjahr.de")
    } else if (input$var_debtindikator == "public_debt_state_rev2") {
      legende <- c("> 400 %", "300% - 400%", "200% - 300%",
                   "< 200%", "keine Daten vorhanden", "nicht Teil der Betrachtung", "Quelle: erlassjahr.de")
    } else if (input$var_debtindikator == "foreign_debt_bip2") {
      legende <- c("> 80 %", "60% - 80%", "40% - 60%",
                   "< 40%", "keine Daten vorhanden", "nicht Teil der Betrachtung", "Quelle: erlassjahr.de")
    } else if (input$var_debtindikator == "foreign_debt_exp2") {
      legende <- c("> 300 %", "225% - 300%", "150% - 225%",
                   "< 150%", "keine Daten vorhanden", "nicht Teil der Betrachtung", "Quelle: erlassjahr.de")
    } else  {
      legende <- c("> 30 %", "22,5% - 30%", "15% - 22,5%",
                   "< 15%", "keine Daten vorhanden", "nicht Teil der Betrachtung", "Quelle: erlassjahr.de")
      }
  })
  
Mouse.Symb <- reactive({
    if (input$var_debtindikator == "debt_sit_cat2") {
      var <- ""
    } else {
     var <- "%"
    }
 }) 
  
  # Create Basemap:
  foundation_map <- reactive({
    
    leaflet(map, options = leafletOptions(minZoom = 2, maxZoom = 10)) %>%  # 
      addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png'
      ) %>% addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
      setView( 0, 15, 2)
    
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
    state_popup <- paste0("<a href=\'", map@data$link,"', target=\"_blank\">Zum Länderprofil </a>")
    
    
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
    
    Llabels <- Legend.Labels()
    # Create color palette for riskfactor
    # palit <- definePalet()
    
    # Create text shown when mouse glides over countries
    mytext <- paste0(
      "<b>", map@data$country, "</b>","<br/>",
      names(choiceVec)[choiceVec == input$var_debtindikator], ": ", 
      "<b>", map$mouseover, Mouse.Symb(), "</b>", "<br/>",
      "Mit Mausklick zum Länderprofil" ) %>%
      lapply(htmltools::HTML)
    # Transform shapefile for poligon input
    map_ll <- spTransform(map, CRS("+init=epsg:4326"))
    
    map_conf$map <- map_ll
    map_conf$pal <- pal 
    map_conf$trend_data <- trend_data
    map_conf$pay_data <- pay_data
    map_conf$Llabels <- Llabels
    
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
                         colors = c(s.kritisch, kritisch,  l.kritisch, n.kritisch, k.Daten, nT.Analyse), #, risk.fact )
                         opacity = 1, title = "Verschuldungssituation",
                         labels = Llabels
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
          colors   = c(s.kritisch, kritisch,  l.kritisch, n.kritisch, k.Daten, nT.Analyse, label.color), #, risk.fact ),
          opacity  = 1, title = "Verschuldungssituation",
          labels   = map_conf$Llabels
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
                       , "_erlassjahr_custom_map"
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

