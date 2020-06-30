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
library(shiny) # Web Application Framework for R
library(rgdal) # Bindings for the 'Geospatial' Data Abstraction Library
library(leaflet) # Create Interactive Web Maps with the JavaScript 'Leaflet' Library
library(dplyr) # A Grammar of Data Manipulation
library(modules) # Self Contained Units of Source Code
library(webshot) # Take Screenshots of Web Pages
library(rnaturalearth) # World Map Data from Natural Earth

# Check for Phantom.js:
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs()
}


findColNumber <- function(df, input) {
  as.numeric(which(colnames(df) == input))
}

# Implement modules 
m <- modules::use("scripts/graphics.R")
feuerIcons <- m$FeuerIcons()
pfeilIcons <- m$PfeilIcons()
##--------------------------------------------------##
## Load Data                                        ##
##--------------------------------------------------##

# load the year variable created from data_preparation.R to build the correct
# map for a given year
load("data/year_data.Rdata")

# Map Data / Shape File
shape_path <- "map_files/TM_WORLD_BORDERS_SIMPL-0.3.shp"
encoding <- "UTF-8"

lon_lat <- readOGR(dsn = path.expand(shape_path),
                  layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                  encoding = encoding)
# Select French-Guiana to merge to shapefile (Missing in ne data)
fr_guiana <- lon_lat[lon_lat$ISO3 == "GUF", ]
fr_guiana@data <- fr_guiana@data[c("ISO3", "NAME")]
fr_guiana@data <-
  rename(fr_guiana@data, iso_a3 = ISO3, geounit = NAME)
# Rename the Polygon ID to 255 to have unique ones
slot(slot(fr_guiana, "polygons")[[1]], "ID") = "255"

# Load Data
data <-
  get(load(paste0("data/final_data_", year, ".RData"), e <-
             new.env()), e)
rm(list = ls(envir = e), envir = e)

data[data == ""] <- NA

data$url <- paste0("<a href='", data$link, "'>", data$ISO3, "</a>")

# rename colum to match sp file
data <- rename(data, iso_a3 = ISO3)

# Use naturalearth shapefile
sp_data2 <-
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sp")
# take out unnecesary data
sp_data2@data <- sp_data2@data[c("featurecla", "geounit", "iso_a3")]

# merge the missing polygon to the sp object
sp_data2 <- raster::bind(sp_data2, fr_guiana)

# add Missing ISO3 Codes
sp_data2@data[sp_data2@data$geounit == "France", "iso_a3"] <- "FRA"
sp_data2@data[sp_data2@data$geounit == "Norway", "iso_a3"] <- "NOR"
sp_data2@data[sp_data2@data$geounit == "Kosovo", "iso_a3"] <- "RKS"

# combine data
map <- sp::merge(sp_data2, data, by.x = "iso_a3", duplicateGeoms = TRUE)
map@data <- rename(map@data, ISO3 = iso_a3)
MergeColumns <- lon_lat@data[, c("ISO3", "LON", "LAT")]
map <-
  sp::merge(map, MergeColumns, by.x = "ISO3", duplicateGeoms = TRUE)

##--------------------------------------------------##
## User Interface Shiny                             ##
##--------------------------------------------------##

ui <- fluidPage(
  #layout with main area and side bar
  sidebarLayout(
    mainPanel(div(
      class = "outer",
      tags$style(
        type = "text/css",
        ".outer {position: fixed; top: 0;
                     left: 0; right: 0; bottom: 0; padding: 0}",
        ".selectize-dropdown-content {max-height: 400px; }"
      ),
      
      leaflet::leafletOutput(
        outputId = "map1",
        height = "100%",
        width = "100%"
      )
    )),
    
    # Create movable fixed (absolute) side panel
    absolutePanel(
      top = 10,
      left = 50,
      width = 200,
      fixed = TRUE,
      draggable = TRUE,
      height = "auto",
      # dropdown input selection Debt Indicator
      selectizeInput(
        inputId = "var_debtindikator",
        label = "Schuldenindikator wählen",
        choices =  list(
          `Aggregierte Indikatoren` = "debt_sit_cat2",
          `Öffentliche Schulden / BIP` = "public_debt_bip2",
          `Öffentliche Schulden / Staatseinnahmen` = "public_debt_state_rev2",
          `Auslandsschuldenstand / BIP` = "foreign_debt_bip2",
          `Auslandsschuldenstand / Exporteinnahmen` = "foreign_debt_exp2",
          `Auslandsschuldendienst / Exporteinnahmen` = "external_debt_service_exp2"
        )
      ),
      
      # drop down input selection income category
      selectInput(
        "var_income",
        "Einkommenskategorie wählen",
        choices =  list(
          `Alle` = "Alle",
          `geringes Einkommen` = "l",
          `niedriges mittleres Einkommen` = "lm",
          `höheres mittleres Einkommen` = "um",
          `hohes Einkommen` = "h"
        )
      ),
      # drop down input selection Region
      selectInput(
        "var_region",
        "Region wählen",
        choices = list(
          `Alle` = "Alle",
          `Südasien, Südostasien, Pazifik` = "Südasien, Südostasien, Pazifik",
          `Europa, GUS` = "Europa, GUS",
          `Lateinamerika, Karibik` = "Lateinamerika, Karibik",
          `Nordafrika, Naher Osten` = "Nordafrika, Naher Osten",
          `Subsahara-Afrika` = "Subsahara-Afrika"
        )
      ),
      
      # drop down input selection Risikofaktoren
      selectInput(
        "var_risiko",
        "Risikofaktoren wählen",
        choices = list(
          `Keine Auswahl` = "None",
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
                    value = FALSE),
      
      # Drop down input selection Zahlungseinstellungen anzeigen
      checkboxInput(
        "var_zahlung",
        "Anhaltende und zwischenzeitliche Zahlungseinstellungen anzeigen",
        value = FALSE
      ),
      downloadButton(outputId = "dl"),
      actionButton(
        inputId = 'Method',
        label = HTML("Methodik: Schuldenreport&nbsp;&nbsp;&nbsp; <br/> von erlassjahr.de und Misereor"),
        icon = icon("th"),
        onclick = "window.open('https://erlassjahr.de/produkt-kategorie/schuldenreporte/')"
      )
    )
  ))

##--------------------------------------------------##
## Farbcodierung Karte                              ##
##--------------------------------------------------##

s_kritisch <- "#E61700"
kritisch <- "#FF8040"
l_kritisch <- "#FFCC99"
n_kritisch <- "#C5C7B8"
k_daten <- "#808080"
nT_analyse <-  "#F0F2E8"
risk_fact <- "#3D36C7"
label_color <- '#f6f8f8'

# Choice vector for Mouseover
choiceVec <-  c(
  "Aggregierte Indikatoren" = "debt_sit_cat2",
  "Öffentliche Schulden / BIP" = "public_debt_bip2",
  "Öffentliche Schulden / Staatseinnahmen" = "public_debt_state_rev2",
  "Auslandsschuldenstand / BIP" = "foreign_debt_bip2",
  "Auslandsschuldenstand / Exporteinnahmen" = "foreign_debt_exp2",
  "Auslandsschuldendienst / Exporteinnahmen" = "external_debt_service_exp2"
)

content <- paste0(sep = "<b>", "Quelle erlassjahr.de", "</b>")


##--------------------------------------------------##
## Server für Shiny Map                             ##
##--------------------------------------------------##

server <- function(input, output, session) {
  
  # Select Data according to input
  filteredData <- reactive({
    col_risiko <- findColNumber(map@data, input$var_risiko)
    
    if (input$var_income == "Alle" &
        input$var_region == "Alle" & input$var_risiko == "None") {
      data <- map@data
    } else if (input$var_income != "Alle" &
               input$var_region == "Alle" & input$var_risiko == "None") {
      data <- subset(map@data, income == input$var_income)
    } else if (input$var_income == "Alle" &
               input$var_region != "Alle" & input$var_risiko == "None") {
      data <- subset(map@data, region == input$var_region)
    } else if (input$var_income != "Alle" &
               input$var_region != "Alle" & input$var_risiko == "None") {
      data <-
        subset(map@data,
               region == input$var_region & income == input$var_income)
    } else if (input$var_income == "Alle" &
               input$var_region == "Alle" & input$var_risiko != "None") {
      data <- subset(map@data, map@data[, col_risiko] == 1)
    } else if (input$var_income != "Alle" &
               input$var_region == "Alle" & input$var_risiko != "None") {
      data <-
        subset(map@data, income == input$var_income &
                 map@data[, col_risiko] == 1)
    } else if (input$var_income == "Alle" &
               input$var_region != "Alle" & input$var_risiko != "None") {
      data <-
        subset(map@data, region == input$var_region &
                 map@data[, col_risiko] == 1)
    } else {
      data <-
        subset(map@data,
               region == input$var_region &
                 income == input$var_income & map@data[, col_risiko] == 1)
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
  
  legendLabels <- reactive({
    if (input$var_debtindikator == "debt_sit_cat2") {
      legende <- c(
        "sehr kritisch",
        "kritisch",
        "leicht kritisch",
        "nicht kritisch",
        "keine Daten vorhanden",
        "nicht Teil der Betrachtung",
        " ",
        "Quelle: erlassjahr.de"
      )
    } else if (input$var_debtindikator == "public_debt_bip2") {
      legende <- c(
        "> 100 %",
        "75% - 100%",
        "50% - 75%",
        "< 50%",
        "keine Daten vorhanden",
        "nicht Teil der Betrachtung",
        " ",
        "Quelle: erlassjahr.de"
      )
    } else if (input$var_debtindikator == "public_debt_state_rev2") {
      legende <- c(
        "> 400 %",
        "300% - 400%",
        "200% - 300%",
        "< 200%",
        "keine Daten vorhanden",
        "nicht Teil der Betrachtung",
        " ",
        "Quelle: erlassjahr.de"
      )
    } else if (input$var_debtindikator == "foreign_debt_bip2") {
      legende <- c(
        "> 80 %",
        "60% - 80%",
        "40% - 60%",
        "< 40%",
        "keine Daten vorhanden",
        "nicht Teil der Betrachtung",
        " ",
        "Quelle: erlassjahr.de"
      )
    } else if (input$var_debtindikator == "foreign_debt_exp2") {
      legende <- c(
        "> 300 %",
        "225% - 300%",
        "150% - 225%",
        "< 150%",
        "keine Daten vorhanden",
        "nicht Teil der Betrachtung",
        " ",
        "Quelle: erlassjahr.de"
      )
    } else  {
      legende <- c(
        "> 30 %",
        "22,5% - 30%",
        "15% - 22,5%",
        "< 15%",
        "keine Daten vorhanden",
        "nicht Teil der Betrachtung",
        " ",
        "Quelle: erlassjahr.de"
      )
    }
  })
  
  mouseSymb <- reactive({
    if (input$var_debtindikator == "debt_sit_cat2") {
      var <- ""
    } else {
      var <- "%"
    }
  })
  
  # Create Basemap:
  foundation_map <- reactive({
    leaflet::leaflet(map, options = leaflet::leafletOptions(minZoom = 2, maxZoom = 10)) %>%  #
      addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png') %>% addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
      setView(0, 15, 2)
    
  })
  
  # create reactive data output
  output$map1 <- leaflet::renderLeaflet({
    foundation_map()
    
  })
  
  
  map_conf <- reactiveValues()
  
  # Display colored countries dependent on the output
  observe({
    # Choose data based on selected input
    datafiltered <- filteredData()
    
    # match selected rows and columns in data with spacial data
    ordercounties <- match(map@data$ISO3, datafiltered$ISO3)
    map@data <- datafiltered[ordercounties,]
    
    # Select Polygons for Debtindikator
    map$variableplot <- map@data[, input$var_debtindikator]
    
    #map url
    state_popup <-
      paste0("<a href=\'",
             map@data$link,
             "', target=\"_blank\">Zum Länderprofil </a>")
    
    #Select input for mouseover text
    map$mouseover <-
      map@data[, gsub('.$', '', input$var_debtindikator)]
    
    # Select input for markers
    trendfilter <- filteredTrend()
    trend_data <-
      map@data[, which(names(map@data) %in% c("LON", "LAT", trendfilter))]
    trend_data <- trend_data[complete.cases(trend_data), ]
    
    trend_data$trendinput <-
      trend_data[, which(colnames(trend_data) == trendfilter)]
    
    # data für feuersymbole
    pay_data <- map@data[!is.na(map@data$payment_stop), ]
    
    # data für links
    link_data <- map@data[!is.na(map@data$link), ]
    
    
    # Create color palette for Debtindicator
    pal <-
      colorFactor(
        c(nT_analyse, n_kritisch, l_kritisch, kritisch, s_kritisch),
        levels = c(-1, 0, 1, 2, 3),
        na.color = "#808080"
      )
    # Create list of labels for legend
    Llabels <- legendLabels()
    
    # Create text shown when mouse glides over countries
    mytext <- paste0(
      "<b>",
      map@data$country,
      "</b>",
      "<br/>",
      names(choiceVec)[choiceVec == input$var_debtindikator],
      ": ",
      "<b>",
      map$mouseover,
      mouseSymb(),
      "</b>",
      "<br/>",
      "Mit Mausklick zum Länderprofil"
    ) %>%
      lapply(htmltools::HTML)
    # Transform shapefile for poligon input
    map_ll <- spTransform(map, CRS("+init=epsg:4326"))
    
    map_conf$map <- map_ll
    map_conf$pal <- pal
    map_conf$trend_data <- trend_data
    map_conf$pay_data <- pay_data
    map_conf$Llabels <- Llabels
    
    # Create Map
    l <- leaflet::leafletProxy("map1", data = map_ll) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ pal(variableplot),
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
      leaflet::addLegend(
        position = "bottomright",
        # Specify colors manually b/c pal function does not show colors in legend
        colors   = c(
          s_kritisch,
          kritisch,
          l_kritisch,
          n_kritisch,
          k_daten,
          nT_analyse,
          label_color,
          label_color
        ),
        #, risk_fact ),
        opacity = 1,
        title = "Verschuldungssituation",
        labels = Llabels
      )
    
    proxy <- leaflet::leafletProxy(mapId = "map1", data = trend_data) %>%
      clearMarkers()
    
    # marker für Trend
    if (input$var_trend) {
      proxy %>% addMarkers(
        lat = trend_data$LAT,
        lng = trend_data$LON,
        icon = ~ pfeilIcons[trend_data$trendinput] ,
        label = trend_data$NAME
      )
    }
    # Marker hinzufügen für Feuer symbole
    if (input$var_zahlung) {
      proxy %>% addMarkers(
        lat = pay_data$LAT,
        lng = pay_data$LON,
        icon = ~ feuerIcons[pay_data$payment_stop] ,
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
      leaflet::leaflet(map_conf$map) %>%
      addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png') %>%
      addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
      addPolygons(
        fillColor    = ~ map_conf$pal(variableplot),
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
        colors   = c(
          s_kritisch,
          kritisch,
          l_kritisch,
          n_kritisch,
          k_daten,
          nT_analyse,
          label_color,
          label_color
        ),
        #, risk_fact ),
        opacity  = 1,
        title = "Verschuldungssituation",
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
          icon = ~ pfeilIcons[map_conf$trend_data$trendinput]
        )
    }
    
    
    if (input$var_zahlung) {
      output_map <- output_map %>%
        addMarkers(
          lat  = map_conf$pay_data$LAT,
          lng  = map_conf$pay_data$LON,
          icon = ~ feuerIcons[map_conf$pay_data$payment_stop]
        )
    }
    
    return(output_map)
    
  })
  
  output$dl <- shiny::downloadHandler(
    filename = paste0(Sys.Date()
                      , "_erlassjahr_custom_map"
                      , ".pdf")
    
    ,
    content = function(file) {
      webshot::mapshot(
        x = user_created_map()
        ,
        file = file
        ,
        cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
        ,
        selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      )
    } # end of content() function
  ) # end of downloadHandler() function
}

# shinyApp
shinyApp(ui = ui, server = server)
