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

library(shiny)
library(DT)
library(rgdal)
library(leaflet)

##--------------------------------------------------##
## Load Data                                        ##
##--------------------------------------------------##

# Map Data / Shape File
shape_path <- "ne_50m_admin_0_countries.shp"
encoding <- "UTF-8"

map <- readOGR(dsn=path.expand(shape_path), layer="ne_50m_admin_0_countries", encoding = encoding)

# Convert Data from data_cleaning.R into csv File
# write.csv(sr19_erlassjahr, "./sr19_erlassjahr.csv")
# data <- read.csv("./sr19_erlassjahr.csv")
load("sr19_erlassjahr.RData")
data <- plyr::rename(sr19_erlassjahr, c("country_A3" = "ISO_A3"))
data <- as.data.frame(data) # data is loaded as tibble but needs to be converted to data.frame for merging
data$debt_sit_cat <- as.numeric(data$debt_sit_cat)
data$debt_sit_cat <- data$debt_sit_cat -1

# Combine data:
datamap <- sp::merge(map, data, by.x="ISO_A3", duplicateGeoms=TRUE)


##--------------------------------------------------##
## User Interface Shiny                             ##
##--------------------------------------------------##

ui <- fluidPage(
  #titlePanel(title = h1("Erlassjahr app", style = "color:#3474A7", align = "left")),
  
  #layout with main area and side bar
   sidebarLayout(
                  mainPanel(
                    div(class="outer",
                        tags$style(type = "text/css", 
                                   ".outer {position: fixed; top: 0; 
                                   left: 0; right: 0; bottom: 0; padding: 0}"), # overflow:   hidden;
                        
                        
                        
                        leafletOutput(outputId = "map", height = "100%", width = "100%"))
                    
                    #DTOutput(outputId = "table")
                    
                    ),
                  
    # Create movable fixed (absolute) side panel
         absolutePanel(
                      top = 100, left = 10, width = 250, fixed=TRUE,
                       draggable = TRUE, height = "auto",
     
      # dropdown input selection Debt Indicator
      selectInput(
        inputId = "var_debtindikator",
        label = "Schuldenindikator Wählen",
        choices =  list(`Aggregierte Indikatoren` = "debt_sit_cat",
                        `Öffentliche Schulden / BIP` = "public_debt_bip2", 
                        `Öffentliche Schulden / Staatseinnahmen` = "public_debt_state_rev2", 
                        `Auslandsschuldenstand / BIP` = "foreign_debt_bip2", 
                        `Auslandsschuldenstand / Exporteinnahmen` = "foreign_debt_exp2", 
                        `Auslandsschuldendienst / Exporteinnahmen` = "external_debt_service_exp2"
                        )
      ), # ADD AGGREGATED OPTION AS COLUM IN DATAFRAME
      
      # drop down input selection income category
      selectInput(
        inputId = "var_income",
        label = "Einkommenskategorie wählen",
        choices =  list(`Alle` = "Alle",
                        `Untere Einkommenstkategorie` = "L",
                        `Untere Mittlere Kategorie` = "LM", 
                        `Obere Mittlere Kategorie` = "UM")
      ),
        # drop down input selection Region
        selectInput("var_region", 
                    "Region wählen", 
                    choices = list(`Alle` = "Alle",
                                 `Asien` = "Asia", 
                                 `Europa` = "Europe", 
                                 `Latein Amerika` = "Latin America", 
                                 `Mittlerer Osten` = "Middle East", 
                                 `Ozeanien` = "Oceania",
                                 `Afrika` = "Sub-Saharan Africa")
    ), 
    
        # Drop down input selection Trend anzeigen
    checkboxInput("var_trend", 
                    "Verschuldungstrend anzeigen", 
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

##--------------------------------------------------##
## Server für Shiny Map                             ##
##--------------------------------------------------##

server <- function(input, output) {
  #output$table <- renderDT(data)
  
  # Select Data according to input
  filteredData <- reactive({
    if (input$var_income == "Alle") {
     data <- subset(data, income == income) 
    } else {
    data <- subset(data, income == input$var_income)
    }
    if (input$var_region == "Alle") {
      data <- subset(data, region_large == region_large) 
    } else {
      data <- subset(data, region_large == input$var_region)
    }
    # add regional input when constructed columns with region input
  })
  
  # create reactive data output
  output$map <- renderLeaflet({
    
    # Add data to map, if option "Alle" is true select all data
    datafiltered <- filteredData()
 
    ordercounties <- match(map@data$ISO_A3, datafiltered$ISO_A3)
    map@data <- datafiltered[ordercounties, ]
    
    map$variableplot <- map@data[, input$var_debtindikator]
    
    # Create leaflet
    pal <- colorFactor(c(n.kritisch, l.kritisch, kritisch, s.kritisch
                         ), levels = c(0, 1, 2, 3), na.color = "#808080" )
    
    # Create text shown when mouse glides over countries
    mytext <- paste0(
      "<b>", map@data$country, "</b>","<br/>",
      "Verschuldungssituation: ", "<b>", map$variableplot, "</b>") %>%
      lapply(htmltools::HTML)
    
    # Create Map
    l <- leaflet(map, options = leafletOptions(minZoom = 2, maxZoom = 10)) %>% 
       addTiles( # urlTemplate = "//{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png"
       ) %>% addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
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
      # Set initial Zoom of Map
      setView( 0, 0, 2 ) %>%
      leaflet::addLegend(position = "bottomright",
        # Specify colors manually b/c pal function does not show colors in legend
        colors = c(s.kritisch, kritisch,  l.kritisch, n.kritisch ), 
        values = c("sehr kritisch", "kritisch", "leicht kritisch",
                              "nicht kritisch", "keine Daten vorhanden"),
        na.label = "keine Daten vorhanden",
        opacity = 0.7, title = "Verschuldungssituation",
         labels = c("sehr kritisch", "kritisch", "leicht kritisch",
                    "nicht kritisch")
      )
  })
  

}

# shinyApp
shinyApp(ui = ui, server = server)
#runApp('./erlassjahr_app.R')


################# USeful Codewaste:
# c("Land mit geringem Einkommen", "Land mit mittlerem Einkommen im unteren Bereich", "Land mit mittlerem Einkommen im oberen Bereich", 
#   "Land mit hohem Einkommen oder ohne Angaben"),
