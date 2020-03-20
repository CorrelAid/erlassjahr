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
shape_path <- "./ne_50m_admin_0_countries.shp"
encoding <- "UTF-8"

map <- readOGR(dsn=path.expand(shape_path), layer="ne_50m_admin_0_countries", encoding = encoding)

# Convert Data from data_cleaning.R into csv File
write.csv(sr19_erlassjahr, "./sr19_erlassjahr.csv")
data <- read.csv("./sr19_erlassjahr.csv")
data <- plyr::rename(data, c("country" = "NAME_DE"))

# Combine data:
datamap <- sp::merge(map, data, by.x="NAME_DE", duplicateGeoms=TRUE)


##--------------------------------------------------##
## User Interface Shiny                             ##
##--------------------------------------------------##

ui <- fluidPage(
  titlePanel(title = h1("Erlassjahr app", style = "color:#3474A7", align = "center")),
  #layout with main area and side bar
   sidebarLayout( position = "left",
    sidebarPanel(
      p("Auswahl Variablen"),
  
      # dropdown input selection Debt Indicator
      selectInput(
        inputId = "var_debtindikator",
        label = "Schuldenindikator Wählen",
        choices = c("public_debt_bip2", "public_debt_state_rev2", "foreign_debt_bip2", 
                    "foreign_debt_exp2", "external_debt_service_exp2")
      ), # ADD AGGREGATED OPTION AS COLUM IN DATAFRAME
      
      # drop down input selection income category
      selectInput(
        inputId = "var_income",
        label = "Einkommenskategorie wählen",
        choices = c("Alle","L", "LM", "UM", "NA")
      ),
        # drop down input selection Region
        selectInput("var_region", 
                    "Region wählen", 
                    choices = c("Afrika", "Europa", "Amerika", "Australien", "Asien")
    ), # ADD REGION VARIABLE IN DATAFRAME
    
        # drop down input selection Trend anzeigen
        selectInput("var_trend", 
                    "Trend anzeigen", 
                    choices = c("Ja", "Nein")
      )),
    mainPanel(
        div(class="outer",
            tags$style(type = "text/css", 
                       ".outer {position: fixed; top: 0; 
                       left: 0; right: 0; bottom: 0; overflow: 
                       hidden; padding: 0}"),
          

      leafletOutput(outputId = "map", height = "100%", width = "100%"))
      #DTOutput(outputId = "table")

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
  
  filteredData <- reactive({
    if (input$var_income == "Alle") {
      subset(data, income == income) 
    } else {
    subset(data, income == input$var_income)
    }
    # add regional input when constructed columns with region input
  })
  # choose_region <- reactive({
  #   if (input$var_region == "Afrika") {
  #     long <- 
  #       lat <- 
  #       zoom <- 12
  #   } # add regional input when constructed columns with region input
  # })
  
  output$map <- renderLeaflet({
    
    # Add data to map, if option "Alle" is true select all data
    datafiltered <- filteredData()
 
    ordercounties <- match(map@data$NAME_DE, datafiltered$NAME_DE)
    map@data <- datafiltered[ordercounties, ]
    
    map$variableplot <- map@data[, input$var_debtindikator]
    
    # Create leaflet
    pal <- colorFactor(c(n.kritisch, l.kritisch, kritisch, s.kritisch
                         ), levels = c(0, 1, 2, 3), na.color = "#808080" )
    
    # labels <- sprintf("%s: %g", map$NAME_DE, map$variableplot) %>%
    #   lapply(htmltools::HTML)
    
    mytext <- paste0(
      "<b>", map@data$NAME_DE, "</b>","<br/>",
      "Verschuldungssituation: ", "<b>", map$variableplot, "</b>") %>%
      lapply(htmltools::HTML)
    
    l <- leaflet(map) %>% 
      addTiles() %>%
      addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
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
runApp('./erlassjahr_app.R')


################# USeful Codewaste:
# c("Land mit geringem Einkommen", "Land mit mittlerem Einkommen im unteren Bereich", "Land mit mittlerem Einkommen im oberen Bereich", 
#   "Land mit hohem Einkommen oder ohne Angaben"),
