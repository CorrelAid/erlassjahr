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
        choices = c("public_debt_bip2", "public_debt_state_rev2", "foreign_debt_exp2", 
                    "risk_excessive_debt2", "external_debt_service_exp2")
      ), # ADD AGGREGATED OPTION AS COLUM IN DATAFRAME
      
      # drop down input selection income category
      selectInput(
        inputId = "var_income",
        label = "Einkommenskategorie wählen",
        choices = c("L", "LM", "UM", "NA")
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
      
      leafletOutput(outputId = "map") #,
      #DTOutput(outputId = "table")

    )
  )
)

# server()
server <- function(input, output) {
  #output$table <- renderDT(data)
  
  filteredData <- reactive({
    subset(data, income == input$var_income)
    # add regional input when constructed columns with region input
  })
  
  output$map <- renderLeaflet({
    
    # Add data to map
    datafiltered <- filteredData()
    ordercounties <- match(map@data$NAME_DE, datafiltered$NAME_DE)
    map@data <- datafiltered[ordercounties, ]
    
    map$variableplot <- as.numeric(
      map@data[, input$var_debtindikator])
    
    # Create leaflet
    pal <- colorFactor("YlOrRd", domain = map$variableplot)
    
    labels <- sprintf("%s: %g", map$NAME_DE, map$variableplot) %>%
      lapply(htmltools::HTML)
    
    l <- leaflet(map) %>% 
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(variableplot),
        color = "white",
        dashArray = "1",
        weight = 0.1, 
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.7,
        label = labels
      ) %>%
      setView( 0, 0, 2 ) %>%
      leaflet::addLegend(position = "bottomright",
        pal = pal, values = ~variableplot,
        opacity = 0.7, title = NULL
      )
  })
  

}

# shinyApp()
shinyApp(ui = ui, server = server)
runApp('./app.R')


################# USeful Codewaste:
# c("Land mit geringem Einkommen", "Land mit mittlerem Einkommen im unteren Bereich", "Land mit mittlerem Einkommen im oberen Bereich", 
#   "Land mit hohem Einkommen oder ohne Angaben"),
