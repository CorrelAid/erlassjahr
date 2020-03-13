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


##--------------------------------------------------##
## User Interface Shiny                             ##
##--------------------------------------------------##

ui <- fluidPage(
  titlePanel(p("Erlassjahr app", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      p("Made with Shiny"),
      selectInput(
        inputId = "variableselected",
        label = "Schuldenindikator Wählen",
        choices = c("public_debt_bip", "public_debt_state_rev", "foreign_debt_exp", 
                    "risk_excessive_debt", "external_debt_service_exp")
      )
    ),
    mainPanel(
      
      leafletOutput(outputId = "map") #,
      #DTOutput(outputId = "table")

    )
  )
)

# server()
server <- function(input, output) {
  #output$table <- renderDT(data)
  output$map <- renderLeaflet({
    
    # Add data to map
    datafiltered <- data
    ordercounties <- match(map@data$NAME_DE, datafiltered$NAME_DE)
    map@data <- datafiltered[ordercounties, ]
  
    map$variableplot <- as.numeric(
      map@data[, input$variableselected])
    
    # Create leaflet
    pal <- colorBin("YlOrRd", domain = map$variableplot, bins = 4)
    
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
      leaflet::addLegend(
        pal = pal, values = ~variableplot,
        opacity = 0.7, title = NULL
      )
  })
}

# shinyApp()
shinyApp(ui = ui, server = server)
runApp('./app.R')
