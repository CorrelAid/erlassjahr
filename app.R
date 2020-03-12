library(shiny)
library(DT)
library(rgdal)
library(leaflet)
#library(raster)

data <- read.csv('C:/Users/scher/Documents/Data AS/Projekt Erlassjahr/Data/SR19 Überblickstabelle.csv')
shape_path <- "C:/Users/scher/Documents/Data AS/Projekt Erlassjahr/NatEarth/ne_50m_admin_0_countries.shp"
encoding <- "UTF-8"
map <- readOGR(dsn=path.expand(shape_path), layer="ne_50m_admin_0_countries", encoding = encoding)


# ui object
ui <- fluidPage(
  titlePanel(p("Erlassjahr app", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      p("Made with Shiny"),
      selectInput(
        inputId = "variableselected",
        label = "Select variable",
        choices = c("Verschuldungs.situation", "Trend")
      )
    ),
    mainPanel(
      
      leafletOutput(outputId = "map"),
      DTOutput(outputId = "table")

    )
  )
)

# server()
server <- function(input, output) {
  output$table <- renderDT(data)
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
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~variableplot,
        opacity = 0.7, title = NULL
      )
  })
}

# shinyApp()
shinyApp(ui = ui, server = server)
runApp('C:/Users/scher/Documents/Data AS/Projekt Erlassjahr/appdir/app.R')
