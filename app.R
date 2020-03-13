library(shiny)
library(DT)
library(rgdal)
library(leaflet)
#library(raster)

shape_path <- "./ne_50m_admin_0_countries.shp"
encoding <- "UTF-8"

### Read Shapefile
map <- readOGR(dsn=path.expand(shape_path), layer="ne_50m_admin_0_countries", encoding = encoding)

### Read Data and Rename Vars for Test app
# Reading in the excel table does give an error when running the Shiny app
# library("readxl")
# data <- read_excel("./SR19 Überblickstabelle.xlsx") 
data <- read.csv("./SR19 Überblickstabelle.csv")
data <- plyr::rename(data, c( "Land" = "NAME_DE", "Öffentliche.Schulden...BIP" = "Oeff.Schuld.Bip", 
                              "Öffentliche.Schulden...Staatseinnahmen" = "Oeff.Schuld.StaatEin"))



# ui object
ui <- fluidPage(
  titlePanel(p("Erlassjahr app", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      p("Made with Shiny"),
      selectInput(
        inputId = "variableselected",
        label = "Schuldenindikator Wählen",
        choices = c("Oeff.Schuld.Bip", "Oeff.Schuld.StaatEin", "Verschuldungs.situation", "Auslandsschuldenstand...Export.einnahmen", "Auslandsschuldenstand...BIP", "Auslandsschuldendienst...Export.einnahmen" )
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
runApp('./app.R')
