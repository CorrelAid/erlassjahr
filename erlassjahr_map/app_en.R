## --------------------------------------------------##
## CorrelAid Project:                               ##
##    erlassjahr.de                                 ##
## --------------------------------------------------##
##  Shiny UI
##  1. Prerequisits
##  2. Load Data
##  3. UI Interface
##  4. Server
##  5. Run app
## --------------------------------------------------##



## --------------------------------------------------##
## Prerequisits                                     ##
## --------------------------------------------------##
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

# Implement modules
m_graphics <- modules::use("modules/graphics.R")
m_legends <- modules::use("modules/legends.R")

feuerIcons <- m_graphics$feuer_icons()
pfeilIcons <- m_graphics$pfeil_icons()
palette <- m_graphics$palette()
debt_legends <- m_legends$debt_legends()

## --------------------------------------------------##
## Load Data                                        ##
## --------------------------------------------------##

# load the year variable created from data_preparation.R to build the correct
# map for a given year
load("data/year_data.Rdata")

# Map Data / Shape File
shape_path <- "map_files/TM_WORLD_BORDERS_SIMPL-0.3.shp"
encoding <- "UTF-8"

lon_lat <- readOGR(
  dsn = path.expand(shape_path),
  layer = "TM_WORLD_BORDERS_SIMPL-0.3",
  encoding = encoding
)
# Select French-Guiana to merge to shapefile (Missing in ne data)
fr_guiana <- lon_lat[lon_lat$ISO3 == "GUF", ]
fr_guiana@data <- fr_guiana@data[c("ISO3", "NAME")]
fr_guiana@data <-
  dplyr::rename(fr_guiana@data, iso_a3 = ISO3, geounit = NAME)
# Rename the Polygon ID to 255 to have unique ones
slot(slot(fr_guiana, "polygons")[[1]], "ID") <- "255"

# Load Data
data <-
  get(load(paste0("data/final_data_", year, ".RData"), e <-
    new.env()), e)
rm(list = ls(envir = e), envir = e)

data[data == ""] <- NA

data$url <- paste0("<a href='", data$link, "'>", data$ISO3, "</a>")

# rename colum to match sp file
data <- dplyr::rename(data, iso_a3 = ISO3)

# Use naturalearth shapefile
sp_data <-
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sp")
# take out unnecesary data
sp_data@data <- sp_data@data[c("featurecla", "geounit", "iso_a3")]

# merge the missing polygon to the sp object
sp_data <- raster::bind(sp_data, fr_guiana)

# add Missing ISO3 Codes
sp_data@data[sp_data@data$geounit == "France", "iso_a3"] <- "FRA"
sp_data@data[sp_data@data$geounit == "Norway", "iso_a3"] <- "NOR"
sp_data@data[sp_data@data$geounit == "Kosovo", "iso_a3"] <- "RKS"

# combine data
map <- sp::merge(sp_data, data, by.x = "iso_a3", duplicateGeoms = TRUE)
map@data <- dplyr::rename(map@data, ISO3 = iso_a3)
lon_lat_merge <- lon_lat@data[, c("ISO3", "LON", "LAT")]
map <-
  sp::merge(map, lon_lat_merge, by.x = "ISO3", duplicateGeoms = TRUE)

## --------------------------------------------------##
## User Interface Shiny                             ##
## --------------------------------------------------##

ui <- fluidPage(
  # layout with main area and side bar
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
        outputId = "display_map",
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
        label = "Choose Debt Indicator",
        choices = list(
          `aggregated indicators` = "debt_sit_cat2",
          `public debt / GDP` = "public_debt_bip2",
          `public debt / government revenue` = "public_debt_state_rev2",
          `foreign debt / GDP` = "foreign_debt_bip2",
          `foreign debt / export earnings` = "foreign_debt_exp2",
          `foreign debt service / export earnings` = "external_debt_service_exp2"
        )
      ),

      # drop down input selection income category
      selectInput(
        "var_income",
        "Choose Income Group",
        choices = list(
          `All` = "Alle",
          `low income` = "l",
          `lower-middle income` = "lm",
          `upper-middle income` = "um",
          `high income` = "h"
        )
      ),
      # drop down input selection Region
      selectInput(
        "var_region",
        "Choose Region",
        choices = list(
          `All` = "Alle",
          `South Asia, Southeast Asia, Pacific` = "South Asia, Southeast Asia, Pacific",
          `Europe, CIS` = "Europe, CIS",
          `Latin America, Caribbean` = "Latin America, Caribbean",
          `Middle East, North Africa` = "Middle East, North Africa",
          `Sub-Saharan Africa` = "Sub-Saharan Africa"
        )
      ),

      # drop down input selection Risikofaktoren
      selectInput(
        "var_risiko",
        "Choose Risk Factor",
        choices = list(
          `None` = "None",
          `extractivism` = "extractivism",
          `political and social fragility` = "fragility",
          `debt structure` = "debt_prob",
          `natural disasters / climate change` = "vulnerability",
          selected = NULL
        )
      ),

      # Drop down input selection Trend anzeigen
      checkboxInput("var_trend",
        "Show Debt trend",
        value = FALSE
      ),

      # Drop down input selection Zahlungseinstellungen anzeigen
      checkboxInput(
        "var_zahlung",
        "Show Persistent and Interim Stop of Payments",
        value = FALSE
      ),
      downloadButton(outputId = "dl"),
      actionButton(
        inputId = "Method",
        label = HTML("Methodology: Debt Report&nbsp;&nbsp;&nbsp; <br/> from erlassjahr.de and Misereor"),
        icon = icon("th"),
        onclick = "window.open('https://erlassjahr.de/produkt-kategorie/schuldenreporte/')"
      )
    )
  )
)

## --------------------------------------------------##
## Farbcodierung Karte                              ##
## --------------------------------------------------##


# Choice vector for Mouseover
choiceVec <- c(
  "aggregated indicators" = "debt_sit_cat2",
  "public debt / GDP" = "public_debt_bip2",
  "public debt / government revenue" = "public_debt_state_rev2",
  "foreign debt / GDP" = "foreign_debt_bip2",
  "foreign debt / export earnings" = "foreign_debt_exp2",
  "foreign debt service / export earnings" = "external_debt_service_exp2"
)

content <- paste0(sep = "<b>", "Source erlassjahr.de", "</b>")


## --------------------------------------------------##
## Server für Shiny Map                             ##
## --------------------------------------------------##

server <- function(input, output, session) {

  # Select Data according to input
  filteredData <- reactive({
    selected_regions <- input$var_region
    selected_incomes <- input$var_income

    # if input is "All" we want all existing regions / incomes
    if (selected_regions == "All") selected_regions <- unique(map@data$region)
    if (selected_incomes == "All") selected_incomes <- unique(map@data$income)

    data <- map@data %>%
      dplyr::filter(region %in% selected_regions) %>%
      dplyr::filter(income %in% selected_incomes)

    if (input$var_risiko != "None") {
      data <- data %>%
        filter(!!sym(input$var_risiko) == 1)
    }
    return(data)
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
    } else {
      trend_var <- "trend_edse_new"
    }
  })

  legendLabels <- reactive({
    if (input$var_debtindikator %in% c(
      "debt_sit_cat2", "public_debt_bip2", "public_debt_state_rev2",
      "foreign_debt_bip2", "foreign_debt_exp2"
    )) {
      legende <- debt_legends[[input$var_debtindikator]]
    } else {
      legende <- debt_legends[["default"]]
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
    leaflet::leaflet(map, options = leaflet::leafletOptions(minZoom = 2, maxZoom = 10)) %>% #
      addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png") %>%
      addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
      setView(0, 15, 2)
  })

  # create reactive data output
  output$display_map <- leaflet::renderLeaflet({
    foundation_map()
  })


  map_conf <- reactiveValues()

  # Display colored countries dependent on the output
  observe({
    # Choose data based on selected input
    datafiltered <- filteredData()

    # match selected rows and columns in data with spatial data
    ordercounties <- match(map@data$ISO3, datafiltered$ISO3)
    map@data <- datafiltered[ordercounties, ]

    # Select Polygons for Debtindikator
    map$variableplot <- map@data[, input$var_debtindikator]

    # map url
    state_popup <-
      paste0(
        "<a href=\'",
        map@data$link,
        "', target=\"_blank\">Go to countr profile. (German only) </a>"
      )

    # Select input for mouseover text
    map$mouseover <-
      map@data[, gsub(".$", "", input$var_debtindikator)]

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
        c(palette$nT_analyse, palette$n_kritisch, palette$l_kritisch, palette$kritisch, palette$s_kritisch),
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
      "Click for contry profile"
    ) %>%
      lapply(htmltools::HTML)
    # Transform shapefile for polygon input
    map_ll <- spTransform(map, CRS("+init=epsg:4326"))

    map_conf$map <- map_ll
    map_conf$pal <- pal
    map_conf$trend_data <- trend_data
    map_conf$pay_data <- pay_data
    map_conf$Llabels <- Llabels

    # Create Map
    leaflet::leafletProxy("display_map", data = map_ll) %>%
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
        colors = c(
          palette$s_kritisch,
          palette$kritisch,
          palette$l_kritisch,
          palette$n_kritisch,
          palette$k_daten,
          palette$nT_analyse,
          palette$label_color,
          palette$label_color
        ),
        # , risk_fact ),
        opacity = 1,
        title = "Debt Situation",
        labels = Llabels
      )

    proxy <- leaflet::leafletProxy(mapId = "display_map", data = trend_data) %>%
      clearMarkers()

    # marker für Trend
    if (input$var_trend) {
      proxy %>% addMarkers(
        lat = trend_data$LAT,
        lng = trend_data$LON,
        icon = ~ pfeilIcons[trend_data$trendinput],
        label = trend_data$NAME
      )
    }
    # Marker hinzufügen für Feuer symbole
    if (input$var_zahlung) {
      proxy %>% addMarkers(
        lat = pay_data$LAT,
        lng = pay_data$LON,
        icon = ~ feuerIcons[pay_data$payment_stop],
        label = pay_data$NAME
      )
    }
  })

  download_map <- reactive({
    # Appearently, we need to basically recreate the whole thing, as there
    # is no obvious way to retrieve the leafletProxy changes and re apply them
    # to the leaflet widget..

    # However, a workaround would consist in lot of redundancy,
    # and probably long waiting time. But it works :P

    m <-
      leaflet::leaflet(map_conf$map) %>%
      addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png") %>%
      addProviderTiles(provider = "CartoDB.PositronNoLabels") %>%
      addPolygons(
        fillColor = ~ map_conf$pal(variableplot),
        color = "black",
        dashArray = "1",
        weight = 0.1,
        smoothFactor = 0.5,
        opacity = 1,
        fillOpacity = 1
      ) %>%
      clearControls() %>%
      leaflet::addLegend(
        position = "bottomright",
        colors = c(
          palette$s_kritisch,
          palette$kritisch,
          palette$l_kritisch,
          palette$n_kritisch,
          palette$k_daten,
          palette$nT_analyse,
          palette$label_color,
          palette$label_color
        ),
        # , risk_fact ),
        opacity = 1,
        title = "Debt Situation",
        labels = map_conf$Llabels
      ) %>%
      setView(
        lng = input$display_map_center$lng,
        lat = input$display_map_center$lat,
        zoom = input$display_map_zoom
      )

    if (input$var_trend) {
      output_map <- output_map %>%
        addMarkers(
          lat = map_conf$trend_data$LAT,
          lng = map_conf$trend_data$LON,
          icon = ~ pfeilIcons[map_conf$trend_data$trendinput]
        )
    }


    if (input$var_zahlung) {
      output_map <- output_map %>%
        addMarkers(
          lat = map_conf$pay_data$LAT,
          lng = map_conf$pay_data$LON,
          icon = ~ feuerIcons[map_conf$pay_data$payment_stop]
        )
    }

    return(m)
  })

  output$dl <- shiny::downloadHandler(
    filename = paste0(
      Sys.Date(),
      "_erlassjahr_custom_map",
      ".pdf"
    ),
    content = function(file) {
      webshot::mapshot(
        x = download_map(),
        file = file,
        cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
        ,
        selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      )
    } # end of content() function
  ) # end of downloadHandler() function
}

# shinyApp
shinyApp(ui = ui, server = server)
