# Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="../world_shape_file.zip")

# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
system("unzip ../world_shape_file.zip")

shape_path <- "./TM_WORLD_BORDERS_SIMPL-0.3.shp"
encoding <- "UTF-8"

world_spdf <- readOGR(dsn=path.expand(shape_path), layer="TM_WORLD_BORDERS_SIMPL-0.3", encoding = encoding)
data <- plyr::rename(sr19_erlassjahr, c("country" = "ISO3"))
world_spdf <- sp::merge(world_spdf, data, by.x="ISO3", duplicateGeoms=TRUE)


# define color scheme
erlassjahr_cols <- c(
  `n.kritisch`      = "#C5C7B8",
  `l.kritisch`       = "#FFCC99",
  `kritisch`     = "#FF8040",
  `s.kritisch`     = "#E61700")

# define color palette
mypalette <- colorFactor(palette=erlassjahr_cols,
                         domain=world_spdf@data$debt_sit_cat, na.color="#F0F2E8")

# define text to be seen when hovering over the coutnry
mytext <- paste0(
  "<b>", world_spdf@data$NAME, "</b>","<br/>",
  "Verschuldungssituation: ", "<b>", world_spdf@data$debt_sit_cat, "</b>") %>%
  lapply(htmltools::HTML)

# map
leaflet(world_spdf) %>% 
  addProviderTiles(provider = "CartoDB.PositronNoLabels")  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(debt_sit_cat), 
    stroke=TRUE, 
    fillOpacity = 1, 
    color="#F0F2E8", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend(pal=mypalette, values=~debt_sit_cat, 
            opacity=0.9, title = "Verschuldungssituation", position = "bottomleft", na.label = "nicht Teil des Samples",
            labels = mypalette)
