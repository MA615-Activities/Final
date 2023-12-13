library(geojsonio)

brunei <- list(lat = 4.5353, lng = 114.7277)

brunei_polys <- geojson_read("polys.json", what="sp")

all_polys <- geojson_read("admin1.geojson", what="sp")

region_density <- c("24.06", "510", "7.242", "40.49")
bins <- c(0, 10, 30, 50, 300, 600)
pal <- colorBin("YlOrRd", domain = as.numeric(region_density), bins = bins)

labels <- sprintf("<strong>%s</strong><br/>%s people / km<sup>2</sup>",
                  subset(all_polys, country == "Brunei")$name,
                  region_density) %>% 
  lapply(htmltools::HTML)

cities <- read.csv(file = "cities.csv")
islands <- read.csv(file = "ki_is.csv")

# Define server logic required to draw a histogram
function(input, output, session) {
  output$debug <- renderPrint({
    cat(paste(c("La", "casa", "en", "el", "árbol"),
              collapse = "\n"))
    })
  output$plot <- renderPlot(
    plot(cars)
    )
  output$worldmap <- renderLeaflet({
    leaflet()  %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lng = brunei$lng, lat = brunei$lat, zoom = 2) %>%
      addMarkers(lng = brunei$lng, lat = brunei$lat, label = "Brunei Darussalam")
    })
  output$regionalmap <- renderLeaflet({
    leaflet(subset(all_polys, country == "Brunei"))  %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
      addPolygons(
        fillColor = ~pal(as.numeric(region_density)),
        color = "black",
        dashArray = "",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "green",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels
        ) %>% 
      addLegend(pal = pal, values = ~region_density, opacity = 0.7, title = "Population Density",
                       position = "topright") %>%
      setView(lng = brunei$lng, lat = brunei$lat, zoom = 9)
  })
  output$countrymap <- renderLeaflet({
    leaflet()  %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = brunei$lng,
              lat = brunei$lat, zoom = 9) %>%
      addCircleMarkers(data = cities,
                       fillColor="darkslateblue",
                       stroke = TRUE,
                       color="darkslateblue",
                       radius = 4,
                       label = cities$city,
                       labelOptions = labelOptions(noHide=T,
                                                   textOnly = T))
    })
  }
