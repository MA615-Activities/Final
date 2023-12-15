library(geojsonio)
library(hrbrthemes)
library(tidyverse)
library(tidyquant)

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



# Define server logic required to draw a histogram
function(input, output, session) {
  output$debug <- renderPrint({
    cat(paste(c("La", "casa", "en", "el", "árbol"),
              collapse = "\n"))
    })
  output$gdpplot <- renderPlot({
    gdp <- read_csv("gdp.csv")
    gdp <- gdp %>% select(-c(2,3,4))
    brunei <- gdp %>% filter(`Country Name`=="Brunei Darussalam")
    brunei <- brunei %>% pivot_longer(!`Country Name`, 
                                      names_to = "year", 
                                      values_to = "gdp") %>% 
      select(-`Country Name`) %>% 
      na.omit()
    gdp <- brunei %>% ggplot(aes(as.integer(year), gdp/1000000000)) +
      geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
      geom_line(group = 1, color="grey") +
      theme_bw() +
      coord_cartesian(xlim = c(input$range1[1], input$range1[2])) +
      labs(x = "Year", y = "GDP (Billion)") +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 18))
    gdp
    })
  output$co2plot <- renderPlot({
    co2 <- read_csv("co2.csv")
    co2 <- co2 %>% select(-c(2,3,4))
    brunei <- co2 %>% filter(`Country Name`=="Brunei Darussalam")
    brunei <- brunei %>% pivot_longer(!`Country Name`, 
                                      names_to = "year", 
                                      values_to = "co2") %>% 
      select(-`Country Name`) %>% 
      na.omit()
    co2 <- brunei %>% ggplot(aes(as.integer(year), co2)) +
      geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
      geom_line(group = 1, color="grey") +
      theme_bw() +
      coord_cartesian(xlim = c(input$range2[1], input$range2[2])) +
      labs(x = "Year", y = "CO2 Emissions (metric ton per capita)") +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 18))
    co2
  })
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
      leaflet::addLegend(pal = pal, values = ~region_density, opacity = 0.7, title = "Population Density",
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
