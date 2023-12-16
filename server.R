library(geojsonio)
library(hrbrthemes)
library(tidyverse)
library(tidyquant)
library(kableExtra)
library(htmlTable)


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
  
  output$demotable <- renderUI({
    
    # Create the table (using table from htmlTables doc as example)
    demo <- read_csv("demographics.csv")
    demo <- demo[-c(1,2,6,10,14,18:21),]
    demo <- demo %>% select(-c(1:3, 8, 13, 18))
    comma <- function(x){
      return(str_replace(x, ",", ""))
    }
    demo <- apply(demo, 2, comma)
    num <- function(x){
      return(as.numeric(x))
    }
    demo <- apply(demo, 2, num)
    demo[,1:ncol(demo)] <- demo[,1:ncol(demo)]/1000
    
    HTML(
    kbl(demo, col.names = NULL) %>% 
      kable_classic(full_width = F, html_font = "Cambria") %>% 
      add_header_above(c("Brunei Muara" = 1, "Belait" = 1, "Tutong" = 1, "Temburong" = 1, "Brunei Muara" = 1, "Belait" = 1, "Tutong" = 1, "Temburong" = 1, "Brunei Muara" = 1, "Belait" = 1, "Tutong" = 1, "Temburong" = 1, "Brunei Muara" = 1, "Belait" = 1, "Tutong" = 1, "Temburong" = 1)) %>% 
      add_header_above(c("2017" = 4, "2018" = 4, "2019" = 4, "2020" = 4)) %>%
      pack_rows("Malay", 1, 3) %>% 
      pack_rows("Chinese", 4, 6) %>% 
      pack_rows("Others", 7, 9) %>% 
      pack_rows("Total", 10, 12) %>% 
      pack_rows("Persons", 1, 1) %>% 
      pack_rows("Male", 2, 2) %>% 
      pack_rows("Female", 3, 3) %>% 
      pack_rows("Persons", 4, 4) %>% 
      pack_rows("Male", 5, 5) %>% 
      pack_rows("Female", 6, 6) %>% 
      pack_rows("Persons", 7, 7) %>% 
      pack_rows("Male", 8, 8) %>% 
      pack_rows("Female", 9, 9) %>% 
      pack_rows("Persons", 10, 10) %>% 
      pack_rows("Male", 11, 11) %>% 
      pack_rows("Female", 12, 12) %>% 
      scroll_box(width = "1500px", height = "450px")
    )
  })
  
  output$religiontable <- renderUI({
    
    # Create the table (using table from htmlTables doc as example)
    reli <- read_csv("religion.csv")
    colnames(reli) <- c("Religious Group", "Brunei", "South-Eastern Asia", "The World")
    
    HTML(
      kbl(reli) %>% 
        kable_classic(full_width = T, html_font = "Cambria") %>%
        scroll_box(width = "1500px", height = "450px")
    )
  })
  
  gdp <- reactive({
    if(input$countries == "Brunei"){
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
        coord_cartesian(xlim = c(input$range1[1]+1, input$range1[2]-1)) +
        labs(x = "Year", y = "GDP (Billion)") +
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 18))
      plot <- gdp
    }
    if(input$countries == "Brunei & Timor-Leste"){
      gdp2 <- read_csv("gdp.csv")
      gdp2 <- gdp2 %>% select(-c(2,3,4))
      brunei <- gdp2 %>% filter(`Country Name`=="Brunei Darussalam")
      brunei <- brunei %>% pivot_longer(!`Country Name`, 
                                        names_to = "year", 
                                        values_to = "bruneigdp") %>% 
        select(-`Country Name`) %>% 
        na.omit()
      timor <- read_csv("timorgdp.csv")
      timor <- timor %>% select(-c(2,3,4))
      timor <- gdp2 %>% filter(`Country Name`=="Timor-Leste")
      timor <- timor %>% pivot_longer(!`Country Name`, 
                                      names_to = "year", 
                                      values_to = "timorgdp") %>% 
        select(-`Country Name`)
      timor <- timor[-c(1:5, 64),]
      data <- cbind(brunei, timor)
      data <- data %>% pivot_longer(!year, names_to = "Country", values_to = "gdp")
      data$gdp <- data$gdp/1000000000
      data$Country <- str_replace(data$Country, "bruneigdp", "Brunei")
      data$Country <- str_replace(data$Country, "timorgdp", "Timor-Leste")
      
      gdp2 <- data %>% ggplot(aes(as.integer(year), gdp)) +
        geom_line(aes(color=Country), lwd = 2) +
        theme_bw() +
        coord_cartesian(xlim = c(input$range1[1]+1, input$range1[2]-1)) +
        labs(x = "Year", y = "GDP (Billion)") +
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 18))
      plot <- gdp2
    }
    if(input$countries=="Brunei & Indonesia"){
      gdp3 <- read_csv("gdp.csv")
      gdp3 <- gdp3 %>% select(-c(2,3,4))
      brunei <- gdp3 %>% filter(`Country Name`=="Brunei Darussalam")
      brunei <- brunei %>% pivot_longer(!`Country Name`, 
                                        names_to = "year", 
                                        values_to = "bruneigdp") %>% 
        select(-`Country Name`) %>% 
        na.omit()
      indo <- read_csv("indogdp.csv")
      indo <- indo %>% select(-c(2,3,4))
      indo <- gdp3 %>% filter(`Country Name`=="Indonesia")
      indo <- indo %>% pivot_longer(!`Country Name`, 
                                      names_to = "year", 
                                      values_to = "indogdp") %>% 
        select(-`Country Name`) %>% 
        na.omit()
      indo <- rbind(NA, NA, indo)
      indo[1,1] <- "1965"
      indo[2,1] <- "1966"
      data <- cbind(brunei, indo)
      data <- data %>% pivot_longer(!year, names_to = "Country", values_to = "gdp")
      data$gdp <- data$gdp/1000000000
      data$Country <- str_replace(data$Country, "bruneigdp", "Brunei")
      data$Country <- str_replace(data$Country, "indogdp", "Indonesia")
      
      gdp3 <- data %>% ggplot(aes(as.integer(year), gdp)) +
        geom_line(aes(color=Country), lwd = 2) +
        theme_bw() +
        coord_cartesian(xlim = c(input$range1[1]+1, input$range1[2]-1)) +
        labs(x = "Year", y = "GDP (Billion)") +
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 18))
      plot <- gdp3
    }
    return(plot)
  })
  
  output$gdpplot <- renderPlot({
    gdp()
    })
  
  
  co2 <- reactive({
    if(input$countries2 == "Brunei"){
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
        coord_cartesian(xlim = c(input$range2[1]+1, input$range2[2]-1)) +
        labs(x = "Year", y = "CO2 Emissions (metric ton per capita)") +
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 18))
      plot <- co2
    }
    if(input$countries2 == "Brunei & Timor-Leste"){
      co2 <- read_csv("co2.csv")
      co2 <- co2 %>% select(-c(2,3,4))
      brunei <- co2 %>% filter(`Country Name`=="Brunei Darussalam")
      brunei <- brunei %>% pivot_longer(!`Country Name`, 
                                        names_to = "year", 
                                        values_to = "bruneico2") %>% 
        select(-`Country Name`) %>% 
        na.omit()
      co22 <- read_csv("timorco2.csv")
      co22 <- co22 %>% select(-c(2,3,4))
      timor <- co22 %>% filter(`Country Name`=="Timor-Leste")
      timor <- timor %>% pivot_longer(!`Country Name`, 
                                        names_to = "year", 
                                        values_to = "timorco2") %>% 
        select(-`Country Name`) %>% 
        na.omit()
      data <- cbind(brunei, timor)
      data <- data %>% pivot_longer(!year, names_to = "Country", values_to = "co2")
      data$Country <- str_replace(data$Country, "bruneico2", "Brunei")
      data$Country <- str_replace(data$Country, "timorco2", "Timor-Leste")
      
      co22 <- data %>% ggplot(aes(as.integer(year), co2)) +
        geom_line(aes(color=Country), lwd = 2) +
        theme_bw() +
        coord_cartesian(xlim = c(input$range2[1]+1, input$range2[2]-1)) +
        labs(x = "Year", y = "CO2 Emissions (metric ton per capita)") +
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 18))
      
      plot <- co22
    }
    if(input$countries2=="Brunei & Indonesia"){
      co2 <- read_csv("co2.csv")
      co2 <- co2 %>% select(-c(2,3,4))
      brunei <- co2 %>% filter(`Country Name`=="Brunei Darussalam")
      brunei <- brunei %>% pivot_longer(!`Country Name`, 
                                        names_to = "year", 
                                        values_to = "bruneico2") %>% 
        select(-`Country Name`) %>% 
        na.omit()
      co23 <- read_csv("indoco2.csv")
      co23 <- co23 %>% select(-c(2,3,4))
      indo <- co23 %>% filter(`Country Name`=="Indonesia")
      indo <- indo %>% pivot_longer(!`Country Name`, 
                                      names_to = "year", 
                                      values_to = "indoco2") %>% 
        select(-`Country Name`) %>% 
        na.omit()
      data <- cbind(brunei, indo)
      data <- data %>% pivot_longer(!year, names_to = "Country", values_to = "co2")
      data$Country <- str_replace(data$Country, "bruneico2", "Brunei")
      data$Country <- str_replace(data$Country, "indoco2", "Indonesia")
      
      co23 <- data %>% ggplot(aes(as.integer(year), co2)) +
        geom_line(aes(color=Country), lwd = 2) +
        theme_bw() +
        coord_cartesian(xlim = c(input$range2[1]+1, input$range2[2]-1)) +
        labs(x = "Year", y = "CO2 Emissions (metric ton per capita)") +
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 18))
      
      plot <- co23
      
    }
    return(plot)
  })
  
  
  output$co2plot <- renderPlot({
    co2()
  })  
  
  oil <- reactive({
    if(input$countries3 == "Brunei"){
      oil <- read_csv("oil.csv")
      oil <- oil %>% select(-c(2,3,4))
      brunei <- oil %>% filter(`Country Name`=="Brunei Darussalam")
      brunei <- brunei %>% pivot_longer(!`Country Name`, 
                                        names_to = "year", 
                                        values_to = "oil") %>% 
        select(-`Country Name`) %>% 
        na.omit()
      oil <- brunei %>% ggplot(aes(as.integer(year), oil)) +
        geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
        geom_line(group = 1, color="grey") +
        theme_bw() +
        coord_cartesian(xlim = c(input$range3[1]+1, input$range3[2]-1)) +
        labs(x = "Year", y = "Oil Rents (% of GDP)") +
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 18))
      plot <- oil
    }
    if(input$countries3 == "Brunei & Timor-Leste"){
      oil <- read_csv("oil.csv")
      oil <- oil %>% select(-c(2,3,4))
      brunei <- oil %>% filter(`Country Name`=="Brunei Darussalam")
      brunei <- brunei %>% pivot_longer(!`Country Name`, 
                                        names_to = "year", 
                                        values_to = "bruneioil") %>% 
        select(-`Country Name`) %>% 
        na.omit()
      oil2 <- read_csv("timoroil.csv")
      oil2 <- oil2 %>% select(-c(2,3,4))
      timor <- oil2 %>% filter(`Country Name`=="Timor-Leste")
      timor <- timor %>% pivot_longer(!`Country Name`, 
                                      names_to = "year", 
                                      values_to = "timoroil") %>% 
        select(-`Country Name`)
      timor <- timor[-c(1:10, 20, 63, 64),]
      data <- cbind(brunei, timor)
      data <- data %>% pivot_longer(!year, names_to = "Country", values_to = "oil")
      data$Country <- str_replace(data$Country, "bruneioil", "Brunei")
      data$Country <- str_replace(data$Country, "timoroil", "Timor-Leste")
      
      oil2 <- data %>% ggplot(aes(as.integer(year), oil)) +
        geom_line(aes(color=Country), lwd = 2) +
        theme_bw() +
        coord_cartesian(xlim = c(input$range3[1]+1, input$range3[2]-1)) +
        labs(x = "Year", y = "Oil Rents (% of GDP)") +
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 18))

      plot <- oil2
    }
    if(input$countries3 == "Brunei & Indonesia"){
      oil <- read_csv("oil.csv")
      oil <- oil %>% select(-c(2,3,4))
      brunei <- oil %>% filter(`Country Name`=="Brunei Darussalam")
      brunei <- brunei %>% pivot_longer(!`Country Name`, 
                                        names_to = "year", 
                                        values_to = "bruneioil") %>% 
        select(-`Country Name`) %>% 
        na.omit()
      oil3 <- read_csv("indooil.csv")
      oil3 <- oil3 %>% select(-c(2,3,4))
      indo <- oil3 %>% filter(`Country Name`=="Indonesia")
      indo <- indo %>% pivot_longer(!`Country Name`, 
                                      names_to = "year", 
                                      values_to = "indooil") %>% 
        select(-`Country Name`)
      indo <- indo[-c(1:10, 20, 63, 64),]
      data <- cbind(brunei, indo)
      data <- data %>% pivot_longer(!year, names_to = "Country", values_to = "oil")
      data$Country <- str_replace(data$Country, "bruneioil", "Brunei")
      data$Country <- str_replace(data$Country, "indooil", "Indonesia")
      
      oil3 <- data %>% ggplot(aes(as.integer(year), oil)) +
        geom_line(aes(color=Country), lwd = 2) +
        theme_bw() +
        coord_cartesian(xlim = c(input$range3[1]+1, input$range3[2]-1)) +
        labs(x = "Year", y = "Oil Rents (% of GDP)") +
        theme(axis.text = element_text(size = 16),
              axis.title = element_text(size = 18))
      
      plot <- oil3
    }
    return(plot)
  })
  
  
  
  output$oilplot <- renderPlot({
    oil()
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
