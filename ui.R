
library(shiny)
library(bslib)
library(shinypanels)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(shinythemes)



ui <- navbarPage(id = "tabs", 
                 title = "Brunei Darussalam", 
                 theme = shinytheme("sandstone"),
                 navbarMenu(title = "General Information",
                            tabPanel("About Brunei",
                                     img(src="KR-flag.jpg", width="200"),
                                     p(style="font-size:12pt", "In the Republic of Kiribati, 
                                     more than 119,000 people live across 33 islands stretching 
                                     over 3.5 million square kilometers of the Pacific.
                                     The World Bank’s support to Kiribati is focused on
                                     improving roads, sustainable fisheries,
                                     health system strengthening, access to clean,
                                     safe drinking water and economic growth.")
                                     )
                            ),
                 navbarMenu(title = "Maps",
                            tabPanel("Regions of the South China Sea",
                                     img(src="South-China-Sea-Map.jpg",
                                         height="550",
                                         style="display: block; margin-left: auto; margin-right: auto;"
                                     ),
                                     p()
                            ),
                            tabPanel("Brunei World Location",
                                     leafletOutput("worldmap", height = 700),
                                     p()
                                     #actionButton("recalc", "New points")
                                     ),
                            tabPanel("Regional Map",
                                     leafletOutput("regionalmap", height = 700),
                                     p()
                                     ),
                            tabPanel("Cities Map",
                                     leafletOutput("countrymap", height = 700),
                                     p()
                            )
                 ),
                 nav_panel("Demographics", 
                           p("Tables and plots go here."),
                           shinypanels::box("box", colapsed=F)
                           ),
                 nav_panel("Pacific Region", p("A regional map goes here. Be sure to include comparisons -- maybe a table,
                                                definitely different graphs"),
                           shinydashboardPlus::box(solidHeader = FALSE,
                                                   title = "Status summary",
                                                   background = NULL,
                                                   width = 4,
                                                   status = "danger",
                                                   footer = fluidRow(column(width = 6,
                                                                            descriptionBlock(number = "17%",
                                                                                             numberColor = "green",
                                                                                             numberIcon = icon("caret-up"),
                                                                                             header = "$35,210.43",
                                                                                             text = "TOTAL REVENUE",
                                                                                             rightBorder = TRUE,
                                                                                             marginBottom = FALSE
                                                                                             )
                                                                           )
                                                                     )
                                                   )
                           ),
                 nav_panel("SWOT", p("Analysis",
                                     panelsPage(
                                       panel(title = "Strengths",
                                             width = 300,
                                             hidden = TRUE,
                                             body = h1("Nothing here")
                                             ),
                                       panel(title = "Weaknesses",
                                             width = 300,
                                             color = "magenta",
                                             hidden = FALSE,
                                             body = div(h3("Here is some info"),
                                                        hr(),
                                                        verbatimTextOutput("debug"),
                                                        hr(),
                                                        p("Lorem ipsum dolor sit amet, consectetur adipisicing elit, 
                                                        sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
                                                        Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris 
                                                        nisi ut aliquip ex ea commodoconsequat. Duis aute irure dolor in 
                                                        reprehenderit in voluptate velit esse cillum dolore eu fugiat 
                                                        nulla pariatur. Excepteur sint occaecat cupidatat non proident, 
                                                        sunt in culpa qui officia deserunt mollit anim id est laborum.")
                                                        ),
                                             hr(),
                                             h2("More info")
                                             ),
                                       panel(title = "Opportunities",
                                             width = 350,
                                             collapsed = TRUE,
                                             body = div(plotOutput("plot"))
                                             ),
                                       panel(title = "Threats",
                                             can_collapse = FALSE,
                                             body = div(),
                                             footer = HTML("Footer")
                                             )
                                       )
                                     )
                           ),
                 navbarMenu(title = "Bibliography",
                            align = "right",
                            tabPanel(tags$a(href="https://en.wikipedia.org/wiki/Kiribati",
                                            "Wikipedia/Kiribati")),
                            tabPanel(tags$a(href="https://www.shinyapps.io/",
                                            "shinyapps.io for publishing")),
                            tabPanel(tags$a(href="https://www.un.org/ohrlls/mvi/documents",
                                            "Multidimensional Vulnerability Index")),
                            tabPanel(tags$a(href = "https://kiribati.gov.ki/",
                                            "Kiribati_gov")),
                            tabPanel(tags$a(href="https://www.forumsec.org/",
                                            "Pacific Islands Forum")),
                            tabPanel(tags$a(href="https://rstudio.github.io/leaflet/",
                                            "Leaflet doc")),
                            tabPanel(tags$a(href="https://www.un.org/ohrlls/mvi/documents",
                                            "Multidimensional Vulnerability Index"))
                            )
                 )


