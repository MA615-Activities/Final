
library(shiny)
library(bslib)
library(shinypanels)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(shinythemes)



ui <- navbarPage(id = "tabs", 
                 title = "Brunei Darussalam", 
                 theme = shinytheme("slate"),
                 navbarMenu(title = "General Information",
                            tabPanel("About Brunei",
                                     img(src=c("brunei.jpeg"),
                                         width="300"),
                                     img(src="Flag-Brunei.jpg.webp",
                                         width="360"),
                                     img(src="night.jpg",
                                         width="450"),
                                     h1(style="font-family: 'Georgia', serif;", "Narrative Description"),
                                     p(style="font-size:13pt; font-family: 'Georgia', serif;", "Brunei Darussalam is a country located on the
                                       island of Borneo in Southeast Asia's Malay Archipelago. Borneo is divided into
                                       the two Malaysian countries of Sabah and Sarawak and the Indonesian country of
                                       Kalimantan. Brunei, however, is the only sovereign state with a small but 
                                       diverse population of around 460,000. The capital of Brunei is Bandar Seri Begawan
                                       located in the northeast. The capital also contains one of the country's national
                                       landmarks symbolizing Islamic faith, the Sultan Omar Ali Saifuddien Mosque pictured
                                       above. Oil and natural gas have played a significant role in Brunei's development
                                       and wealth. Despite this wealth, the country prioritizes conservation of its biodiversity
                                       and tropical rainforests which make up more than 70% of Brunei."),
                                     h1(style="font-family: 'Georgia', serif;", "Fun Facts"),
                                     tags$ul(
                                       tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", "Since a substantial amount of 
                                       evenue is generated from oil and gas reserves, Brunei is one of 16 countries without income tax."), 
                                       tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", "The Istana Nurul Iman, 
                                               the Sultan's official residence, holds the Guinness World Record as the largest residential palace
                                               in the world."), 
                                       tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", "The sale and consumption of 
                                               alcohol is illegal in Brunei. Alcohol is prohibited in public but non-Muslims over 17 
                                               years old are allowed restricted peresonal use.")
                                       )
                                       
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
                 nav_panel("SWOT", p("SWOT Analysis",
                                     panelsPage(
                                       panel(title = "Strengths",
                                             collapsed = TRUE,
                                             width = 300,
                                             body = h1("Nothing here")
                                             ),
                                       panel(title = "Weaknesses",
                                             collapsed = TRUE,
                                             width = 300,
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
                                             collapsed = TRUE,
                                             body = div(),
                                             footer = HTML("Footer")
                                             )
                                       )
                                     )
                           ),
                 navbarMenu(title = "Bibliography",
                            align = "right",
                            tabPanel(tags$a(href="https://en.wikipedia.org/wiki/Brunei",
                                            "Brunei Wikipedia"))
                            )
                 )


