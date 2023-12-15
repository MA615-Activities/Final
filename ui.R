
library(shiny)
library(bslib)
library(shinypanels)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(shinythemes)
library(shinyBS)
library(shinyWidgets)



ui <- navbarPage(id = "tabs", 
                 title = "Brunei Darussalam", 
                 #theme = shinytheme("slate"),
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
                                       revenue is generated from oil and gas reserves, Brunei is one of 16 countries without income tax."), 
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
                           tabsetPanel(
                             tabPanel("Population",
                                      htmlOutput("demotable"))
                           )
                           ),
                 nav_panel("Pacific Region", 
                           tabsetPanel(
                             tabPanel("GDP", 
                                      sidebarPanel(
                                        sliderTextInput("range1", "Year Range:",
                                                        choices = 1965:2022,
                                                        selected = c(1965,2022)
                                        )
                                      ),
                                      plotOutput("gdpplot")),
                             tabPanel("CO2 Emissions", 
                                      sidebarPanel(
                                        sliderTextInput("range2", "Year Range:",
                                                        choices = 1990:2020,
                                                        selected = c(1990,2020)
                                        )
                                      ),
                                      plotOutput("co2plot")),
                             tabPanel("Oil Rents", 
                                      sidebarPanel(
                                        sliderTextInput("range3", "Year Range:",
                                                        choices = 1970:2021,
                                                        selected = c(1970,2021)
                                        )
                                      ),
                                      plotOutput("oilplot"))
                             )
                           ),
                 nav_panel("SWOT", p("SWOT Analysis",
                                     panelsPage(
                                       panel(title = "Strengths",
                                             collapsed = TRUE,
                                             width = 300,
                                             body = div(h3(style="font-family: 'Georgia', serif;",
                                                           "Economic Stability"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "Brunei is rich is oil and natural gas preserves which provides
                                                                  a stable income for the country.")
                                                          ),
                                                        h3(style="font-family: 'Georgia', serif;",
                                                           "Standard of Living"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "Citizens in Brunei have a high standard of living as a result
                                                                  of low crime rates combined with high education, subsidized healthcare, 
                                                                  and many social welfare programs.")
                                                          ),
                                                        h3(style="font-family: 'Georgia', serif;",
                                                           "Location"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "Brunei's geographical location in Southeast Asia provides
                                                                  strategic access to sea trade routes.")
                                                          ),
                                                        h3(style="font-family: 'Georgia', serif;",
                                                           "Culture"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "The strong influence of Malay and Islamic culture and
                                                                  traditions attracts many tourists.")
                                                          )
                                                        )
                                             ),
                                       panel(title = "Weaknesses",
                                             collapsed = TRUE,
                                             width = 300,
                                             body = div(h3(style="font-family: 'Georgia', serif;",
                                                           "Economic Dependence"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "Brunei is heavily reliant on oil and gas exports which makes
                                                                  the economy vulnerable to changes in global oil prices. This
                                                                  dependence also hinders diversification as it hinders the
                                                                  development of other sectors.")
                                                          ),
                                                        h3(style="font-family: 'Georgia', serif;",
                                                           "Limited Workforce"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "Brunei's relatively small population leads to a limited
                                                                  workforce and talent pool for economic development.")
                                                          )
                                                        )
                                             ),
                                       panel(title = "Opportunities",
                                             width = 350,
                                             collapsed = TRUE,
                                             body = div(h3(style="font-family: 'Georgia', serif;",
                                                           "Diversification"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "Opportunities exist for diversifying the economy
                                                                  into sectors like tourism, technology, and renewable energy
                                                                  to reduce dependency on oil and natural gases.")
                                                          ),
                                                        h3(style="font-family: 'Georgia', serif;",
                                                           "Infrastructure"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "Investing in the development and improvement of 
                                                                  Brunei's infrastructure could attract foreign investments
                                                                  and enhance economic growth.")
                                                          ),
                                                        h3(style="font-family: 'Georgia', serif;",
                                                           "Regional Collaboration"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "Opportunities exist for trade and investment by Brunei's
                                                                  participation in regional economic alliances and partnerships.")
                                                          )
                                                        )
                                             ),
                                       panel(title = "Threats",
                                             collapsed = TRUE,
                                             body = div(h3(style="font-family: 'Georgia', serif;",
                                                           "Global Economic Changes"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "Changes in the global prices of oil as well as economic downturns
                                                                  can harm Brunei's economy.")
                                                          ),
                                                        h3(style="font-family: 'Georgia', serif;",
                                                           "Environmental Threats"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "Factors like deforestation, pollution, and loss of biodiversity
                                                                  threaten Brunei's natural resources and ecosystems.")
                                                          ),
                                                        h3(style="font-family: 'Georgia', serif;",
                                                           "Competition"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "Brunei's growth is also threatened by competition from
                                                                  neighboring countries for investments, tourism, and a skilled
                                                                  workforce.")
                                                          )
                                                        )
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


