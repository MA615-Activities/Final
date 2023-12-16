
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
                            tabPanel("Regions of Maritime Southeast Asia",
                                     img(src="South-China-Sea-Map.jpg",
                                         height="550",
                                         style="display: block; margin-left: auto; margin-right: auto;"
                                     ),
                                     p(style="font-size:13pt; font-family: 'Georgia', serif;", "Brunei is an island state located on the
                                       larger island of Borneo in Southeast Asia. Brunei is bordered in the north by the South China Sea
                                       and Malaysia elsewhere. Other island countries in this region include Indonesia which encompass parts 
                                       of Borneo, Timor-Leste near the southern parts of Indonesia, the Philippines northeast of Brunei, and 
                                       Singapore at the southern tip of the Malay Peninsula.")
                            ),
                            tabPanel("Brunei World Location",
                                     leafletOutput("worldmap", height = 700),
                                     
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
                             tabPanel("Population by Gender & Ethnicity",
                                      htmlOutput("demotable"),
                                      p(style="font-size:13pt; font-family: 'Georgia', serif;", "This demographic table shows
                                        the population in thousands of people distributed in the different regions of Brunei
                                        by gender and ethnicity. From this table, we can see that the most populated region
                                        is Brunei Maura which is where the capital is located. Additionally, from these
                                        four recorded years, the majority of citizens in all regions of Brunei are males and
                                        the largest ethnic group is Malay.")),
                             tabPanel("Religion",
                                      htmlOutput("religiontable"),
                                      p(style="font-size:13pt; font-family: 'Georgia', serif;", "This demographic table shows
                                        the percentage of Brunei's population that is each religious group and compares to religious
                                        group distribution in South-Eastern Asia and the world. While the largest religious group
                                        in the world is Christians, in Brunei and all of South-Eastern Asia, the largest religious
                                        group is Muslims, most being Sunnis. The official religion of Brunei is Islam and most of
                                        the country's festivals and events revolve around the religion."))
                           )
                           ),
                 nav_panel("Maritime Southeast Asia Region", 
                           tabsetPanel(
                             tabPanel("GDP", 
                                      sidebarPanel(
                                        sliderTextInput("range1", "Year Range:",
                                                        choices = 1965:2022,
                                                        selected = c(1965,2022)
                                        ),
                                        selectInput("countries", "Countries:",
                                                    c("Brunei",
                                                      "Brunei & Timor-Leste",
                                                      "Brunei & Indonesia")
                                                    )
                                        ),
                                      mainPanel(plotOutput("gdpplot")),
                                      p(style="font-size:13pt; font-family: 'Georgia', serif;", "These graphs show the Gross
                                          Domestic Product in billions of dollars for Brunei and compares it to the GDP of
                                          Timor-Leste and Indonesia which are other island countries in the region. Timor-Leste
                                          is a much larger country than Brunei with over two times the population of Brunei. Yet
                                          according to recent years' data, the GDP or economic wealth of Brunei is more than 5
                                          billion dollars larger than Timor-Leste.")
                                      ),
                            tabPanel("CO2 Emissions", 
                                      sidebarPanel(
                                        sliderTextInput("range2", "Year Range:",
                                                        choices = 1990:2020,
                                                        selected = c(1990,2020)
                                        ),
                                        selectInput("countries2", "Countries:",
                                                    c("Brunei",
                                                      "Brunei & Timor-Leste",
                                                      "Brunei & Indonesia")
                                        )
                                      ),
                                     mainPanel(plotOutput("co2plot")),
                                     p(style="font-size:13pt; font-family: 'Georgia', serif;", "These graphs show the Carbon Dioxide
                                          emissions in metric tons per capita for Brunei and compares it to the CO2 emissions of
                                          Timor-Leste and Indonesia. Although Brunei is a much smaller country compared to Timor-Leste
                                          and Indonesia, Brunei still produces over 5 times more CO2 emissions.")
                                      ),
                             tabPanel("Oil Rents", 
                                      sidebarPanel(
                                        sliderTextInput("range3", "Year Range:",
                                                        choices = 1970:2021,
                                                        selected = c(1970,2021)
                                        ),
                                        selectInput("countries3", "Countries:",
                                                    c("Brunei",
                                                      "Brunei & Timor-Leste",
                                                      "Brunei & Indonesia")
                                        )
                                      ),
                                      mainPanel(plotOutput("oilplot")),
                                      p(style="font-size:13pt; font-family: 'Georgia', serif;", "These graphs show the Oil Rents for 
                                          Brunei and compares it to the CO2 emissions of Timor-Leste and Indonesia. The World
                                          Bank defines oil rents as the difference between the value of crude oil production at 
                                          regional prices and the total cost of production.")
                                      )
                             )
                           ),
                 nav_panel("SWOT", p("SWOT Analysis",
                                     panelsPage(
                                       shinypanels::panel(title = "Strengths",
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
                                                          ),
                                                        img(src="culture.jpeg",
                                                            width="300",
                                                            style="display: block; margin-left: auto; margin-right: auto;")
                                                        )
                                             ),
                                       shinypanels::panel(title = "Weaknesses",
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
                                                        img(src="oil.jpeg",
                                                            width="300",
                                                            style="display: block; margin-left: auto; margin-right: auto;"),
                                                        h3(style="font-family: 'Georgia', serif;",
                                                           "Limited Workforce"),
                                                        tags$ul(
                                                          tags$li(style="font-size:13pt; font-family: 'Georgia', serif;", 
                                                                  "Brunei's relatively small population leads to a limited
                                                                  workforce and talent pool for economic development.")
                                                          )
                                                        )
                                             ),
                                       shinypanels::panel(title = "Opportunities",
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
                                                        img(src="tourism.png",
                                                            width="290",
                                                            style="display: block; margin-left: auto; margin-right: auto;"),
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
                                       shinypanels::panel(title = "Threats",
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
                                                        img(src="forest.jpeg",
                                                            width="300",
                                                            style="display: block; margin-left: auto; margin-right: auto;"),
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
                                            "Brunei Wikipedia")),
                            tabPanel(tags$a(href="https://data.worldbank.org/country/brunei-darussalam",
                                            "The World Bank")),
                            tabPanel(tags$a(href="https://deps.mofe.gov.bn/SitePages/Population.aspx",
                                            "Demographic Data"))
                            )
                 )


