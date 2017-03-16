
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(SPARQL) ; library(DT); library(ggplot2)

# shinyUI(fluidPage(
#   fluidRow(
#     column(7, offset = 1,
#            br(),
#            div(h3("Gender Pay Gap")),
#            div(h4(textOutput("const_name"))),
#            div(h4("The gap between male and female pay by parliamentary constituency")),
#            div(h4(textOutput("title"), align = "center"), style = "color:black"),
#            div(h5(textOutput("period"), align = "center"), style = "color:black"),
#            br())),
#   fluidRow(
#     column(7, offset = 1,
#            tabsetPanel(
#              tabPanel("Map",leafletOutput("map", height="600"),
#                       br(),
#                       actionButton("reset_button", "Reset view")),
#              tabPanel("Table",DT::dataTableOutput("table"))
#            )
#     ),
#     column(3,
#            sliderInput("smokerange", "Choose the range of values you would like to display", min = -5000, max = 20000, value = c(-5000,20000)),
#            plotOutput("plot1"),
#            br()))
#   
# ))

navbarPage("ONS Linked Data", id="nav",
           
           tabPanel("MAP",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls",style = " height: 100vh; overflow-y: auto; ", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 450, height = "auto",
                                      
                                      h2("Choose Data"),
                                      selectInput("map","Show map:",
                                                  c("Median Pay (ONS)" = "mapapay",
                                                    "Median Male Pay (ONS)" = "mapmpay",
                                                    "Median Female Pay (ONS)" = "mapfpay",
                                                    "Pay Gap (ONS)" = "mapgap",
                                                    "Breastfeeding (ScotGov)" = "mapbf",
                                                    "Deliberate Fires (ScotGov)" = "mapfire",
                                                    "Jobseekers (ScotGov)" = "mapjsa",
                                                    "Dwellings per Hectare (ScotGov)" = "mapdwell",
                                                    "Alcohol-related Discharge (ScotGov)" = "mapalc"
                                                                 )),
                                      selectInput("filter","Filter by:",
                                                  c("Median Pay (ONS)" = "filterapay",
                                                    "Median Male Pay (ONS)" = "filtermpay",
                                                    "Median Female Pay (ONS)" = "filterfpay",
                                                    "Pay Gap (ONS)" = "filtergap",
                                                    "Breastfeeding (ScotGov)" = "filterbf",
                                                    "Deliberate Fires (ScotGov)" = "filterfire",
                                                    "Jobseekers (ScotGov)" = "filterjsa",
                                                    "Dwellings per Hectare (ScotGov)" = "filterdwell",
                                                    "Alcohol-related Discharge (ScotGov)" = "filteralc"
                                                  )),
                                      h5("Filter constituencies by the gap between male and female median pay"),
                                      #sliderInput("paygaprange", "Choose the range of values you would like to display", min = -5000, max = 20000, value = c(-5000,20000)),
                                      numericInput("lower", "Paygap between", value= -5000),
                                      numericInput("upper", "and", value=20000),
                                      plotOutput("plot1")
                                      #parking this for now - arranging two chart elements side-by-side
                                      #fluidRow(
                                      #  column(6,plotOutput("plot1"))#,
                                      #  column(6,plotOutput("plot2"))
                                      #)
                                      
                                      
                                      
                                      
                                      #selectInput("color", "Color", vars),
                                      #selectInput("size", "Size", vars, selected = "adultpop"),
                                      # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                      #                  # Only prompt for threshold when coloring or sizing by superzip
                                      #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      # ),
                                      
                                      #plotOutput("histCentile", height = 200),
                                      #plotOutput("scatterCollegeIncome", height = 250)
                        )
                        
                        #tags$div(id="cite",
                        #         'Data compiled for ', tags$em('Coming Apart: The Fucking Absolute State of the UK, 2017'), ' by Jamie Whyte (Crown Forum, 2012).'
                        #)
                    )
           ),
           
           tabPanel("DATA",
                    fluidRow(
                      column(3,
                             div(h3("Gender Pay Gap"))
                      ),
                      column(3,
                             div(h3("Gender Pay Gap"))
                      ),
                      column(3,
                             div(h3("Gender Pay Gap"))
                      )
                    ),
                    fluidRow(
                      column(1,
                             div(h3("Gender Pay Gap"))
                      ),
                      column(1,
                             div(h3("Gender Pay Gap"))
                      )
                    )
           ),
           
           tabPanel("ABOUT",
                    fluidRow(
                      column(1,
                             div(h5("About this tool"))
                             )
                      )
                    ),
           
           conditionalPanel("false", icon("crosshair"))
)
