
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny) ; library(dplyr) ; library(rgdal) ; library(leaflet) ; library(raster) ; library(SPARQL) ; library(DT) ; library(reshape2) ; library(ggplot2)

endpoint <- "http://ons.publishmydata.com/sparql"
scotpayquery <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?s ?value ?areaname ?sexname ?areacode ?statname WHERE {
?s qb:dataSet <http://statistics.data.gov.uk/data/annual-survey-of-hours-and-earnings-2016-earnings> ;
<http://statistics.data.gov.uk/def/measure-properties/value> ?value ;
<http://statistics.data.gov.uk/def/dimension/earnings> <http://statistics.data.gov.uk/def/concept/earnings/annual-pay-gross>;
<http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?area;
<http://statistics.data.gov.uk/def/dimension/earningsStatistics> ?statcode;
<http://purl.org/linked-data/sdmx/2009/dimension#sex> ?sex;
<http://statistics.data.gov.uk/def/dimension/workingPattern> <http://statistics.data.gov.uk/def/concept/working-pattern/full-time> .
?area <http://statistics.data.gov.uk/def/statistical-entity#code> <http://statistics.data.gov.uk/id/statistical-entity/S12> ;
<http://statistics.data.gov.uk/def/statistical-geography#officialname> ?areaname .
?sex rdfs:label ?sexname .
?area rdfs:label ?areacode .
?statcode rdfs:label ?statname .
}"

# SPARQL is too big to run succesfully
#qd <- SPARQL(endpoint,query)

# Load the constituency geography file
scotcouncil <- readOGR("ScottishCouncilAreas2_simplified.geojson", "OGRGeoJSON")

# Load the csv file containing the pay data
pgdata <- read.csv2("ons_ashe_scot_paygap.csv",header = TRUE, sep=",")

#Turn the value column into from scientific notation to number
pgdata <- transform(pgdata, value = as.numeric(value))

pgdata <- as.data.frame(pgdata)
pgdata2 <- pgdata[ which(pgdata$statname == "Median"), ]
#pgdata <- subset(pgdata, year == "2016" & stat == "Median")
pgdata2 <- dcast(pgdata2,areaname + areacode ~ sexname, value.var = 'value')
pgdata2$gap <- with(pgdata2,Male - Female)

#pgdist <- pgdata[ which(pgdata$year == "2016"), ]

pgdist2 <- dcast(pgdata,areaname + areacode + statname ~ sexname, value.var = 'value')

pgdist2$gap <- with(pgdist2, Male - Female)

#Pluck the national figure out of the data
pgdist2Nat <- pgdist2[ which(pgdist2$areacode == "K03000001"),]



server <- (function(input, output, session) {
  
  # This bit is the bit that responds to the slider on the UI
  #selected <- reactive({
  # subset(smokpreg,
  #        percsmoking < input$smokerange[2] & percsmoking >= input$smokerange[1])
  #})
  
  selected <- reactive({
    subset(pgdata2,
           gap < input$upper & gap >= input$lower)
  })
  
  # Put the default map co-ordinates and zoom level into variables
  lat <- 54.542788
  lng <- -3.144708
  zoom <- 6
  
  # Draw the map
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      #maybe set a more colourful map background..?
      addProviderTiles("Esri.WorldStreetMap") %>% 
      setView(lat = lat, lng = lng, zoom = zoom) %>%
      addPolygons(data = scotcouncil, opacity=1, color = "black", weight = 1, fillOpacity=0.5, layerId = scotcouncil$CODE)
    
  })
  
  # Draw the table (the columns need to match with all those in selected())
  output$table <- DT::renderDataTable({
    data.frame(x=selected())}, colnames = c('areaname','areacode','All','Female','Male','gap'), options = list(order = list(7,'desc')))
  
  #
  observe({
    
    #merge the data from the csv / sparql with the geojson data for mapping
    scotcouncil@data <- left_join(scotcouncil@data, selected(), by=c("CODE"="areacode"))
    
    #sets the colour range to be used on the choropleth
    qpal <- colorNumeric("Spectral", pgdata2$gap, na.color = "#bdbdbd")
    
    #the popup on the map
    popup <- paste0("<h5>",scotcouncil$NAME,"</h5><br /><h3>£",scotcouncil$gap,"</h3>")
    
    #draw the map with stuff on
    leafletProxy("map", data = scotcouncil) %>%
      addProviderTiles("Esri.WorldStreetMap") %>% 
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(data = scotcouncil, fillColor = ~qpal(gap), fillOpacity = 0.7, 
                color = "#bdbdbd", weight = 1, popup = popup, layerId = scotcouncil$CODE) %>%
      addLegend(pal = qpal, values = ~gap, opacity = 0.7,
                position = 'bottomleft', 
                title = paste0("The Pay Gap"))
  })
  
  observe({
    input$reset_button
    leafletProxy("map") %>% setView(lat = lat, lng = lng, zoom = zoom)
  })
  
  observe({
    click<-input$map_shape_click
    if(is.null(click))
      return()
   
    available <- pgdata2[ which(pgdata2$areacode == click$id), ]
    
    text2 <- paste0("Constituency: ", available[1,1], " (", available[1,2],")")
    
    #text2<-paste("You've selected shape: ", click$id)
    output$const_name<-renderText({
      text2
    })
    
    pgdist2Const <- pgdist2[ which(pgdist2$areacode == available[1,2]),]
    output$plot1 <- renderPlot({
      ggplot() + geom_bar(data = pgdist2Const, aes(x=statname,y=All), stat="identity") + geom_point(data = pgdist2Nat, aes(x=statname,y=All), stat="identity")
    })
    
    
  })
  
  
  
})
