
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

#This is the megaquery that pulls a range of indicators from Scottish Datastore
scotstatquery <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT ?areaname ?indicatorlabel ?yearname ?value ?areacode
WHERE {
{?indicator qb:dataSet data:job-seekers-allowance;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/quarter/2012-Q4> ;
<http://statistics.gov.scot/def/dimension/age> <http://statistics.gov.scot/def/concept/age/16-64> ;
<http://statistics.gov.scot/def/dimension/gender> <http://statistics.gov.scot/def/concept/gender/all> .
data:job-seekers-allowance rdfs:label ?indicatorlabel}
UNION
{?indicator qb:dataSet data:dwellings-hectare;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/year/2012> .
data:dwellings-hectare rdfs:label ?indicatorlabel}
UNION
{?indicator qb:dataSet data:smoking-at-booking;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/gregorian-interval/2013-01-01T00:00:00/P2Y> ;
<http://statistics.gov.scot/def/dimension/populationGroup> <http://statistics.gov.scot/def/concept/population-group/current-smoker> .
BIND ('Antenatal smoking' as ?indicatorlabel) .}
UNION
{?indicator qb:dataSet data:low-birthweight;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/gregorian-interval/2013-01-01T00:00:00/P2Y> ;
<http://statistics.gov.scot/def/dimension/birthWeight> <http://statistics.gov.scot/def/concept/birth-weight/low-weight-births> .
data:low-birthweight rdfs:label ?indicatorlabel}
UNION
{?indicator qb:dataSet data:fire;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/year/2012> ;
<http://statistics.gov.scot/def/dimension/indicator(fire)> <http://statistics.gov.scot/def/concept/indicator-fire/deliberate-fires-excluding-chimney-fires-per-100-000-population> .
BIND ('Deliberate fires' as ?indicatorlabel) .}
UNION
{?indicator qb:dataSet data:breastfeeding;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/year/2013> ;
<http://statistics.gov.scot/def/dimension/breastfeedingDataCollectionTime> <http://statistics.gov.scot/def/concept/breastfeeding-data-collection-time/first-visit> ;
<http://statistics.gov.scot/def/dimension/populationGroup> <http://statistics.gov.scot/def/concept/population-group/breastfed> .
data:breastfeeding rdfs:label ?indicatorlabel}
UNION
{?indicator qb:dataSet data:fuel-poverty-shcs;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/gregorian-interval/2010-01-01T00:00:00/P2Y> ;
<http://statistics.gov.scot/def/dimension/age> <http://statistics.gov.scot/def/concept/age/all> ;
<http://statistics.gov.scot/def/dimension/gender> <http://statistics.gov.scot/def/concept/gender/all> ;
<http://statistics.gov.scot/def/dimension/disabilityStatus> <http://statistics.gov.scot/def/concept/disability-status/all> .
BIND ('Perc of households in fuel poverty' as ?indicatorlabel) .}
UNION
{?indicator qb:dataSet data:alcohol-related-discharge ;
mp:ratio ?value ;
sdmxd:refPeriod <http://reference.data.gov.uk/id/government-year/2012-2013> .
data:alcohol-related-discharge rdfs:label ?indicatorlabel} .
?indicator sdmxd:refArea ?area ;
sdmxd:refPeriod ?year .
?year rdfs:label ?yearname .
?area stat:code <http://statistics.gov.scot/id/statistical-entity/S12> ;
rdfs:label ?areaname ;
skos:notation ?areacode
}"

# SPARQL is too big to run succesfully
#qd <- SPARQL(endpoint,query)

# Load the constituency geography file
scotcouncil <- readOGR("ScottishCouncilAreas2_simplified.geojson", "OGRGeoJSON")

# Load the csv file containing the pay data
pgdata <- read.csv2("ons_ashe_scot_paygap.csv",header = TRUE, sep=",")

#Load the csv file containing the scottish government data
sgdata <- read.csv2("scot_stat_data.csv",header = TRUE, sep=",")

#Turn the value column into from scientific notation to number
pgdata <- transform(pgdata, value = as.numeric(value))
sgdata <- transform(sgdata, value = as.numeric(value))

#Turn into dataframes
pgdata <- as.data.frame(pgdata)
sgdata <- as.data.frame(sgdata)

#Pick out only the median statistic from the paygap data (ie discarding distribution)
pgdata2 <- pgdata[ which(pgdata$statname == "Median"), ]

#Pivot the data row headers to the left of the tilde, column headers to the right, then value
pgdata2 <- dcast(pgdata2,areaname + areacode ~ sexname, value.var = 'value')

sgdata2 <- dcast(sgdata,areaname + areacode ~ indicatorlabel, value.var = 'value')

#Add a calculated field for the paygap between males and females
pgdata2$gap <- with(pgdata2,Male - Female)

#pgdist <- pgdata[ which(pgdata$year == "2016"), ]

#Pivot the distribution of pay across the deciles. Think this will not be needed for this particular report
pgdist2 <- dcast(pgdata,areaname + areacode + statname ~ sexname, value.var = 'value')

#Again, add the calculated field to this dataframe
pgdist2$gap <- with(pgdist2, Male - Female)

#Pluck the national figure out of the data
pgdist2Nat <- pgdist2[ which(pgdist2$areacode == "K03000001"),]

#Merge the two dataframes into one, horizontally
combdata <- merge(pgdata2,sgdata2,by="areacode")

server <- (function(input, output, session) {
  
  #Filter the data according to the values entered into the filter text boxes
  #***Need to change this so that instead of it always filtering gap, it filters using the field selected in the filter dropdown
  selected <- reactive({
    subset(combdata,
           gap < input$upper & gap >= input$lower)
  })
  
  # Put the default map co-ordinates and zoom level into variables
  lat <- 57.542788
  lng <- 0.144708
  zoom <- 6
  
  # Draw the map
  output$map <- renderLeaflet({
    
    leaflet() %>% 
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
    #***Needs to change based on what's selected in the map dropdown
    qpal <- colorNumeric("Spectral", pgdata2$gap, na.color = "#bdbdbd")
    
    #the popup on the map
    #***Need to make this dynamic based on what's selected in the map dropdown
    popup <- paste0("<h5>",scotcouncil$NAME,"</h5><br /><h3>£",scotcouncil$gap,"</h3>")
    
    #draw the map with stuff on
    #***Need to make this dynamic based on what's selected in the map dropdown
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
   
    available <- combdata[ which(combdata$areacode == click$id), ]
    
    text2 <- paste0("Council area: ", available[1,1], " (", available[1,2],")")
    
    output$const_name<-renderText({
      text2
    })
    
    #Probably get rid of this for this demo and use scatterplot / plain barplot
    #pgdist2Const <- pgdist2[ which(pgdist2$areacode == available[1,2]),]
    #output$plot1 <- renderPlot({
    #  ggplot() + geom_bar(data = pgdist2Const, aes(x=statname,y=All), stat="identity") + geom_point(data = pgdist2Nat, aes(x=statname,y=All), stat="identity")
    #})
    #scatterplot
    output$plot1 <- renderPlot({
      ggplot(combdata, aes(x=All, y=Breastfeeding,label=areaname.x)) + geom_point(shape=21, size=6, color="blue",fill="red", alpha=0.3) + stat_smooth(method = "lm", col = "red") + ggtitle('Test Scatterplot') + labs(x='Median Pay', y='Breastfeeding Rates') + theme_bw() 
    })
    
    
    
  })
  
  
  
})
