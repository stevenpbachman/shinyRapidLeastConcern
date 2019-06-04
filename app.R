#
# This is a Shiny web application. You can run the application by clicking # the 'Run App' button above.
#
### Rapid Least Concern
### A tool to download occurrence data from GBIF, clean for georef errors, filter on native range using POWO and download in IUCN Red List accepted format
### Steven Bachman - Royal Botanic Gardens, Kew

### this code is organised by:

# 1 - libraries
# 2 - shapefiles
# 3 - functions
# 4 - UI
# 5 - Server

### to do later
# add selective rows from datatable: https://yihui.shinyapps.io/DT-rows/ to pick correct key and IPNI ID
# https://stackoverflow.com/questions/28274584/get-selected-row-from-datatable-in-shiny-app
# add other issues for cleaning - get from gbif table - use GBIF website format
# useful code here: C:\R_local\LATEST\RedLeastApply


#### 1 - libraries
library(magrittr)
library(readr)
library(rgdal)
library(DT)
library(leaflet)
library(rgbif)
library(sp)
library(jsonlite)
library(tidyverse)
library(raster)
library(httr)
library(zip)
library(shinythemes)
library(wicket)
library(sf)
library(stringr)
library(shiny)
library(plyr)
library(rCAT)

#remove.packages(sf, mylib)
#mylib = .libPaths()

#### 2 - shapefiles/rasters/other files---------------
TDWGpolys = sf::read_sf("data/level3/level3.shp")
##TDWGpolys = rgdal::readOGR("data/level3/level3.shp")
raster.tdwg = raster::raster("data/rasters/tdwg3.tiff")
tdwg_raster <- read.csv("data/tdwg_raster.csv")
plantgflist <- read.csv("data/plantgrowthformslookup.csv", encoding="UTF-16")
habitatlist <- read.csv("data/habitatslookup.csv", encoding="UTF-16")
taxonomy_iucn <- read.csv("data/taxonomy_iucn_lookup.csv", encoding="UTF-16")
TDWG_to_IUCN_version3_UTF <- read.delim("data/TDWG_to_IUCN.txt", encoding="UTF-16", na.strings="")

source("Rapid_LC_functions.R")


#### 4 - UI---------------
ui <- fluidPage(
  
  # set themes
  theme = shinythemes::shinytheme("simplex"),
  
  # Sidebar with a slider input for number of bins 
  navbarPage("Rapid Least Concern", id = "navLC",
             tabPanel("1 Search",
                      sidebarLayout(position = "left",
                                    sidebarPanel(
                                      textInput("speciesinput",
                                                "1 Enter species e.g. Aloe zebrina",
                                                placeholder = "Aloe zebrina"),

                                      br(),
                                      
                                      textInput("key",
                                                "2 Enter 'usageKey' from GBIF search results to map points:"
                                      ),
                                      
                                      br(),
                                      textInput("powo",
                                                "3 Enter IPNI_ID from POWO search results to get native range:"
                                      ),
                                      
                                      br(),
                                      
                                      textInput("name",
                                                "4 Enter name of assessor/compiler:",
                                                placeholder = "John Smith"),
                                      br(),
                                      
                                      textInput("email",
                                                "5 Enter email of assessor/compiler:",
                                                placeholder = "j.smith@email.com"),
                                      
                                      br(),
                                      
                                      textInput("affiliation",
                                                "6 Affiliation of assessor/compiler:",
                                                placeholder = "my institution"),
                                      
                                      br(),
                                      actionButton("getPoints", "Go to section 2 >>")
                                      #br(),
                                      #br(),
                                      #helpText("Now go to tab '2 CLEAN' to clean the points")
                                      
                                    ),
                                    
                                    # Show the input species
                                    mainPanel(
                                      
                                      # Output: Header + summary of distribution ----
                                      h6("GBIF search results:"),
                                      # search results from GBIF
                                      DT::dataTableOutput("summarytab"),
       
                                      br(),
                                      
                                      h6("Plant of the World Online search results:"),
                                      # search results from POWO
                                      DT::dataTableOutput("powotab"),
                                      
                                      #br(),
                                      
                                      #h6("Distribution map: "),
                                      
                                      #leaflet::leafletOutput("mymap", width = "100%", height = 400),
                                      
                                      #br(),
                                      br()
                                      
                                    )
                      )
             ),
             tabPanel("2 Clean",
                      sidebarLayout(position = "left",
                                    sidebarPanel(
                                      
                                      helpText("Select options below to clean the points "),
                                      
                                      #maybe checkbox group is better here - look up names
                                      
                                      br(),
                                      
                                      checkboxInput("native", "Remove non-native points", FALSE),

                                      br(),
                                      
                                      actionButton("cleanPoints", "Clean"),
                                      br(),
                                      br(),
                                      actionButton("goto3", "Go to section 3 >>")
                                      #helpText("Now go to tab '3 DOWNLOAD' to save the point file and SIS CSV files")
                                      
                                    ),
                                    
                                    # Show the input species
                                    mainPanel(
                                      
                                      h6("Raw Distribution map: "),
                                      leaflet::leafletOutput("mymap", width = "100%", height = 400),
                                      
                                      h6("Clean Distribution map: "),
                                      leaflet::leafletOutput("cleaningmap", width = "100%", height = 400)
                                      
                                    )
                      )
                      
             ),
             
             tabPanel("3 Download",
                      sidebarLayout(position = "left",
                                    sidebarPanel(helpText("Enter additional data for Habitat and Plant growth form:"),
                                                 
                                                 selectInput("gfinput",
                                                             label = ("Select growth form(s)"), 
                                                             choices = plantgflist[,2],
                                                             selected = plantgflist[1,2],
                                                             selectize = TRUE,
                                                             multiple = TRUE),
                                                 
                                                 # multi select?
                                                 selectInput("habinput",
                                                             label = ("Select habitat(s)"), 
                                                             #choices = list("Tree - size unknown" = 1, "Tree - large" = 2, "Tree - small" = 3),
                                                             choices = habitatlist[,2],
                                                             selected = habitatlist[126,2],
                                                             selectize = TRUE,
                                                             multiple = TRUE),
                                                 
                                                 br(),
                                                 
                                                 helpText("Download spatial point file:"),
                                                 
                                                 downloadButton('download', "Download clean point file"),
                                                 
                                                 br(),
                                                 br(),
                                                 
                                                 helpText("Download SIS Connect csv files:"),
                                                 
                                                 downloadButton('downloadSIS', "Download SIS Connect Files")
                                                 
                                                 
                                    ),
                                    
                                    # Show the input species
                                    mainPanel(
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Point table", DT::dataTableOutput("pointstab")),
                                                  tabPanel("Allfields", DT::dataTableOutput("outallf")),
                                                  tabPanel("Assessments", DT::dataTableOutput("outassessments")),
                                                  tabPanel("Countries", DT::dataTableOutput("outocc")),
                                                  tabPanel("Credits", DT::dataTableOutput("outcredits")),
                                                  tabPanel("Habitats", DT::dataTableOutput("outhab")),
                                                  tabPanel("Plant specific", DT::dataTableOutput("outgfinput"))
                                                  #tabPanel("Taxonomy", DT::dataTableOutput("outtax"))
                                                  
                                                  
                                                  
                                      )
                                    )
                      )
             ),
             
             tabPanel("4 Batch",

                      sidebarPanel(
                        fileInput("file1", "Upload a list of names from a CSV file",
                                multiple = FALSE,
                                accept = (".csv")
                                ),
                        downloadButton('getcleantab', "Download table"),
                        helpText("Check for any problematic names and if necessary reload a table with a clean list of names"),
                        
                        br(),

                        helpText("Click 'Get statistics' for range metrics such as EOO and AOO"),
                        
                        actionButton('getStats', "Get statistics"),
                        br(),
                        br(),
                        helpText("Adjust thresholds to determine Least Concern"),
                        
                        # Input: Threat reminder
                         
                        checkboxInput("threatvalue", label = "No observed, estimated, projected, inferred, or suspected declines likely 
                                       to trigger criteria A, B, C, D or E." , value = TRUE),
                        
                        
                        # Input: EOO threshold ----
                        sliderInput("eoo", "Extent of Occurrence (EOO):",
                                    min = 1, max = 100000,
                                    value = 30000),
                        
                        # Input: AOO threshold ----
                        sliderInput("aoo", "Area of occupancy (AOO):",
                                    min = 1, max = 10000,
                                    value = 100),
                        
                        # Input: Number of records threshold ----
                        sliderInput("records", "Number of records:",
                                    min = 1, max = 1000,
                                    value = 75),
                        
                        # Input: Number of TDWG regions ----
                        sliderInput("tdwg", "Number of Level 3 TDWG regions:",
                                    min = 1, max = 100,
                                    value = 5),
                        
                        helpText("Click to download SIS Connect and point files:"),
                        
                        downloadButton('downloadbatch', "Download SIS Connect Files")
                        
                        ),
                        
                        mainPanel(
                          
                          # Output: Data file ----
                          DT::dataTableOutput("contents"),
                          br(),
                          # Output: Data file ----
                          DT::dataTableOutput("stats"),
                          verbatimTextOutput("threatvalue")
                          
                        )
                      
             ),
                        
    
             tabPanel("Help"#,
                      #includeHTML("README.html")
             )
  )
)


### 5 - Server---------------
server <- function(input, output, session) {
  
  ############  INPUTS ############
  
  ### 1. Search inputs
  
  mapInput <- eventReactive(input$getPoints, {
    withProgress(message = 'Querying GBIF',
                 value = 2, {
                   points = gbif.points(input$key)
                 })
    points
  })
  
  cleanmapInput <- eventReactive(input$getPoints, {
    withProgress(message = 'Querying GBIF',
                 value = 2, {
                   points = gbif.points(input$key)
                 })
    points$COMPILER = paste0(input$name)
    ##, ", ", substr(input$firstname, 1, 1),".", input$initials, sep = "")
    points$CITATION = paste0(input$affiliation, sep = "")
    points = within(points, rm("issues", "datasetKey", "recordNumber", "recordedBy"))
    points
    
    if (input$native == TRUE) {
      powo = input$powo
      points = native.clip(points, TDWGpolys, powo)
      
    } else {
      points
    }
    
  })
  
  ### 1. Search outputs
  
  #Show results of GBIF search as a table
  output$summarytab <- DT::renderDataTable({
    req(input$speciesinput)
    sp_key = gbif.key(input$speciesinput)
  },
  options = list(pageLength = 5)#, #formatStyle(
  #columns = 'usageKey',
  #backgroundColor = styleEqual('red','red','red')
  )
  
  # Show results of powo search as a table
  output$powotab <- DT::renderDataTable({
    req(input$speciesinput)
    powosp = check.accepted.POWO(input$speciesinput)
    
  }, 
  options = list(pageLength = 5))
  
  # link to navpanel 2 Clena
  observeEvent(input$goto3, {
    updateTabsetPanel(session, "navLC",
                      selected = "3 Download"
    )
  })
  
  ### 2. clean inputs
  
  cleaningmapInput <- eventReactive(input$cleanPoints, {
    withProgress(message = 'Querying GBIF',
                 value = 2, {
                   points = gbif.points(input$key)
                 })
    points$COMPILER = paste0(input$name)
    ##, ", ", substr(input$firstname, 1, 1),".", input$initials, sep = "")
    points$CITATION = paste0(input$affiliation, sep = "")
    points = within(points, rm("issues", "datasetKey", "recordNumber", "recordedBy", "LEVEL3_NAM", "LEVEL3_COD",
                               "VALUE",	"ID",	"LEVEL2_COD",	"LEVEL1_COD",	"establishment",	"featureId",	"tdwgLevel",	"POWO_ID"))
    points$BINOMIAL = input$speciesinput
    points
    
    if (input$native == TRUE) {
      #points = subset(points, points$EVENT_YEAR < "1950")
      powo = input$powo
      points = native.clip(points, TDWGpolys, powo)
      points = within(points, rm("issues", "datasetKey", "recordNumber", "recordedBy", "LEVEL3_NAM", "LEVEL3_COD",
                                 "VALUE",	"ID",	"LEVEL2_COD",	"LEVEL1_COD",	"establishment",	"featureId",	"tdwgLevel",	"POWO_ID"))
    } else {
      points$BINOMIAL = input$speciesinput
      points
    }
    
  })
  
  #######################################################
  
  ### 2. clean outputs
  
  # output for the map on Cleanpage
  output$mymap <- renderLeaflet({
    df <- mapInput()
    sptdwg = tdwg.dist = check.tdwg(input$powo)
    sptdwg = merge(TDWGpolys, tdwg.dist)
    
    leaflet(data = df) %>%
      addMapPane("points", zIndex = 420) %>%
      addMapPane("poly", zIndex = 410) %>%
      addCircleMarkers(lng = ~DEC_LONG,
                       lat = ~DEC_LAT, radius = 4, color = "green", popup = paste("Collector:", df$recordedBy, "<br>",
                                                                                  "Number:", df$recordNumber, "<br>",
                                                                                  "Year:", df$EVENT_YEAR, "<br>",
                                                                                  "Catalogue No.:", df$CATALOG_NO),
                       options = pathOptions(pane = "points")) %>%
      # maybe add an IF here to control whether native range is mapped
      addPolygons(data=sptdwg, color = "red", weight = 1, fillColor = "red", fillOpacity = 0.2, options = pathOptions(pane = "poly")) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE))
    
  })
  
  # output for the clean map on clean page
  output$cleaningmap <- renderLeaflet({
    df <- cleaningmapInput()
    sptdwg = tdwg.dist = check.tdwg(input$powo)
    sptdwg = merge(TDWGpolys, tdwg.dist)
    
    leaflet(data = df) %>%
      addMapPane("points", zIndex = 420) %>%
      addMapPane("poly", zIndex = 410) %>%
      addCircleMarkers(lng = ~DEC_LONG,
                       lat = ~DEC_LAT, radius = 4, color = "green", popup = paste("Collector:", df$recordedBy, "<br>",
                                                                                  "Number:", df$recordNumber, "<br>",
                                                                                  "Year:", df$EVENT_YEAR, "<br>",
                                                                                  "Catalogue No.:", df$CATALOG_NO),
                       options = pathOptions(pane = "points")) %>%
      # maybe add an IF here to control whether native range is mapped
      addPolygons(data=sptdwg, color = "red", weight = 1, fillColor = "red", fillOpacity = 0.2, options = pathOptions(pane = "poly")) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE))
    
  })
  
  # link to navpanel 3 Download
  observeEvent(input$getPoints, {
    updateTabsetPanel(session, "navLC",
                      selected = "2 Clean"
    )
  })
  
  
  
  ### 3. Download inputs
  
  taxinput = eventReactive(input$key, {
    tax = taxonomy(input$key, input$speciesinput, input$powo)
    #credits$internal_taxon_id = input$datasetInput[1,1]
    tax
  })
  
  
  #######################################################
  
  ### 3. Download outputs
  
  # Show GBIF occurrence points
  output$pointstab <- DT::renderDataTable({
    req(input$speciesinput)
    df = cleaningmapInput()
    df
  }, 
  options = list(pageLength = 5))
  
  # download the cleaned gbif point file
  output$download = downloadHandler(
    filename = function(){
      paste(input$speciesinput, "_", Sys.Date(), ".csv", sep = "" ) # change this to species name
    },
    content = function(file){
      write.csv(cleaningmapInput(), file, row.names = FALSE)
      
    }
  )
  
  # Show csv files as data tables before download
  output$outallf <- DT::renderDataTable({
    allfields = allfields(input$speciesinput, input$powo)
    #credits$internal_taxon_id = input$datasetInput[1,1]
    allfields
  })
  
  output$outassessments <- DT::renderDataTable({
    assessments = assessments(input$speciesinput, input$powo)
    #credits$internal_taxon_id = input$datasetInput[1,1]
    assessments
  })
  
  output$outocc <- DT::renderDataTable({
    occ = countries(input$powo)
    occ
  })
  
  output$outcredits <- DT::renderDataTable({
    credits = credits(input$name, input$email, input$affiliation, input$speciesinput, input$powo)
    #credits$internal_taxon_id = input$datasetInput[1,1]
    credits
  })
  
  output$outhab <- DT::renderDataTable({
    req(input$habinput)
    hab = habitats(input$habinput, input$speciesinput, input$powo)
    #credits$internal_taxon_id = input$datasetInput[1,1]
    hab
  })
  
  output$outgfinput <- DT::renderDataTable({
    req(input$gfinput)
    plantspecific = plantspecific(input$gfinput, input$speciesinput, input$powo)
    #credits$internal_taxon_id = input$datasetInput[1,1]
    plantspecific
  })
  
  #output$outtax <- DT::renderDataTable({
  #  taxtable = taxinput()
  #  #taxtable = taxonomy(input$key, input$speciesinput, input$powo)
  #  taxtable
  #})
  
  # download the SIS connect files
  output$downloadSIS = downloadHandler(
    filename = function(){
       paste("SIS_connect.zip") # change this to species name
    },
    content = function(file){
      
      saveSIS = all_SIS(species = input$speciesinput,
                        powo =  input$powo,
                        name = input$name,
                        email = input$name,
                        affiliation = input$affiliation,
                        habitat = input$habinput,
                        growthform = input$gfinput,
                        key = input$key)

      zipdir = paste0(getwd(), "data/singlezip/")
      #thefiles = list.files(zipdir, full.names = TRUE)
      #thefilesnames = paste0(zipdir, thefiles)     
      thefiles = c("data/singlezip/allfields.csv",
                   "data/singlezip/assessments.csv",
                   "data/singlezip/countries.csv",
                   "data/singlezip/credits.csv",
                   "data/singlezip/habitats.csv",
                   "data/singlezip/plantspecific.csv")
                   #"singlezip/taxonomy.csv")
      
      #files2zip <- dir(zipdir, full.names = TRUE)
      zip::zipr('singlezip.zip', thefiles)


      #use copy to force the download
      file.copy("singlezip.zip", file)

    },
    contentType = "application/zip"
   )
  
  #######################################################
  
  ### 4. Batch inputs
  batchInput <- eventReactive(input$file1, {
    
    df <- read.csv(input$file1$datapath)
    # 3.14
    # check the names against POWO first
    withProgress(message = 'Getting there...',
                 value = 2, {
                   applytest = lapply(df$name_in,batch.POWO)
                 })
    
    applytest_df = do.call(rbind, applytest)
    #applytest_df = cbind(applytest_df,df)
    applytest_df
    
  })
  
  # Reactive expression to get values from sliders ----
  # output 

  output$threatvalue<- renderPrint({ 
    if ((input$threatvalue) == TRUE) {
      invisible(input$threatvalue)
    } else {
      print(paste0("WARNING - please consider possible threats (past, present, future) that could cause declines and trigger criteria A, B, C, D, or E."))
    }
    
    })
  eooValue <- reactive({
    input$eoo
  })
  aooValue <- reactive({
    input$aoo
  })
  recordsValue <- reactive({
    input$records
  })
  tdwgValue <- reactive({
    input$tdwg
  })
  
  statsInput <- eventReactive(input$getStats, {
    species = batchInput()
    #single = LC_comb(species)
    withProgress(message = 'Getting there...',
                 value = 2, {
                   multi = adply(species, 1, LC_comb)
                 })
    df = multi
    df
  })
  

  
  ### 4. Batch outputs
  output$contents <- DT::renderDataTable({
    df = batchInput()
    df
  }, 
  options = list(pageLength = 5))
  
  output$getcleantab = downloadHandler(
    # download the checked names table
    filename = function(){
      paste("checked_names_", Sys.Date(), ".csv", sep = "" ) # change this to species name
    },
    content = function(file){
      write.csv(batchInput(), file, row.names = FALSE)
      
    }
  )
  
  output$stats <- DT::renderDataTable({
    dt = statsInput()
    dt = subset(dt, EOO >= eooValue())
    dt = subset(dt, AOO >= aooValue())
    dt = subset(dt, RecordCount >= recordsValue())
    dt = subset(dt, TDWGCount >= tdwgValue())
    }, 
    options = list(pageLength = 5))
  
  ####################
  
  output$downloadbatch = downloadHandler(
    
     # download the results
    filename = function(){
      paste("batch_SIS_connect_", Sys.Date(), ".zip", sep = "" ) # change this to species name
    },
    content = function(file){

      dt = statsInput()
      
      # first get the full results out - this will include any error species - with explanation
      dtpath = paste0(getwd(), "/data/batchzip/results.csv")
      dttable = dt
      write.csv(dttable, dtpath, row.names = FALSE)
      
      # now take out the species that are errors so we have clean set for next bit
      drop_cols = "Warning"
      dt  = dt [ , !(names(dt ) %in% drop_cols)]
 
      # now you have to add the filters again, otherwise you get the full table
      dt = subset(dt, EOO >= eooValue())
      dt = subset(dt, AOO >= aooValue())
      dt = subset(dt, RecordCount >= recordsValue())
      dt = subset(dt, TDWGCount >= tdwgValue())
      
      # get the points
      withProgress(message = 'Getting there...',
                   value = 2, {
                     multipoints = adply(dt, 1, all_batch_points) # run through each species
                   })
      
      
      multipoints = adply(dt, 1, all_batch_points) # run through each species
      multipoints = multipoints[ -c(1:10)] #drop first 11 columns
      pointspath = paste0(getwd(), "/data/batchzip/points.csv") # save path
      write.csv(multipoints, pointspath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # now the csv files
      # allfields
      batch_allfields = adply(dt, 1, batch_allfields)
      batch_allfields = batch_allfields[ -c(1:10)]
      batch_allfieldspath = paste0(getwd(), "/data/batchzip/allfields.csv")
      write.csv(batch_allfields, batch_allfieldspath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # assessments
      batch_assessments = adply(dt, 1, batch_assessments)
      batch_assessments = batch_assessments[ -c(1:10)]
      batch_assessmentspath = paste0(getwd(), "/data/batchzip/assessments.csv")
      write.csv(batch_assessments, batch_assessmentspath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # credits
      batch_credits = adply(dt, 1, batch_credits)
      batch_credits = batch_credits[ -c(1:10)]
      batch_creditspath = paste0(getwd(), "/data/batchzip/credits.csv")
      write.csv(batch_credits, batch_creditspath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # habitats
      batch_habitats = adply(dt, 1, batch_habitats)
      batch_habitats = batch_habitats[ -c(1:10)]
      batch_habitatspath = paste0(getwd(), "/data/batchzip/habitats.csv")
      write.csv(batch_habitats, batch_habitatspath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # plantspecific
      batch_plantspecific = adply(dt, 1, batch_plantspecific)
      batch_plantspecific = batch_plantspecific[ -c(1:10)]
      batch_plantspecificpath = paste0(getwd(), "/data/batchzip/plantspecific.csv")
      write.csv(batch_plantspecific, batch_plantspecificpath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # plantspecific
      batch_taxonomy = adply(dt, 1, batch_taxonomy)
      batch_taxonomy = batch_taxonomy[ -c(1:10)]
      batch_taxonomypath = paste0(getwd(), "/data/batchzip/taxonomy.csv")
      write.csv(batch_taxonomy, batch_taxonomypath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # countries
      batch_countries = adply(dt, 1, batch_countries)
      batch_countries = batch_countries[ -c(1:10)]
      batch_countriespath = paste0(getwd(), "/data/batchzip/countries.csv")
      write.csv(batch_countries, batch_countriespath, row.names = FALSE) # write it - row.names = FALSE! 
    
      
      #write.csv(dt, file, row.names = FALSE)
      batchzipdir = paste0(getwd(), "/data/batchzip/")
      
      thefiles = c("data/batchzip/allfields.csv",
                   "data/batchzip/assessments.csv",
                   "data/batchzip/countries.csv",
                   "data/batchzip/credits.csv",
                   "data/batchzip/habitats.csv",
                   "data/batchzip/plantspecific.csv",
                   "data/batchzip/taxonomy.csv",
                   "data/batchzip/results.csv",
                   "data/batchzip/points.csv")

        # get all files in the directory
      
      zip::zipr('data/batchzip.zip', thefiles)
        
      # use copy to force the download
      file.copy("data/batchzip.zip", file)
      
      
    },
    contentType = "application/zip"
    

  )
  
    
  
  
}  


# Run the application 
shinyApp(ui = ui, server = server)

#rm(list=ls())

