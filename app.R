#
# This is a Shiny web application. You can run the application by clicking # the 'Run App' button above.
#
### Rapid Least Concern
### A tool to download occurrence data from GBIF, clean for georef errors, filter on native range using POWO and download in IUCN Red List accepted format
### Steven Bachman - Royal Botanic Gardens, Kew

### this code is organised by:

# 1 - libraries
# 2 - functions
# 3 - UI
# 4 - Server

### to do later
# add selective rows from datatable: https://yihui.shinyapps.io/DT-rows/ to pick correct key and IPNI ID
# https://stackoverflow.com/questions/28274584/get-selected-row-from-datatable-in-shiny-app
# add other issues for cleaning - get from gbif table - use GBIF website format
# useful code here: C:\R_local\LATEST\RedLeastApply


#### 1 - libraries
library(raster)
library(here)
library(magrittr)
library(rgdal)
library(DT)
library(leaflet)
library(rgbif)
library(jsonlite)
library(tidyverse)
library(httr)
library(zip)
library(shinythemes)
library(wicket)
library(sf)
library(shiny)
library(rCAT)
library(flexdashboard)
library(shinydashboard)


#### 2 - Source the functions---------------
source("Rapid_LC_functions.R")

#### 3 - UI---------------
ui <- fluidPage(
  
  # set themes
  theme = shinythemes::shinytheme("simplex"),
  
  # Navigation
  navbarPage("Rapid Least Concern", id = "navLC",
              tabPanel("Home",
                wellPanel(
                  tags$h1("Welcome to Rapid Least Concern"),
                  
                  br(),
                  br(),
                  
                  tags$blockquote("Rapid Least Concern combines plant data from GBIF and Plants of
                                  the World Online to generate a Red List compliant Least Concern assessment."),
                  br(),
             
                  actionButton("gotosingle", "Single assessment >>"),
                  tags$h5("For generating Least Concern assessments one at a time."),
                  
                  br(),
                  br(),
                  
                  actionButton("gotobatch", "Batch assessment >>"),
                  tags$h5("For generating multiple Least Concern assessments based on a user-defined species list."),
                  
                  br(),
                  br(),

                  br(),
                  br(),
                  br(),
                  HTML('<iframe width="400" height="200" src="https://www.youtube.com/embed/lJIrF4YjHfQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')                 
                  )
                ),
             
              tabPanel("1 Single",
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
                          
                                      checkboxInput("native", "Remove non-native points", FALSE),
                                      
                                      actionButton("getPoints", "Map >>"),
                                      
                                      actionButton('getSingleStats', "Get statistics >>"),
                                      
                                      br(),
                                      br(),
                                      
                                      # extra data that can't be generated automatically - ask user to input or select
                                      actionButton("addData", "4. Enter Additional Data ▼▲", style='padding:4px; font-size:80%'),
                                      
                                      br(),
                                      
                                      conditionalPanel(
                                        condition = "input.addData % 2 == 0",selectInput(
                                          "gfinput",
                                          label = ("Select growth form(s)"),
                                          choices = plantgflist[, 2],
                                          selected = plantgflist[1, 2],
                                          selectize = TRUE,
                                          multiple = TRUE
                                        )
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.addData % 2 == 0",selectInput(
                                          "habinput",
                                          label = ("Select habitat(s)"),
                                          #choices = list("Tree - size unknown" = 1, "Tree - large" = 2, "Tree - small" = 3),
                                          choices = habitatlist[, 2],
                                          selected = habitatlist[126, 2],
                                          selectize = TRUE,
                                          multiple = TRUE
                                        )
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.addData % 2 == 0",textInput("name",
                                                                                       "Enter name of assessor/compiler:",
                                                                                       placeholder = "John Smith")
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.addData % 2 == 0",textInput("email",
                                                                                      "Enter email of assessor/compiler:",
                                                                                      placeholder = "j.smith@email.com")
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.addData % 2 == 0",textInput("affiliation",
                                                                                       "Affiliation of assessor/compiler:",
                                                                                       placeholder = "my institution")
                                      ),
                                      #helpText("4. Enter additional data for Habitat and Plant growth form:"),
                                      #tags$h5("4. Enter additional data"),
                                      
  
                                      br(),
                                      
                                      #helpText("Download spatial point file:"),
                                      
                                      tags$blockquote("
                                      Check the map and statistics. As a guideline, gauges in green indicate high probability of being Least Concern, red indicates possibly threatened. If you think you have an LC specie, download the point file and SIS connect files"),
                                      
                                      tags$h5("5. Download data:"),
                                      
                                      downloadButton('download', "Download clean point file"),
                                      
                                      br(),
                                 
                                      helpText("Download SIS Connect csv files:"),
                                      
                                      downloadButton('downloadSIS', "Download SIS Connect Files")
                                                   
                                      
                                    ),
                                    
                                    # Show the results of the name search against GBIF and POWO
                                    mainPanel(
                                      
                                      # Output: Header + summary of distribution ----
                                      actionButton("minmaxgbif", "Hide/Show search results ▼▲", style='padding:4px; font-size:80%'),
                                      
                                      br(),
                                      
                                      
                                      # search results from GBIF
                                      h5("GBIF search results:"),
                                      conditionalPanel(
                                        condition = "input.minmaxgbif % 2 == 0",DT::dataTableOutput("summarytab")
                                      ),
                                      
                                      h5("Plant of the World Online search results:"),
                                      conditionalPanel(
                                        condition = "input.minmaxgbif % 2 == 0",DT::dataTableOutput("powotab")
                                      ),
                                   
                                      
                                      br(),
                                      
                                      actionButton("minmaxmap", "Hide/Show map ▼▲", style='padding:4px; font-size:80%'),
                                      
                                      h5("Distribution map: "),
                                      
                                      conditionalPanel(
                                        condition = "input.minmaxmap % 2 == 0",leaflet::leafletOutput("mymap", width = "100%", height = 400)
                                      ),
                                      
                                      br(),
                                      
                                      actionButton("minmaxstats", "Hide/Show statistics ▼▲", style='padding:4px; font-size:80%'),
                                      h5("Statistics: "),
                                      conditionalPanel(
                                        condition = "input.minmaxstats % 2 == 0",DT::dataTableOutput("singletab"),
                                        
                                        tags$blockquote("
                                      Gauges in green indicate high probability of being Least Concern, red indicates possibly threatened."),
                                        
                                        dashboardBody(
                                          fluidRow(
                                            
                                            
                                            box(flexdashboard::gaugeOutput("plt1"),width=3,background ="green" ),
                                            
                                            box(flexdashboard::gaugeOutput("plt2"),width=3,background ="green" ),
                                            
                                            box(flexdashboard::gaugeOutput("plt3"),width=3,background ="green" ),
                                            
                                            box(flexdashboard::gaugeOutput("plt4"),width=3,background ="green" )
                                            
                                          )
                                          
                                        )
                                        
                                        ),
                                    
                             

                                      
                                      actionButton("minmaxSIS", "Hide/Show SIS tables ▼▲", style='padding:4px; font-size:80%'),
                                      
                                      br(),
                                      conditionalPanel(
                                        condition = "input.minmaxSIS % 2 == 0",
                                        
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Point table", DT::dataTableOutput("pointstab")),
                                                    tabPanel("Allfields", DT::dataTableOutput("outallf")),
                                                    tabPanel("Assessments", DT::dataTableOutput("outassessments")),
                                                    tabPanel("Countries", DT::dataTableOutput("outocc")),
                                                    tabPanel("Credits", DT::dataTableOutput("outcredits")),
                                                    tabPanel("Habitats", DT::dataTableOutput("outhab")),
                                                    tabPanel("Plant specific", DT::dataTableOutput("outgfinput")),
                                                    tabPanel("Taxonomy", DT::dataTableOutput("outtax"))
                                                    
                                        )
                                      ),

                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br()
                                      
                                    )
                      )
              
             ),
             

             # batch option - user needs to load in list of species names as binomials under column: 'name_in' 
             tabPanel("2 Batch",

                      sidebarPanel(
                        fileInput("file1", "Upload a list of names from a CSV file. One field must be called 'name_in' and should contain binomials e.g. 'Poa annua'",
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
                        
             tabPanel("3 Batch - user points"
                      #includeHTML("README.html")
             ),
             
             tabPanel("Help"
                      #includeHTML("README.html")
             )
    )
)


### 4 - Server---------------
server <- function(input, output, session) {

############  INPUTS ############
  
  values <- reactiveValues(points=NULL,
                           native_range=NULL)
  
  ### Home page navigation
  
  # link to navpanel 1 single
  observeEvent(input$gotosingle, {
    updateTabsetPanel(session, "navLC",
                      selected = "1 Single"
    )
  })
  
  # link to navpanel 2 batch
  observeEvent(input$gotobatch, {
    updateTabsetPanel(session, "navLC",
                      selected = "2 Batch"
    )
  })
  
  
  ### 1 Single -  prepare map input
  
  observeEvent(input$getPoints, {
    withProgress(message = 'Querying GBIF',
                 value = 2, {
                   gbif_results = gbif.points(input$key)
                 })
    
    if (input$powo != "") {
      powo_results <- check.tdwg(input$powo)
      values$native_range <- powo_results
      gbif_results <- find.native(gbif_results, values$native_range, TDWGpolys)
    }
    
    
    
    values$points <- gbif_results
  })
  
  ### 1 Single - prepare name search results output
  
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
  

#######################################################
  
  ### 1. Single - map render
  # Output map
  output$mymap <- renderLeaflet({
    if (is.null(values$points)) {
      return(NULL)
    }

    df <- values$points
    if (input$native & ! is.null(values$native_range)) {
      df <- filter(df, ! is.na(native_range))
    }
    
    sptdwg = merge(TDWGpolys, values$native_range)
    
    leaflet(data = df) %>%
      addMapPane("points", zIndex = 420) %>%
      addMapPane("poly", zIndex = 410) %>%
      addCircleMarkers(lng = ~DEC_LONG,
                       lat = ~DEC_LAT, 
                       radius = 4, 
                       color = "green", 
                       popup = ~paste("Collector:", recordedBy, "<br>",
                                     "Number:", recordNumber, "<br>",
                                     "Year:", EVENT_YEAR, "<br>",
                                     "Catalogue No.:", CATALOG_NO),
                       options = pathOptions(pane = "points")) %>%
      # maybe add an IF here to control whether native range is mapped
      addPolygons(data=sptdwg, 
                  color = "red", 
                  weight = 1, 
                  fillColor = "red", 
                  fillOpacity = 0.2, 
                  options = pathOptions(pane = "poly")) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE)) %>%
    # Layers control
    addLayersControl(
      #baseGroups = c("points", "poly"),
      overlayGroups = c("Points", "Native range"),
      options = layersControlOptions(collapsed = FALSE)
    )
      
  })
  
  observeEvent(input$mymap_click, {
    ClickVar<-input$mymap_click
    
    proxy = leafletProxy("mymap")
    
    proxy %>%
    #clearGroup("NewPoints") %>%
    #clearMarkers(layerId=input$mymap_click$id) %>%
    addCircleMarkers(lng=ClickVar$lng, lat=ClickVar$lat, radius = 4, color = "green", group = "NewPoints")
  })
  

  
  
  #### 1 Single - generate statistics - EOO, AOO etc. using LC_comb function
  SingleStats <- eventReactive(input$getSingleStats, {
    #species = batchInput()
    
    # get the summary table first - 
    single_powo = batch.POWO(input$speciesinput)
    
    withProgress(message = 'Getting there...',
                 value = 2, {
                   single = LC_comb(single_powo$fullname, single_powo$IPNI_ID)
                 })
    df = single
    df
  })
  
  # output stats table 
  output$singletab <- DT::renderDataTable({
    req(input$speciesinput)
    df = SingleStats()
    df
  }, 
  options = list(pageLength = 5))
  

  
  # Use gauges to show results against LC thresholds
  output$plt1 <- flexdashboard::renderGauge({
    
    stats_df = SingleStats()
    EOOnum = stats_df$EOO
    
    gauge( EOOnum, min = 0, max = 50000, label = paste("EOO"),gaugeSectors(
      success = c(30000,50000), danger = c(0,29999)
    ))
    
  })
  
  output$plt2 <- flexdashboard::renderGauge({
    
    stats_df = SingleStats()
    AOOnum = stats_df$AOO
    
    gauge(AOOnum, min = 0, max = 15000, label = paste("AOO"),gaugeSectors(
      success = c(10000,15000), danger = c(0,9999)
    ))
    
  })
  
  output$plt3 <- flexdashboard::renderGauge({
    
    stats_df = SingleStats()
    RecordCount = stats_df$RecordCount
    
    gauge( RecordCount, min = 0, max = 150, label = paste("RecordCount"),gaugeSectors(
      success = c(75,150), danger = c(0,74)
    ))
    
  })
  
  output$plt4 <- flexdashboard::renderGauge({
    
    stats_df = SingleStats()
    TDWGnum = stats_df$TDWGCount
    
      gauge(TDWGnum, min = 0, max = 10, label = paste("TDWG count"),gaugeSectors(
        success = c(6,10), danger = c(0,5)
      ))
      
  })
  
  ### 1 single - prepare download outputs
  
  # Show GBIF occurrence points
  output$pointstab <- DT::renderDataTable({
    req(input$speciesinput)
    values$points
  }, 
  options = list(pageLength = 5))
  
  # download the cleaned gbif point file - adding in compiler and citation if added after map was first generated
  output$download = downloadHandler(
    filename = function(){
      paste(input$speciesinput, "_", Sys.Date(), ".csv", sep = "" ) # change this to species name
    },
    content = function(file){
      df = mapInput()
      df$COMPILER = paste0(input$name)
      ##, ", ", substr(input$firstname, 1, 1),".", input$initials, sep = "")
      df$CITATION = paste0(input$affiliation, sep = "")
      
      write.csv(df, file, row.names = FALSE)
      }
  )
  
  # Show csv files in mainpanel as data tables before download
  output$outallf <- DT::renderDataTable({
    allfields = allfields(input$speciesinput, input$powo)
    #credits$internal_taxon_id = input$datasetInput[1,1]
    datatable(allfields, colnames = c('ID', 'Trend', 'No threat', 'Threat unknown')) 
  })
  
  output$outassessments <- DT::renderDataTable({
    assessments = assessments(input$speciesinput, input$powo)
    #credits$internal_taxon_id = input$datasetInput[1,1]
    datatable(assessments, colnames = c('ID', 'Rationale', 'Map', 'Date', 'Version', 'Category', 'Trend', 'System', 'Language', 'Range', 'Pop', 'Habitat', 'Threats', 'Manual', 'Realm'))
  })
  
  output$outocc <- DT::renderDataTable({
    occ = countries(input$powo)
    datatable(occ, colnames = c('ID', 'Lookup', 'Country', 'Presence', 'Origin', 'Seasonality')) 
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
    datatable(hab, colnames = c('Name','Lookup','ID', 'Suitability', 'Important', 'Season')) 
  })
  
  output$outgfinput <- DT::renderDataTable({
    req(input$gfinput)
    plantspecific = plantspecific(input$gfinput, input$speciesinput, input$powo)
    #credits$internal_taxon_id = input$datasetInput[1,1]
    datatable(plantspecific, colnames = c('Growth Form', 'Lookup', 'ID')) 
  })
  
  output$outtax <- DT::renderDataTable({
  #  taxtable = taxinput()
  taxtable = taxonomy(input$key, input$speciesinput, input$powo)
    taxtable
  })
  
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
                   "data/singlezip/plantspecific.csv",
                   "data/singlezip/taxonomy.csv")
      
      #files2zip <- dir(zipdir, full.names = TRUE)
      zip::zipr('singlezip.zip', thefiles)


      #use copy to force the download
      file.copy("singlezip.zip", file)

    },
    contentType = "application/zip"
   )
  
  #######################################################
  
  ### 2. Batch inputs
  batchInput <- eventReactive(input$file1, {
    
    df <- read.csv(input$file1$datapath)
    
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
                   multi = purrr::map2_dfr(species$fullname, species$IPNI_ID, LC_comb)
                 })
    df = multi
    df
  })
  

  
  ### 2. Batch outputs
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

