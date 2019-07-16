
### Rapid Least Concern
### A tool to download occurrence data from GBIF, clean for georef errors, filter on native range using POWO and download in IUCN Red List accepted format
### Steven Bachman & Barnaby Walker - Royal Botanic Gardens, Kew

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
library(shinyjs)
#library(V8)

#### 2 - Source the functions---------------
source(here("R/resources.R"))
source(here("R/request_functions.R"))
source(here("R/calculation_functions.R"))
source(here("R/file_functions.R"))

#### 3 - UI---------------
ui <- fluidPage(
  useShinyjs(),
<<<<<<< HEAD
=======
  
>>>>>>> added checks in batch to stop early analysis or calculation
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
                                      actionButton("resetSingleForm", "Clear form!"),
                                      actionButton("randomSpecies", "Random species!"),
  
                                      fluidRow(
                                        column(8, align="center", offset = 2,
                                               tags$h4("Enter species")
                                               #actionButton("randomSpecies", "Random species!")
                                        )
                                      ),
                                      
                                      #fluidRow(
                                      #  column(4, 
                                      #         textInput("speciesinput", label = "Enter species",
                                      #                   placeholder = "Aloe zebrina")
                                      #         ),
                                      #  column(4, offset = 2,
                                      #         actionButton("randomSpecies", "Random")
                                      #  )
                                      #),
                                      
                                      textInput("speciesinput",
                                                "Enter species e.g. Aloe zebrina",
                                                placeholder = "Aloe zebrina"),
                                      
                                      #actionButton("randomSpecies", "Random species!"),
                                      
                                      p("Selected GBIF ID:"),
                                      wellPanel(textOutput("key")),
                                      p("Matching POWO ID:"),
                                      wellPanel(textOutput("powo")),

                                      tags$hr(style="border-color: black;"),
                                      fluidRow(
                                        column(8, align="center", offset = 2,
                                               tags$h4("Set parameters:")
                                         )
                                        ),
                                      
                                      # Input: EOO threshold ----
                                      sliderInput("gbif_limit", "GBIF record maximum:",
                                                  min = 1000, 
                                                  max = 10000,
                                                  value = 3000, 
                                                  step = 1000),
                                      
                                      checkboxInput("native", "Remove non-native points", FALSE),
                                      
                                      tags$hr(style="border-color: black;"),
                                      #tags$h5("3 Run analysis:"),
                                      fluidRow(
                                        column(8, align="center", offset = 2,
                                               tags$h4("Run analysis:")
                                        )
                                      ),
                                      
                                      #actionButton("runSingle", "Run analysis!"),
                                      fluidRow(
                                        column(8, align="center", offset = 2,
                                               actionButton("runSingle", "Run analysis!")
                                        )
                                      ),
                                      
                                      br(),
                                     
                                      tags$blockquote("
                                      Check the map and statistics. As a guideline, gauges in green indicate high probability of being Least Concern, red indicates possibly threatened. If you think you have an LC species, download the point file and SIS connect files"),
                                      
                                      tags$hr(style="border-color: black;"),
                                      #tags$h5("4 Download data:"),
                                      fluidRow(
                                        column(8, align="center", offset = 2,
                                               tags$h4("Download data:")
                                        )
                                      ),
                                      
                                      fluidRow(
                                        column(8, align="center", offset = 2,
                                               downloadButton('download', "Download clean point file")
                                        )
                                      ),
                                      #downloadButton('download', "Download clean point file"),
                                      
                                      br(),
                                 
                                      #helpText("Download SIS Connect csv files:"),
                                      
                                      fluidRow(
                                        column(8, align="center", offset = 2,
                                               downloadButton('downloadSIS', "Download SIS Connect Files")
                                        )
                                      ),
                                      #downloadButton('downloadSIS', "Download SIS Connect Files"),
                                      
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      
                                      # extra data that can't be generated automatically - ask user to input or select
                                      actionButton("addData", "Enter Additional Data ▼▲", style='padding:4px; font-size:80%'),
                                      
                                      br(),
                                      
                                      conditionalPanel(
                                        condition = "input.addData % 2 == 1",selectInput(
                                          "gfinput",
                                          label = ("Select growth form(s)"),
                                          choices = GROWTHFORM_LOOKUP$description,
                                          selected = head(GROWTHFORM_LOOKUP, 1)$description,
                                          selectize = TRUE,
                                          multiple = TRUE
                                        )
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.addData % 2 == 1",selectInput(
                                          "habinput",
                                          label = ("Select habitat(s)"),
                                          #choices = list("Tree - size unknown" = 1, "Tree - large" = 2, "Tree - small" = 3),
                                          choices = HABITAT_LOOKUP$description,
                                          selected = tail(HABITAT_LOOKUP, 1)$description,
                                          selectize = TRUE,
                                          multiple = TRUE
                                        )
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.addData % 2 == 1",textInput("name",
                                                                                       "Enter name of assessor/compiler:",
                                                                                       placeholder = "John Smith")
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.addData % 2 == 1",textInput("email",
                                                                                       "Enter email of assessor/compiler:",
                                                                                       placeholder = "j.smith@email.com")
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.addData % 2 == 1",textInput("affiliation",
                                                                                       "Affiliation of assessor/compiler:",
                                                                                       placeholder = "my institution")
                                      )
 
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
                                      
                                      #h5("Plant of the World Online search results:"),
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
                        fluidRow(
                          column(9, p("Upload a list of names from a CSV file. One field must be called 'name_in' and should contain binomials e.g. 'Poa annua'"))
                        ),
                        fluidRow(
                          column(9, fileInput("file1", NULL,
                                              multiple = FALSE,
                                              accept = (".csv")
                          )),
                          column(3, actionButton("resetBatchForm", "Clear upload!"))
                          
                        ),
                        br(),

                        
                        # Input: EOO threshold ----
                        sliderInput("gbif_batch_limit", "GBIF record maximum:",
                                    min = 1000, 
                                    max = 10000,
                                    value = 3000, 
                                    step = 1000),
                        
                        br(),

                        p("Click 'Get statistics' for range metrics such as EOO and AOO"),
                        
                        actionButton('getStats', "Get statistics"),
                        br(),
                        br(),
                        p("Adjust thresholds to determine Least Concern"),
                        
                        # Input: EOO threshold ----
                        sliderInput("eoo", "Extent of Occurrence (EOO):",
                                    min = 1, max = 100000,
                                    value = 30000),
                        
                        # Input: AOO threshold ----
                        sliderInput("aoo", "Area of occupancy (AOO):",
                                    min = 1, max = 10000,
                                    value = 3000),
                        
                        # Input: Number of records threshold ----
                        sliderInput("records", "Number of records:",
                                    min = 1, max = 1000,
                                    value = 75),
                        
                        # Input: Number of TDWG regions ----
                        sliderInput("tdwg", "Number of Level 3 TDWG regions:",
                                    min = 1, max = 100,
                                    value = 5),
                        
                        
                        actionButton("resetBatchSliders", "Reset Values!"),
                        br(),
                        
                        # Input: Threat reminder
                        
                        checkboxInput("threatvalue", label = "No observed, estimated, projected, inferred, or suspected declines likely 
                                       to trigger criteria A, B, C, D or E." , value = FALSE),
                        
                        helpText("Click to download SIS Connect and point files:"),
                        
                        downloadButton('downloadbatch', "Download SIS Connect Files")
                        
                        ),
                        
                        mainPanel(
                          
                          # Output: Data file ----
                          DT::dataTableOutput("contents"),
                          br(),
                          verbatimTextOutput("powo_warnings"),
                          br(),
                          # Output: Data file ----
                          DT::dataTableOutput("stats"),
                          verbatimTextOutput("threatvalue"),
                          verbatimTextOutput("stats_warnings"),
                          br()
                          
                        )
                      
             ),
                        
             tabPanel("3 Batch - user points"
                      #includeHTML("README.html")
             ),
             
             tabPanel("Help"
                      #, includeHTML("README.html")
             )
    )
)


### 4 - Server---------------
server <- function(input, output, session) {

  # value store for passing between things
  values <- reactiveValues(points=NULL,
                           native_range=NULL,
                           statistics=NULL,
                           powo_results=NULL,
                           gbif_keys=NULL,
                           species_info=NULL,
                           gbif_limit=NULL)

  
  ## home navigation events ----
  
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
  
  # single species events ----
  
  # request a random species
  # reset button
  observeEvent(input$randomSpecies, {
    withProgress(message="Finding a random species...",
                 value=2, {
                   random_species <- get_random_powo()
                 })
    updateTextInput(session, "speciesinput", value=random_species)
  }) 
  # reset form
  observeEvent(input$resetSingleForm, {
    walk(names(values), function(x) {values[[x]] <- NULL})
    updateTextInput(session, "speciesinput", value="")
    updateTextInput(session, "key", value="")
    updateTextInput(session, "powo", value="")
    updateSelectInput(session, "gfinput", selected=head(GROWTHFORM_LOOKUP, 1)$description)
    updateSelectInput(session, "habinput", selected=tail(HABITAT_LOOKUP, 1)$description)
    updateTextInput(session, "name", value="")
    updateTextInput(session, "email", value="")
    updateTextInput(session, "affiliation", value="")
    updateSliderInput(session, "gbif_limit", value=3000)
  })
  
  # observe for GBIF record row selection
  observe_row_selection <- observe(suspended=TRUE, {
    input$summarytab_rows_selected
    isolate({
      values$key <- values$gbif_keys[input$summarytab_rows_selected, ]$usageKey
    })
  })
  
  # observer to prevent calculations before IDs have been selected
  observe({
    ids_found <- ! is_empty(values$key) & ! is_empty(values$powo)
    toggleState(id="runSingle", condition=ids_found)
  })
  
  # request points and species info
  observeEvent(input$runSingle, {
    
    withProgress(message = 'Getting points from GBIF...',
                 value = 2, {
                   gbif_results = get_gbif_points(values$key, input$gbif_limit)
                 })
    
    withProgress(message="Getting native range from POWO...",
                 value=2, {
                   powo_results <- get_native_range(values$powo)
    })
    
    values$native_range <- powo_results
    
    # add indicator for points in native range
    gbif_results <- check_if_native(gbif_results, values$native_range, TDWG_LEVEL3)
    
    # use POWO info to generate species info tables
    values$species_info <- get_species_info(list(
      key = values$key,
      powo = values$powo,
      author = filter(values$powo_results, IPNI_ID == values$powo)$author,
      native_range = values$native_range,
      name = input$name,
      email = input$email,
      affiliation = input$affiliation,
      habinput = input$habinput,
      gfinput = input$gfinput
    ))

    gbif_results$BINOMIAL = input$speciesinput
    values$points <- gbif_results

    powo_info <- filter(values$powo_results, IPNI_ID == values$powo)
    
    withProgress(message = 'Calculating statistics...',
                 value = 2, {
                   values$statistics = calculate_statistics(powo_info$name, powo_info$IPNI_ID, values$points, values$native_range)
                 })
  })
  
  # point file download handler
  output$download = downloadHandler(
    filename = function(){
      date <- format(Sys.Date(), "%Y%m%d")
      species_name <- str_replace_all(input$speciesinput, " ", "_")
      paste(species_name, "_", date, ".csv", sep = "" )
    },
    content = function(file){
      df = select(values$points, -native_range)
      df$COMPILER = input$name
      df$CITATION = input$affiliation
      
      write_csv(df, file)
    }
  )
  
  # SIS zip file download handler
  output$downloadSIS = downloadHandler(
    filename = function(){
      date <- format(Sys.Date(), "%Y%m%d")
      species_name <- str_replace_all(input$speciesinput, " ", "_")
      paste(species_name, "_sis_connect_", date, ".zip", sep = "" )
    },
    content = function(file){
      zip_folder = here("data/singlezip")
      
      # update species info tables
      input_info <- reactiveValuesToList(input)
      input_info$author <- filter(values$powo_results, IPNI_ID == input$powo)$author
      input_info$native_range <- values$native_range
      values$species_info <- get_species_info(input_info)
      
      prepare_sis_files(values$species_info, zip_folder=zip_folder)
      
      
      files_to_zip = purrr::map_chr(names(values$species_info), 
                                    ~paste(zip_folder, "/", .x, ".csv", sep=""))
      
      zip::zipr('singlezip.zip', files_to_zip)
      
      #use copy to force the download
      file.copy("singlezip.zip", file)
    },
    contentType = "application/zip"
  )

  # single species display items ----
  
  # output selected key of GBIF record
  output$key <- renderText({
    shiny::validate(
      need(input$speciesinput == "" | ! is_empty(values$key),
           message="Please select a row from the table.")
    )
    
    values$key
  })
  
  # lookup and output matching POWO ID
  output$powo <- renderText({
    if (! is_empty(values$key)) {
      gbif_name <- values$gbif_keys[values$gbif_keys$usageKey == values$key, ]$acceptedSpecies  
      values$powo_results <- search_name_powo(gbif_name)
      matching_powo <- filter(values$powo_results, accepted == TRUE)
      matching_powo <- filter(matching_powo, name == gbif_name)
      
      shiny::validate(
        need(is_empty(values$key) | nrow(matching_powo) > 0,
             message="No accepted species found in POWO.")
      )
      values$powo <- matching_powo$IPNI_ID
    }
    values$powo
  })
  
  #Show results of GBIF search as a table
  output$summarytab <- DT::renderDataTable({
    req(input$speciesinput)
    values$gbif_keys <- search_name_gbif(input$speciesinput)
    observe_row_selection$resume()
    values$gbif_keys
  },
  options = list(pageLength = 5),
  selection="single",
  server=TRUE)
  
  # leaflet map to show points
  output$mymap <- renderLeaflet({
    if (is.null(values$points)) {
      return(NULL)
    }

    df <- values$points
    if (input$native & ! is.null(values$native_range)) {
      df <- filter(df, ! is.na(native_range))
    }
    
    range_shapes <- filter(TDWG_LEVEL3, LEVEL3_COD %in% values$native_range$LEVEL3_COD)
    
    leaflet(data = df) %>%
      addMapPane("points", zIndex = 420) %>%
      addMapPane("poly", zIndex = 410) %>%
      addCircleMarkers(group = "Points",
                       lng = ~DEC_LONG,
                       lat = ~DEC_LAT, 
                       radius = 4, 
                       color = "green", 
                       popup = ~paste("Collector:", recordedBy, "<br>",
                                     "Number:", recordNumber, "<br>",
                                     "Year:", EVENT_YEAR, "<br>",
                                     "Catalogue No.:", CATALOG_NO),
                       options = pathOptions(pane = "points")) %>%
      # maybe add an IF here to control whether native range is mapped
      addPolygons(group = "Native range",
                  data=range_shapes, 
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

  # output stats table 
  output$singletab <- DT::renderDataTable({
    req(input$speciesinput)
    values$statistics
  }, 
  options = list(pageLength = 5))

  # Use gauges to show results against LC thresholds
  output$plt1 <- flexdashboard::renderGauge({
    EOOnum = values$statistics$EOO
    
    gauge( EOOnum, min = 0, max = 50000, label = paste("EOO"),gaugeSectors(
      success = c(30000,50000), danger = c(0,29999)
    ))
  })
  
  output$plt2 <- flexdashboard::renderGauge({
 
    AOOnum = values$statistics$AOO
    
    gauge(AOOnum, min = 0, max = 10000, label = paste("AOO"),gaugeSectors(
      success = c(3000,10000), danger = c(0,2999)
    ))
    
  })
  
  output$plt3 <- flexdashboard::renderGauge({

    RecordCount = values$statistics$RecordCount
    
    gauge( RecordCount, min = 0, max = 150, label = paste("RecordCount"),gaugeSectors(
      success = c(75,150), danger = c(0,74)
    ))
    
  })
  
  output$plt4 <- flexdashboard::renderGauge({

    TDWGnum = values$statistics$TDWGCount
    
      gauge(TDWGnum, min = 0, max = 10, label = paste("TDWG count"),gaugeSectors(
        success = c(6,10), danger = c(0,5)
      ))
      
  })

  # Show GBIF occurrence points
  output$pointstab <- DT::renderDataTable({
    req(input$speciesinput)
    if (! is.null(values$points)) {
      select(values$points, -native_range)
    }
  }, 
  options = list(pageLength = 5))

  # Show csv files in mainpanel as data tables before download
  output$outallf <- DT::renderDataTable({
    values$species_info$allfields
  })
  
  output$outassessments <- DT::renderDataTable({
    values$species_info$assessments
  })
  
  output$outocc <- DT::renderDataTable({
    values$species_info$countries
  })
  
  output$outcredits <- DT::renderDataTable({
    values$species_info$credits
  })
  
  output$outhab <- DT::renderDataTable({
    req(input$habinput)
    values$species_info$habitats
  })
  
  output$outgfinput <- DT::renderDataTable({
    req(input$gfinput)
    values$species_info$plantspecific
  })
  
  output$outtax <- DT::renderDataTable({
    values$species_info$taxonomy
  })
  
  
  
  # batch species events ----
  
  # reset uploaded values
  observeEvent(input$resetBatchForm, {
    walk(names(values), function(x) {values[[x]] <- NULL})
  })
  
  observeEvent(input$resetBatchSliders, {
    updateSliderInput(session, "eoo", value=30000)
    updateSliderInput(session, "aoo", value=3000)
    updateSliderInput(session, "records", value=75)
    updateSliderInput(session, "tdwg", value=5)
    updateSliderInput(session, "gbif_limit", value=3000)
  })
  
  # upload and get species ids from POWO
  observeEvent(input$file1, {
    input_data <- read_csv(input$file1$datapath)

    withProgress(message="Checking names in POWO...",
                 value=2,
                 {
                   values$powo_results=purrr::map_dfr(input_data$name_in, get_accepted_name)
                 })
  })
  
  # observer to prevent calculations before species have been uploaded
  observe({
    species_uploaded <- ! is_empty(values$powo_results)
    toggleState(id="getStats", condition=species_uploaded)
  })
  
  # calculate statistics and get info for all species
  observeEvent(input$getStats, {
    # TODO: clean up how this works with warnings and missing values
    withProgress(message="Getting GBIF reference keys...",
                 value=2, 
                 {
                   accepted_species <- 
                    values$powo_results %>%
                    filter(! is.na(IPNI_ID), accepted) %>%
                    select(IPNI_ID, name_in, name_searched) %>%
                    mutate(gbif_results=map(name_in, get_gbif_key)) %>%
                    unnest()
                   
                   missing_species <-
                     values$powo_results %>%
                     filter(is.na(IPNI_ID) | ! accepted) %>%
                     mutate(gbif_key=NA_character_,
                            warning=case_when(is.na(IPNI_ID) ~ "No matching name found in POWO",
                                              ! accepted ~ "Not an accepted species in POWO",
                                              TRUE ~ NA_character_)) %>%
                     select(IPNI_ID, name_in, name_searched, gbif_key, warning)
                   
                   values$gbif_keys <- bind_rows(accepted_species, missing_species)
                   
                 })
    
    # this could grind things to a halt with too many species/points
    
    withProgress(message="Getting points from GBIF...",
                 value=2, 
                 {
                   values$points <-
                    values$gbif_keys %>%
                    mutate(points=map(gbif_key, get_gbif_points, input$gbif_batch_limit)) %>%
                    select(IPNI_ID, points, name_searched) %>%
                    unnest()
                 })
    
    withProgress(message="Getting native ranges from POWO...",
                 value=2, 
                 {
                   # only want to search for native range for things we have an ID for
                   filtered_powo <-
                     values$powo_results %>%
                     filter(! is.na(IPNI_ID), accepted)
                   
                   values$native_range <- map_dfr(filtered_powo$IPNI_ID, get_native_range)
                 })
    
    nested_native_range <- 
      values$native_range %>% 
      group_by(POWO_ID) %>% 
      nest(.key = "native_tdwg")
    
    withProgress(message="Checking which points are in native range...",
                 value=2,
                 {
                  # clip points to native ranges
                  values$points <-
                    values$points %>%
                    group_by(IPNI_ID) %>%
                    nest() %>%
                    left_join(nested_native_range, by=c("IPNI_ID"="POWO_ID")) %>%
                    mutate(points=map2(data, native_tdwg, ~check_if_native(.x, .y, TDWG_LEVEL3))) %>%
                    unnest(points)
                 })
    
    withProgress(message="Calculating least concern statistics...",
                 value=2,
                 {
                   values$statistics <-
                     values$points %>%
                     group_by(name_searched, IPNI_ID) %>%
                     nest() %>%
                     left_join(values$gbif_keys, by=c("IPNI_ID", "name_searched")) %>%
                     left_join(nested_native_range, by=c("IPNI_ID"="POWO_ID")) %>%
                     mutate(statistics=pmap(list(name_in, IPNI_ID, data, native_tdwg, warning), 
                                            calculate_statistics)) %>%
                     select(name_searched, statistics) %>%
                     unnest()
                 })    
  })
  
  # observer to prevent download before calculations done and threat statement checked
  observe({
    download_ready <- ! is_empty(values$statistics) & input$threatvalue
    toggleState(id="downloadbatch", download_ready)
  })
  
  # batch species zip file download handler
  output$downloadbatch = downloadHandler(
    
    # download the results
    filename = function(){
      date <- format(Sys.Date(), "%Y%m%d")
      paste("batch_SIS_connect_", date, ".zip", sep = "" )
    },
    content = function(file){
      batch_folder <- here("data/batchzip")
      
      # filter out any result with a warning
      least_concern_results <- filter(values$statistics, is.na(Warning))
      
      # keep only the least concern results
      least_concern_results <- filter(least_concern_results,
                                      EOO >= eooValue(),
                                      AOO >= aooValue(),
                                      RecordCount >= recordsValue(),
                                      TDWGCount >= tdwgValue())
      
      # get the points
      least_concern_points <- filter(values$points, IPNI_ID %in% least_concern_results$POWO_ID)
      
      # now the csv files
      least_concern_ranges <- filter(values$native_range, POWO_ID %in% least_concern_results$POWO_ID)
      least_concern_keys <- filter(values$gbif_keys, IPNI_ID %in% least_concern_results$POWO_ID)
      least_concern_powo <- filter(values$powo_results, IPNI_ID %in% least_concern_results$POWO_ID)
      
      # get all the info tables for all species
      values$species_info <- list(
        allfields=map_dfr(least_concern_results$POWO_ID, allfields),
        assessments=map_df(least_concern_results$POWO_ID, assessments),
        countries=countries(least_concern_ranges),
        credits=map_dfr(least_concern_results$POWO_ID, credits),
        habitats=map_dfr(least_concern_results$POWO_ID, habitats, HABITAT_LOOKUP),
        plantspecific=map_dfr(least_concern_results$POWO_ID, plantspecific, GROWTHFORM_LOOKUP),
        taxonomy=pmap_dfr(list(least_concern_results$POWO_ID, least_concern_keys$gbif_key, least_concern_powo$author), taxonomy, taxonomy_lookup=IUCN_TAXONOMY),
        results=values$statistics,
        points=least_concern_points
      )
      
      # prepare all files for download as a zip
      prepare_sis_files(values$species_info, zip_folder=batch_folder)
      
      files_to_zip = purrr::map_chr(names(values$species_info), 
                                    ~paste(batch_folder, "/", .x, ".csv", sep=""))
      
      
      zip::zipr('batchzip.zip', files_to_zip)
      
      # use copy to force the download
      file.copy("batchzip.zip", file)      
    },
    contentType = "application/zip"
    
    
  )
  
  # batch species reactive events ----

  output$threatvalue<- renderPrint({ 
    if (! input$threatvalue & ! is_empty(values$statistics)) {
      cat("WARNING - please consider possible threats (past, present, future) that could cause declines and trigger criteria A, B, C, D, or E.")
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
  
  # batch species display items ----
  
  # display powo ids for all species
  output$contents <- DT::renderDataTable({
    req(values$powo_results)
    datatable(values$powo_results,
              options = list(pageLength = 5)) %>%
      formatStyle("accepted",
                  target="row",
                  color=styleEqual(c(0, NA), c("red", "red")))
  })

  output$powo_warnings <- renderPrint({
    req(values$powo_results)
    n_not_found <- sum(is.na(values$powo_results$IPNI_ID))
    n_synonyms <- sum(! values$powo_results$accepted, na.rm=TRUE)
    n_total <- nrow(values$powo_results)

    cat(
      sprintf("%d names searched in POWO", n_total),
      sprintf("%d names not found", n_not_found),
      sprintf("%d names identified as synonyms", n_synonyms),
      "missing names and synonyms (highlighted in red above) will be ommited from all calculations",
      sep="\n"
    )
  })
  
  
  # display stats for least concern species
  output$stats <- DT::renderDataTable({
      
      req(values$statistics)
      
      values$statistics <- mutate(values$statistics,
                                  leastConcern=EOO >= eooValue() & 
                                    AOO >= aooValue() & 
                                    RecordCount >= recordsValue() & 
                                    TDWGCount >= tdwgValue())
      
      values$statistics <- arrange(values$statistics, desc(leastConcern))
    
      datatable(values$statistics,
                options=list(pageLength=5)) %>%
        formatStyle("leastConcern",
                    target="row",
                    color=styleEqual(c(1, NA), c("green", "red")))
    })
  
  output$stats_warnings <- renderPrint({
    req(values$statistics)
    
    if ("leastConcern" %in% colnames(values$statistics)) {
      n_least_concern <- sum(values$statistics$leastConcern, na.rm=TRUE)
      n_warnings <- sum(is.na(values$statistics$leastConcern))
      n_total <- nrow(values$statistics)
      
      cat(
        sprintf("%d species considered", n_total),
        sprintf("%d with warnings", n_warnings),
        sprintf("%d identified as least concern", n_least_concern),
        "only information about species identified as least concern will be included in the SIS Connect files.",
        sep="\n"
      )  
    }
    
  })

}  


# Run the application 
shinyApp(ui = ui, server = server)

#rm(list=ls())

