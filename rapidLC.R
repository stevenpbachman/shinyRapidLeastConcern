#
# This is a Shiny web application. You can run the application by clicking # the 'Run App' button above.
#
### GBIF 2 Red List Point Map
### A tool to download occurrence data from GBIF, clean for georef errors, and download in IUCN Red List accepted format
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


#### 1 - libraries
library(magrittr)
library(readr)
#library(ggplot2)
#library(plotly)
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

#install.packages("tidyverse")

#### 2 - shapefiles/rasters/other files
TDWGpolys = sf::read_sf("level3/level3.shp")
#TDWGpolys = rgdal::readOGR("level3/level3.shp")
###
raster.tdwg = raster::raster("rasters/tdwg3.tiff")
tdwg_raster <- read.csv("tdwg_raster.csv")
plantgflist <- read.csv("Plantgrowthforms.plantgrowthformslookup.csv", encoding="UTF-16")
habitatlist <- read.csv("habitats.csv", encoding="UTF-16")
taxonomy_iucn <- read.csv("taxonomy_iucn.csv", encoding="UTF-16")
TDWG_to_IUCN_version3_UTF <- read.delim("TDWG_to_IUCN_version3_UTF-8.txt", encoding="UTF-8", na.strings="")

#### 3 - functions
# 3.1 get the gbif key
gbif.key = function (full_name) {
  #full_name = species.list$full_name
  gbif.key = rgbif::name_backbone(
    name = full_name,
    rank = 'species',
    kingdom = 'Plantae',
    strict = FALSE,
    verbose = TRUE #change to TRUE to get more options
  )
  
  # bind together in case there are missing data
  merged = dplyr::bind_rows(gbif.key$alternatives, gbif.key$data)
  
  
  # change col names
  colnames(merged)[which(names(merged) == "species")] = "acceptedSpecies"
  
  if (!"acceptedUsageKey" %in% colnames(merged)) {
    merged$acceptedUsageKey = NA
    as.character(merged$acceptedUsageKey)
  }
  
  # subset the data with the fields you want
  options = subset(
    merged,
    select = c(
      usageKey,
      acceptedUsageKey,
      scientificName,
      rank,
      status,
      confidence,
      family,
      acceptedSpecies
    )
  )
  
  # arrange table in descending order to show best guess at top of table
  options = dplyr::arrange(options, desc(confidence))
  return(options)
}

# 3.2 fetch the points using key
gbif.points = function(key) {
  #gbifkey = result.table
  #gbifkey = result.table[,3]
  
  if (key == "") {
    
    res = data.frame(
      basisOfRecord = as.character("NA"),
      BINOMIAL = as.character("NA"),
      DEC_LONG = as.numeric("-999"),
      DEC_LAT = as.numeric("-999"),
      EVENT_YEAR = as.integer("-999"),
      CATALOG_NO = as.character("NA"),
      SPATIALREF = "WGS84",
      PRESENCE = "1",
      ORIGIN = "1",
      SEASONAL = "1",
      DATA_SENS = "No",
      SOURCE = as.character("NA"),
      YEAR = as.integer("-999"),
      COMPILER = as.character("NA"),
      CITATION = as.character("NA"),
      stringsAsFactors = FALSE
    )
  } else    {
    
    res = rgbif::occ_data(
      taxonKey = key,
      hasGeospatialIssue = FALSE,
      hasCoordinate = TRUE,
      #geometry = mygeom,
      limit = 1000
    )
    
    
    res$data$taxonKey = key
    res = res$data
    res = as.data.frame(res)
    
    
    if (!"recordNumber" %in% colnames(res)) {
      res$recordNumber = NA
      as.character(res$recordNumber)
    }
    
    if (!"decimalLongitude" %in% colnames(res)) {
      res$decimalLongitude = NA
      as.character(res$decimalLongitude)
    }
    
    if (!"decimalLatitude" %in% colnames(res)) {
      res$decimalLatitude = NA
      as.character(res$decimalLatitude)
    }
    
    if (!"year" %in% colnames(res)) {
      res$year = NA
      as.character(res$year)
    }
    
    if (!"datasetKey" %in% colnames(res)) {
      res$datasetKey = NA
    }
    
    if (!"basisOfRecord" %in% colnames(res)) {
      res$basisOfRecord = NA
    }
    
    if (!"catalogNumber" %in% colnames(res)) {
      res$catalogNumber = NA
    }
    
    if (!"recordedBy" %in% colnames(res)) {
      res$recordedBy = NA
    }
    
    if (!"issues" %in% colnames(res)) {
      res$issues = NA
    }
    
    if (!"institutionCode" %in% colnames(res)) {
      res$institutionCode = NA
    }
    
    if (!"country" %in% colnames(res)) {
      res$country = NA
    }
    
    if (!"familyKey" %in% colnames(res)) {
      res$familyKey = NA
      as.character(res$familyKey)
    }
    
    if (!"scientificName" %in% colnames(res)) {
      res$scientificName = NA
    }
  }
  
  res = subset(
    res,
    select = c(
      'basisOfRecord',
      'datasetKey',
      #'taxonKey',
      #'familyKey',
      'scientificName',
      'decimalLongitude',
      'decimalLatitude',
      'year',
      'issues',
      #'country',
      'recordNumber',
      'catalogNumber',
      'recordedBy'
      #'institutionCode'
    )
  )
  
  colnames(res)[which(names(res) == "decimalLatitude")] = "DEC_LAT"
  colnames(res)[which(names(res) == "decimalLongitude")] = "DEC_LONG"
  colnames(res)[which(names(res) == "scientificName")] = "BINOMIAL"
  colnames(res)[which(names(res) == "year")] = "EVENT_YEAR"
  colnames(res)[which(names(res) == "catalogNumber")] = "CATALOG_NO"
  colnames(res)[which(names(res) == "basisOfRecord")] = "BasisOfRec"
  
  res$SPATIALREF = "WGS84"
  res$PRESENCE = "1"
  res$ORIGIN = "1"
  res$SEASONAL = "1"
  res$YEAR = substring(Sys.Date(), 1, 4)
  res$DATA_SENS = "No"
  res$SOURCE = paste0("https://www.gbif.org/dataset/", res$datasetKey, sep = "")
  res$COMPILER = ""
  res$CITATION = ""
  
  return(res)
} 

# 3.3 check name against POWO
# takes binomial and checks against POWO (http://www.plantsoftheworldonline.org)
check.accepted.POWO = function(name_in) {
  
  #name_in = "Olea europaea subsp. cerasiformis"
  #name_in = "Eugenia cordata"
  
  # get binomial from string
  #binom = word(name_in, start = 1,2)
  
  # use name full name to search API  
  full_url =  paste("http://plantsoftheworldonline.org/api/1/search?q=names:", name_in, sep = "")
  #full_url =  paste("http://plantsoftheworld.online/api/2/search?q=", name_in, sep = "")
  
  # encode
  full_url = utils::URLencode(full_url)
  
  # new api 2
  #http://plantsoftheworld.online/api/2/taxon/urn:lsid:ipni.org:names:594092-1
  
  # get raw json data
  raw.data <- readLines(full_url, warn = "F", encoding = "UTF-8")
  
  # organise
  rd = jsonlite::fromJSON(raw.data)
  
  if (length(rd$results) == 0) {
    # add new column to results and call it warning
    
    accepted = "NA"          
    author = ""  
    #kingdom = ""  
    name = ""  
    #rank  = "" 
    #synonymOf = ""
    #base_url = ""    
    IPNI_ID = ""
    #search_name = name_in
    #fullname = name_in
    results = data.frame(IPNI_ID, name,author, accepted)
    
  } else {
    
    # make data frame
    results = as.data.frame(rd$results)
    
    # add original search term
    #results$search_name = name_in
    #results$fullname = name_in
    
    
    # PROBLEM HERE - var with no author - replace with blank field?
    if (!"author" %in% colnames(results)) {
      results$author = NA
    }
    
    # split url to get url and IPNI_ID
    results = results %>% tidyr::separate(url, into = c("base_url", "IPNI_ID"), sep = "names:")
    
    # only include these fields - you don't want synonym of
    results = subset(results, select=c(IPNI_ID, name,author, accepted))
    
    # take out any results where it matched on something that wasn't species 
    #results = subset(results, rank == "Species") 
    
    # check if 
    if (nrow(results) < 1) {
      # add new column to results and call it warning
      
      accepted = "NA"          
      author = ""  
      #kingdom = ""  
      name = ""  
      #rank  = "" 
      #synonymOf = ""
      #base_url = ""    
      IPNI_ID = ""
      #search_name = name_in 
      #fullname = name_in
      results = data.frame(IPNI_ID, name,author, accepted)
      
    }
    # make data frame
    results = as.data.frame(results)
  }
  
  return(results)
  
}

# 3.3b check name against POWO
# takes binomial and checks against POWO (http://www.plantsoftheworldonline.org)
batch.POWO = function(name_in) {
  
  #name_in = "Olea europaea subsp. cerasiformis"
  #name_in = "Eugenia cordata"
  #name_in = "Rhamnus intermedia"
  
  
  # get binomial from string
  #binom = word(name_in, start = 1,2)
  
  # use name full name to search API  
  full_url =  paste("http://plantsoftheworldonline.org/api/1/search?q=names:", name_in, sep = "")
  #full_url =  paste("http://plantsoftheworld.online/api/2/search?q=", name_in, sep = "")
  
  # encode
  full_url = utils::URLencode(full_url)
  
  # new api 2
  #http://plantsoftheworld.online/api/2/taxon/urn:lsid:ipni.org:names:594092-1
  
  # get raw json data
  raw.data <- readLines(full_url, warn = "F", encoding = "UTF-8")
  
  # organise
  rd = jsonlite::fromJSON(raw.data)
  
  if (length(rd$results) == 0) {
    # add new column to results and call it warning
    
    accepted = "NA"          
    author = ""  
    #kingdom = ""  
    name = ""  
    #rank  = "" 
    #synonymOf = ""
    #base_url = ""    
    IPNI_ID = ""
    #search_name = name_in
    #fullname = name_in
    results = data.frame(IPNI_ID, name,author, accepted)
    
  } else {
    
    # make data frame
    results = as.data.frame(rd$results)
    
    # add original search term
    #results$search_name = name_in
    #results$fullname = name_in
    
    
    # PROBLEM HERE - var with no author - replace with blank field?
    if (!"author" %in% colnames(results)) {
      results$author = NA
    }
    
    # split url to get url and IPNI_ID
    results = results %>% tidyr::separate(url, into = c("base_url", "IPNI_ID"), sep = "names:")
    
    # only include these fields - you don't want synonym of
    results = subset(results, select=c(IPNI_ID, name,author, accepted))
    
    # take out any results where it matched on something that wasn't species 
    #results = subset(results, rank == "Species") 
    
    # check if 
    if (nrow(results) < 1) {
      # add new column to results and call it warning
      
      accepted = "NA"          
      author = ""  
      #kingdom = ""  
      name = ""  
      #rank  = "" 
      #synonymOf = ""
      #base_url = ""    
      IPNI_ID = ""
      #search_name = name_in 
      #fullname = name_in
      results = data.frame(IPNI_ID, name,author, accepted)
      
    }
    # make data frame
    results = as.data.frame(results)
    results = results[1,]
  }
  
  return(results)
  
}

# 3.4 get the TDWG native range from POWO
check.tdwg = function(ID){
  
  full_url = paste0("http://plantsoftheworld.online/api/2/taxon/urn:lsid:ipni.org:names:", ID, "?fields=distribution")
  
  # encode
  full_url = utils::URLencode(full_url)
  
  # get raw json data
  raw.data <- readLines(full_url, warn = "F", encoding = "UTF-8")
  
  # organise
  rd = jsonlite::fromJSON(raw.data)
  
  rd = as.data.frame(rd$distribution$natives)
  
  if (!nrow(rd)) {
    nat_dis = data.frame(
      tdwgCode = "NA",
      featureId = "NA",
      tdwgLevel = "NA",
      establishment = "NA",
      LEVEL3_NAM = "NA",
      POWO_ID = ID,
      TDWG_Name = "NA",
      ID = "")
  }
  
  else{
    rd["POWO_ID"] = ID
    colnames(rd)[which(names(rd) == "name")] = "LEVEL3_NAM"
    colnames(rd)[which(names(rd) == "tdwgCode")] = "LEVEL3_COD"
    return(rd)
  }
  
}

# 3.5 native range clip
native.clip = function(points, TDWGpolys, powo){
  
  #sptdwg = check.tdwg("530052-1")
  #sptdwg = merge(TDWGpolys, tdwg.dist)
  
  # prepare the point data as spatial
  decimalLongitude = points$DEC_LONG
  decimalLatitude = points$DEC_LAT
  coords = cbind(decimalLongitude, decimalLatitude)
  coords = base::data.matrix(coords)
  coords = base::unique(coords)
  sp = sp::SpatialPoints(coords)
  
  #merge with sptdwg
  #merged_points = merge(joined_points, sptdwg, by = LEVEL3_COD)
  
  # extract values from TDWG raster
  extract.out = raster::extract(raster.tdwg, sp, df = TRUE)
  extract.out = cbind(extract.out, coords)
  colnames(extract.out)[which(names(extract.out) == "tdwg3")] = "VALUE"
  merge = merge(extract.out, tdwg_raster, by = "VALUE", all.x = TRUE)
  tdwg.merge.out = subset(merge, LEVEL3_NAM != "NA") # this gets rid of TDWG areas that had no points in
  # change colnames 
  colnames(tdwg.merge.out)[which(names(tdwg.merge.out) == "decimalLongitude")] = "DEC_LONG"
  colnames(tdwg.merge.out)[which(names(tdwg.merge.out) == "decimalLatitude")] = "DEC_LAT"
  point.clean.bind = merge(points, tdwg.merge.out, by = c("DEC_LONG", "DEC_LAT"), all = TRUE)
  
  # now I have points with TDWG regions added - need to merge with TDWG native range
  sptdwg = check.tdwg(powo)
  sptdwg = merge(TDWGpolys, sptdwg)
  sptdwg = within(sptdwg, rm("geometry"))
  native_points = merge(point.clean.bind, sptdwg)
  native_points
}

# 3.6 SIS files allfields.csv
allfields = function(species, ID){
  #check = check.accepted.POWO(species)
  #ID = check$IPNI_ID  
  allf = data.frame(
    internal_taxon_id = ID,	
    CurrentTrendDataDerivation.value = "Suspected",
    nothreats.nothreats = "TRUE",
    threatsunknown.value = "FALSE")
  
  return(allf)
}

# 3.7 SIS files assessments.csv
assessments = function(species, ID){
  sysdate = base::Sys.Date()
  day = base::substr(sysdate, 9, 10)
  month = base::substr(sysdate, 6, 7)
  year = base::substr(sysdate, 1, 4)
  as = data.frame(
    internal_taxon_id = ID,	
    RedListRationale.value = "",	
    mapstatus.status = "Done",	
    RedListAssessmentDate.value = paste0(day, "/", month, "/", year),	
    RedListCriteria.critVersion	= "3.1",
    RedListCriteria.manualCategory	= "LC",
    PopulationTrend.value	= "Stable",
    System.value	= "Terrestrial",
    language.value	= "English",
    rangedocumentation.narrative	= "",
    populationdocumentation.narrative	= "",
    habitatdocumentation.narrative	= "",
    threatsdocumentation.value	= "",
    redlistcriteria.ismanual	= "TRUE",
    biogeographicrealm.realm = "")
  return(as)
}

# 3.8 SIS files countries.csv
countries = function(ID){
  
  internal_taxon_id = ID
  range = check.tdwg(internal_taxon_id) 
  # merge with IUCN country file
  colnames(TDWG_to_IUCN_version3_UTF)[which(names(TDWG_to_IUCN_version3_UTF) == "Level.3.code")] =
    "LEVEL3_COD"
  merged_range = merge(range, TDWG_to_IUCN_version3_UTF, by = "LEVEL3_COD")
  country_tab = merged_range[c(6,10,9)]
  country_tab$CountryOccurrence.CountryOccurrenceSubfield.presence = "Extant"
  country_tab$CountryOccurrence.CountryOccurrenceSubfield.origin = "Native"
  country_tab$CountryOccurrence.CountryOccurrenceSubfield.seasonaility = "Resident"
  country_tab
}

# 3.9 SIS files - credits.csv
credits = function(name,email,affiliation,species, ID) {
  #check = check.accepted.POWO(species)
  #ID = check$IPNI_ID
  credits = data.frame(
    internal_taxon_id = ID,
    credit_type = "Assessor",
    firstName = stringr::word(name,1),
    lastName = stringr::word(name,2),
    initials = "",
    Order = "1",
    email = email,
    affiliation = affiliation,
    user_id = "1"
  )
  return(credits)
}

# 3.10 SIS files - habitats.csv
habitats = function(habitatinput, species, ID) {
  hab = data.frame(description = habitatinput)
  hab = merge(hab,habitatlist, by = "description")
  hab$internal_taxon_id = 	ID
  colnames(hab)[which(names(hab) == "description")] = "GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsName"
  colnames(hab)[which(names(hab) == "code")] = "GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsLookup"
  hab$GeneralHabitats.GeneralHabitatsSubfield.suitability = "Suitable"
  hab$GeneralHabitats.GeneralHabitatsSubfield.majorImportance	= ""
  hab$GeneralHabitats.GeneralHabitatsSubfield.season = ""	
  return(hab)
}

# 3.11 SIS files - plantspecific.csv
plantspecific = function(gfinput, species, ID) {
  
  gf = data.frame(description = gfinput)
  gf = merge(gf,plantgflist, by = "description")
  gf$internal_taxon_id = 	ID
  colnames(gf)[which(names(gf) == "description")] = "PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName"
  colnames(gf)[which(names(gf) == "code")] = "PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup"
  gf
  return(gf)
}

#key = "2704179"
#species = "Poa annua"
#ID = "320035-2"
#taxtest = taxonomy(key, species, ID)

# 3.12 SIS files taxonomy.csv
taxonomy = function(key, species, ID){
  
  #check = gbif.key(species)
  nameinfo = rgbif::name_usage(key)
  powo = check.accepted.POWO(species)
  powo2 = subset(powo, subset = powo$IPNI_ID == ID)
  
  ID = ID
  tax = data.frame(
    internal_taxon_id = ID,	
    #kingdom	= "PLANTAE",
    #phylum = "",
    #classname = "",
    #ordername = "",
    family =  nameinfo$data$family,
    genus = nameinfo$data$genus,
    species = word(nameinfo$data$species,2),  
    taxonomicAuthority = powo2$author)
  
  #now merge with iucn taxonomy to get higher tax
  #colnames(taxonomy_iucn)[which(names(taxonomy_iucn) == "fam")] = "family"
  taxmerged = merge(tax, taxonomy_iucn, by = "family")
  taxmerged = taxmerged[c(2,6:8,1,3:5)]
  
  return(taxmerged)
}

# 3.13 save all SIS files
all_SIS = function(species, powo, name, email, affiliation, habitat, growthform, key){
  
  do.call(file.remove, list(list.files(paste0(getwd(), "/forzip/"), full.names = TRUE)))
  
  
  allfpath = paste0(getwd(), "/forzip/allfields.csv")
  allfields = allfields(species, powo)
  write.csv(allfields, allfpath)
  
  assessmentspath = paste0(getwd(), "/forzip/assessments.csv")
  assessmentstable = assessments(species, powo)
  write.csv(assessmentstable, assessmentspath)
  
  occspath = paste0(getwd(), "/forzip/countries.csv")
  occstable = countries(powo)
  write.csv(occstable, occspath)
  
  credpath = paste0(getwd(), "/forzip/credits.csv")
  credits = credits(name, email, affiliation, species, powo)
  write.csv(credits, credpath)
  
  habitatpath = paste0(getwd(), "/forzip/habitats.csv")
  hab = habitats(habitat, species, powo)
  write.csv(hab, habitatpath)
  
  plantspath = paste0(getwd(), "/forzip/plantspecific.csv")
  plantspecific = plantspecific(growthform, species, powo)
  write.csv(plantspecific, plantspath)
  
  #taxpath = paste0(getwd(), "/forzip/taxonomy.csv")
  ##taxtable =  taxinput()
  #taxtable = taxonomy(key, species, powo)
  #write.csv(taxtable, taxpath)

}

# 3.14 EOO and AOO
eoo.aoo = function(native) {
  
  mypointsll = data.frame(lat = native$DEC_LAT, long = native$DEC_LONG)
  centreofpoints <- trueCOGll(mypointsll)
  mypointsxy <- simProjWiz(mypointsll, centreofpoints)
  
  #EOO and AOO calculation
  EOOm2 <- EOOarea(mypointsxy)
  EOOkm2 <- EOOm2 / 1000000
  EOOkm2abs = abs(EOOkm2)
  rec_count = nrow(mypointsxy)
  cellsizem <- 10000
  AOOnocells <- AOOsimp (mypointsxy, cellsizem)
  
  eoo.aoo.res = data.frame(
    EOO = round(EOOkm2abs,0),
    AOO = AOOnocells,
    RecordCount = rec_count)
  
  return(eoo.aoo.res)
}

# 3.15 combine functions to get LC results - use apply on this
LC_comb = function(species){
  
  
  full_name = species$name
  ID = species$IPNI_ID
  
  # get the gbif key or bail out if there is no match
  sp_key = gbif.key(full_name)
  sp_key = sp_key[1,1]
  
  # get the points using the key
  points = gbif.points(sp_key$usageKey)
  
  # get the native range
  native = check.tdwg(ID)
  
  # clip points to native range
  native_clipped = native.clip(points, TDWGpolys, ID)  
  
  # get EOO and AOO
  EOO_AOO = eoo.aoo(native_clipped)
  
  # pull the results together
  Results = data.frame(
    POWO_ID = ID,
    full_name = full_name,
    #GBIF_SuggestedKey = sp_tax$GBIF_SuggestedKey,
    #kin = sp_tax$kin,
    #phy = sp_tax$phy,
    #ord = sp_tax$ord,
    #fam = sp_tax$fam,
    #GBIF_SuggestedGen = sp_tax$GBIF_SuggestedGen,
    #GBIF_SuggestedSpe = sp_tax$GBIF_SuggestedSpe,
    #GBIF_Suggestedauth = sp_tax$GBIF_Suggestedauth,
    #GBIF_SuggestedNameStatus = sp_tax$GBIF_SuggestedNameStatus,
    #GBIF_AcceptedKey = sp_tax$GBIF_AcceptedKey,
    #Warning = "",
    #TDWGCount = tdwg_count,
    EOO = EOO_AOO$EOO,
    AOO = EOO_AOO$AOO,
    RecordCount = EOO_AOO$RecordCount)
  
  
}





#### 4 - UI
ui <- fluidPage(
  
  # set themes
  theme = shinythemes::shinytheme("paper"),
  
  # Sidebar with a slider input for number of bins 
  navbarPage("Rapid Least Concern",
             tabPanel("1 Search",
                      sidebarLayout(position = "left",
                                    sidebarPanel(
                                      textInput("speciesinput",
                                                "1 Enter species e.g. Aloe zebrina",
                                                placeholder = "Aloe zebrina"),
                                      
                                      #br(),
                                      
                                      #actionButton("goName", "Search"),
                                      
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
                                      actionButton("getPoints", "Draw map"),
                                      br(),
                                      br(),
                                      helpText("Now go to tab '2 CLEAN' to clean the points")
                                      
                                    ),
                                    
                                    # Show the input species
                                    mainPanel(
                                      
                                      # Output: Header + summary of distribution ----
                                      h6("GBIF search results:"),
                                      # search results from GBIF
                                      DT::dataTableOutput("summarytab"),
                                      #DT::dataTableOutput("goNameInput"),
                                      
                                      
                                      br(),
                                      
                                      h6("Plant of the World Online search results:"),
                                      # search results from POWO
                                      DT::dataTableOutput("powotab"),
                                      
                                      br(),
                                      
                                      h6("Distribution map: "),
                                      
                                      leaflet::leafletOutput("mymap", width = "100%", height = 400),
                                      
                                      br(),
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
                                      
                                      #checkboxInput("outliers", "Remove point outliers", FALSE),
                                      #checkboxInput("native", "Clip points to native range", FALSE),
                                      #checkboxInput("gbif", "Clip points to native range", FALSE),
                                      
                                      br(),
                                      
                                      actionButton("cleanPoints", "Clean"),
                                      br(),
                                      br(),
                                      helpText("Now go to tab '3 DOWNLOAD' to save the point file and SIS CSV files")
                                      
                                    ),
                                    
                                    # Show the input species
                                    mainPanel(
                                      
                                      h6("Raw Distribution map: "),
                                      leaflet::leafletOutput("mycleanmap", width = "100%", height = 400),
                                      
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
                                                 
                                                 #checkboxGroupInput("sisoptions", "Download SIS files",
                                                  #                  choices = list("All fields" = 1,
                                                   #                 "Assessments" = 2,
                                                    #                "Countries" = 3,
                                                     #               "Credits" = 4,
                                                      #              "Habitats" = 5,
                                                       #             "Plant specific" = 6,
                                                        #            "Taxonomy" = 7)
                                                                    
                                                # ),
                                                                    
                                                 
                                                 #checkboxInput("allf", "All fields", TRUE),
                                                 #checkboxInput("assessments", "Assessments", TRUE),
                                                 #checkboxInput("occ", "Countries", TRUE),
                                                 #checkboxInput("cred", "Credits", TRUE),
                                                 #checkboxInput("habitatInput", "Habitats", TRUE),
                                                 #checkboxInput("plantsp", "Plant specific", TRUE),
                                                 #checkboxInput("taxcsv", "Taxonomy", TRUE),
                                                
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
                      #helpText("Coming soon - option to process multiple species from a single file "),
                      sidebarPanel(
                        fileInput("file1", "Choose CSV File",
                                multiple = FALSE,
                                accept = (".csv")
                                ),
                        downloadButton('getcleantab', "Download table"),
                        
                        br(),
                        br(),
                        br(),
                        
                        helpText("Click to calculate statistics:"),
                        
                        actionButton('getStats', "Get statistics"),
                        
                        br(),
                        br(),
                        
                        helpText("Click to run batch:"),
                        
                        actionButton('downloadbatch', "Run batch")
                        
                        ),
                        
                        mainPanel(
                          
                          # Output: Data file ----
                          DT::dataTableOutput("contents"),
                          br(),
                          # Output: Data file ----
                          DT::dataTableOutput("stats")
                        )
                      
             ),
                        
                      
                      #fluidRow(
                      #  column(9, DT::dataTableOutput('output$summarytab')),
                      #  column(3, verbatimTextOutput('print_tab'))
                      #)
                      
                      
             
             
             tabPanel("Help",
                      helpText("Coming soon - some help notes here ")
             )
  )
)


### 5 - Server
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
  
  # output for the map on Search page
  output$mymap <- renderLeaflet({
    df <- mapInput()
    sptdwg = check.tdwg(input$powo)
    sptdwg = merge(TDWGpolys, sptdwg)
    
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
  
  ### 2. clean outputs
  
  # output for the map on Cleanpage
  output$mycleanmap <- renderLeaflet({
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
  
  
  ### 3. Download inputs
  
  taxinput = eventReactive(input$key, {
    tax = taxonomy(input$key, input$speciesinput, input$powo)
    #credits$internal_taxon_id = input$datasetInput[1,1]
    tax
  })
  
  
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

      zipdir = paste0(getwd(), "/forzip/")
      #thefiles = list.files(zipdir, full.names = TRUE)
      #thefilesnames = paste0(zipdir, thefiles)     
      thefiles = c("forzip/allfields.csv",
                   "forzip/assessments.csv",
                   "forzip/countries.csv",
                   "forzip/credits.csv",
                   "forzip/habitats.csv",
                   "forzip/plantspecific.csv")
                   #"forzip/taxonomy.csv")
      
      #files2zip <- dir(zipdir, full.names = TRUE)
      zip::zipr('myzip.zip', thefiles)


      #use copy to force the download
      file.copy("myzip.zip", file)

    },
    contentType = "application/zip"
   )
  
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
    applytest_df
  })
  
  
  statsInput <- eventReactive(input$getStats, {
    
    species = batchInput()
    #single = LC_comb(species)
    withProgress(message = 'Getting there...',
                 value = 2, {
                   multi = adply(species, 1, LC_comb)
                 })
    multi
  })
  
  

  ### 4. Batch outputs
  output$contents <- DT::renderDataTable({
  
    df = batchInput()
    df
  
  })
  
  output$stats <- DT::renderDataTable({
    
    df = statsInput()
    df
    
  })
  
  
  output$getcleantab = downloadHandler(
    # download the cleaned gbif point file
    
    filename = function(){
      paste("test_", Sys.Date(), ".csv", sep = "" ) # change this to species name
    },
    content = function(file){
      write.csv(batchInput(), file, row.names = FALSE)
      
    }
  )

  output$downloadRes = downloadHandler(
    # download the cleaned gbif point file
    
    filename = function(){
      paste("test_", Sys.Date(), ".csv", sep = "" ) # change this to species name
    },
    content = function(file){
      # pull out full name and ID
      species = batchInput()
      
      multi = adply(species, 1, LC_comb)
      
      #applydf = lapply(df$name_in,batch.POWO)
      
      #full_name = (df$name)
      #ID = (df$IPNI_ID)
      
      # get the gbif key or bail out if there is no match
      #sp_key = gbif.key(full_name)
      
      
      write.csv(multi, file, row.names = FALSE)
      
    }
  )
  
  
  
  
}  


# Run the application 
shinyApp(ui = ui, server = server)

#rm(list=ls())

