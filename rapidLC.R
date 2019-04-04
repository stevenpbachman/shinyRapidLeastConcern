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
# useful code here: C:\R_local\LATEST\RedLeastApply



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
  
  #full_name = "Steve bachman"
  
  gbif.key = rgbif::name_backbone(
    name = full_name,
    rank = 'species',
    kingdom = 'Plantae',
    strict = FALSE,
    verbose = TRUE #change to TRUE to get more options
  )
  
  # bind together in case there are missing data
  merged = dplyr::bind_rows(gbif.key$alternatives, gbif.key$data)
  
  if (merged$matchType == "HIGHERRANK" && nrow(merged) == 1){

    options = data.frame(
        usageKey = as.integer(""),
        acceptedUsageKey = as.character(""),
        scientificName = as.character(""),
        rank = as.character(""),
        status = as.character(""),
        confidence = as.integer(""),
        family = as.character(""),
        acceptedSpecies = as.character("")
    )
    
  }
  
  else {
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
  
  # reformat to iucn standard
  res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "FOSSIL_SPECIMEN","FossilSpecimen" )
  res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "PRESERVED_SPECIMEN","PreservedSpecimen" )
  res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "LIVING_SPECIMEN", "LivingSpecimen")
  res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "HUMAN_OBSERVATION", "HumanObservation")
  res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "MACHINE_OBSERVATION","MachineObservation")
  res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "UNKNOWN","Unknown")
  
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
    name_in = ""  
    #rank  = "" 
    #synonymOf = ""
    #base_url = ""    
    IPNI_ID = ""
    #search_name = name_in
    #fullname = name_in
    results = data.frame(IPNI_ID, name_in,author, accepted)
    
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
    
    colnames(results)[which(names(results) == "name")] = "name_in"
    
    # take out any results where it matched on something that wasn't species 
    #results = subset(results, rank == "Species") 
    
    # check if 
    if (nrow(results) < 1) {
      # add new column to results and call it warning
      
      accepted = "NA"          
      author = ""  
      #kingdom = ""  
      name_in = ""  
      #rank  = "" 
      #synonymOf = ""
      #base_url = ""    
      IPNI_ID = ""
      #search_name = name_in 
      #fullname = name_in
      results = data.frame(IPNI_ID, name_in, author, accepted)
      
    }
    # make data frame
    results = as.data.frame(results)
    results = results[1,]
  }
  
  return(results)
  
}

# 3.4 get the TDWG native range from POWO
check.tdwg = function(ID){
  
 #ID = "49818-1"

  #full_url = paste0("http://plantsoftheworld.online/api/2/taxon/urn:lsid:ipni.org:names:", ID, "?fields=distribution")
  full_url = paste0("http://plantsoftheworld.online/api/2/taxon/urn:lsid:ipni.org:names:", ID, "?fields=distribution")
  
  # encode
  full_url = utils::URLencode(full_url)
  
  # get raw json data
  raw.data <- readLines(full_url, warn = "F", encoding = "UTF-8")
  
  # organise
  rd = jsonlite::fromJSON(raw.data)
  
  rd = as.data.frame(rd$distribution$natives)
  
  if (!nrow(rd)) {
    rd = data.frame(
      LEVEL3_COD = "NA",
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
    
    #replace Panamá with Panama
    rd$LEVEL3_NAM = gsub("Panamá", "Panama", rd$LEVEL3_NAM)
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
    RedListRationale.value = "This species has a very wide distribution, large population, is not currently experiencing any major threats and no significant future threats have been identified. This species is therefore assessed as Least Concern.",	
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
  write.csv(allfields, allfpath, row.names = FALSE)
  
  assessmentspath = paste0(getwd(), "/forzip/assessments.csv")
  assessmentstable = assessments(species, powo)
  write.csv(assessmentstable, assessmentspath, row.names = FALSE)
  
  occspath = paste0(getwd(), "/forzip/countries.csv")
  occstable = countries(powo)
  write.csv(occstable, occspath, row.names = FALSE)
  
  credpath = paste0(getwd(), "/forzip/credits.csv")
  credits = credits(name, email, affiliation, species, powo)
  write.csv(credits, credpath, row.names = FALSE)
  
  habitatpath = paste0(getwd(), "/forzip/habitats.csv")
  hab = habitats(habitat, species, powo)
  write.csv(hab, habitatpath, row.names = FALSE)
  
  plantspath = paste0(getwd(), "/forzip/plantspecific.csv")
  plantspecific = plantspecific(growthform, species, powo)
  write.csv(plantspecific, plantspath, row.names = FALSE)
  
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
  AOOkm2 <- AOOnocells * (cellsizem/1000)^2
  
  eoo.aoo.res = data.frame(
    EOO = round(EOOkm2abs,0),
    #AOO = AOOnocells,
    AOO = AOOkm2,
    RecordCount = rec_count)
  
  return(eoo.aoo.res)
}

# 3.15 combine functions to get LC results - use apply on this
LC_comb = function(species) {
  #full_name = "Hypoestes acuminata"
  #ID = "49818-1"
  
  full_name = species$name
  ID = species$IPNI_ID
  
  # get the gbif key or bail out if there is no match
  sp_key = gbif.key(full_name)
  
  if (sp_key$usageKey[1] == "NA") {
    Results = data.frame(
      EOO = NA,
      AOO = NA,
      RecordCount = NA,
      TDWGCount = NA,
      POWO_ID = ID,
      full_name = full_name,
      Warning = "No name match in GBIF"
      
    )
  }
  
  else {
    sp_key = sp_key[1, 1]
    
    # get the points using the key
    points = gbif.points(sp_key$usageKey)
    
    # count georeferenced occurrences
    points_count = nrow(points)
    
    # check if there are no gbif points
    if (nrow(points) == 0) {
      Results = data.frame(
        EOO = NA,
        AOO = NA,
        RecordCount = NA,
        TDWGCount = NA,
        POWO_ID = ID,
        full_name = full_name,
        Warning = "No GBIF points"
        
      )
    }
    
    else {
      # get the native range
      native = check.tdwg(ID)
      
      if (native$tdwgLevel[1] == "NA") {
        Results = data.frame(
          EOO = NA,
          AOO = NA,
          RecordCount = NA,
          TDWGCount = NA,
          POWO_ID = ID,
          full_name = full_name,
          Warning = "No TDWG distribution data"
        )
        
      }
      
      else {
        # clip points to native range
        native_clipped = native.clip(points, TDWGpolys, ID)
        
        if (nrow(native_clipped) < 1) {
          Results = data.frame(
            EOO = NA,
            AOO = NA,
            RecordCount = NA,
            TDWGCount = NA,
            POWO_ID = ID,
            full_name = full_name,
            Warning = "No GBIF points in native range"
            
          )
        }
        
        else {
          # get EOO and AOO
          EOO_AOO = eoo.aoo(native_clipped)
          
          # pull the results together
          Results = data.frame(
            EOO = EOO_AOO$EOO,
            AOO = EOO_AOO$AOO,
            RecordCount = EOO_AOO$RecordCount,
            TDWGCount = nrow(native),
            POWO_ID = ID,
            full_name = full_name,
            Warning = ""
          )
          return(Results)
        }
      }
    }
  } 
}

# 3.16 combine functions to get LC results - use apply on this
all_batch_points = function(species){
  
  #full_name = "Calamus aruensis"
  #ID = "664971-1"
  
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
  res = native.clip(points, TDWGpolys, ID)  
  res$BINOMIAL = full_name
  
  res = subset(
    res,
    select = c(
      'POWO_ID',
      'BasisOfRec',
      'DEC_LONG',
      'DEC_LAT',
      'BINOMIAL',
      'EVENT_YEAR',
      'CATALOG_NO',
      'SPATIALREF',
      'PRESENCE',
      'ORIGIN',
      'SEASONAL',
      'YEAR',
      'DATA_SENS',
      'SOURCE',
      'COMPILER',
      'CITATION')
  )
  
  colnames(res)[which(names(res) == "POWO_ID")] = "internal_taxon_id"
  
  # reformat basis of record column
  res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "FOSSIL_SPECIMEN","FossilSpecimen" )
  res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "PRESERVED_SPECIMEN","PreservedSpecimen" )
  res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "LIVING_SPECIMEN", "LivingSpecimen")
  res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "HUMAN_OBSERVATION", "HumanObservation")
  res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "MACHINE_OBSERVATION","MachineObservation")
  res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "UNKNOWN","Unknown")

  return(res)
  
  }
  
# 3.17 batch allfields
batch_allfields = function(species){

    ID = species$IPNI_ID
    EOOval = species$EOO

    #check = check.accepted.POWO(species)
    #ID = check$IPNI_ID  
    allf = data.frame(
      internal_taxon_id = ID,	
      CurrentTrendDataDerivation.value = "Suspected",
      nothreats.nothreats = "TRUE",
      threatsunknown.value = "FALSE",
      EOO.range = EOOval)
    
    return(allf)
}

# 3.18 batch assessments
batch_assessments = function(species) {
  ID = species$IPNI_ID
  
  sysdate = base::Sys.Date()
  day = base::substr(sysdate, 9, 10)
  month = base::substr(sysdate, 6, 7)
  year = base::substr(sysdate, 1, 4)
  as = data.frame(
    internal_taxon_id = ID,
    RedListRationale.value = "This species has a very wide distribution, large population, is not currently experiencing any major threats and no significant future threats have been identified. This species is therefore assessed as Least Concern.",
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
    biogeographicrealm.realm = ""
  )
  
  return(as)
}

# 3.19
batch_credits = function(species) {
  
  ID = species$IPNI_ID
  
  #check = check.accepted.POWO(species)
  #ID = check$IPNI_ID
  credits = data.frame(
    internal_taxon_id = ID,
    credit_type = "Assessor",
    firstName = "Your first name",   #stringr::word(name,1),
    lastName = "Your last name", #stringr::word(name,2),
    initials = "",
    Order = "1",
    email = "Your email", #email,
    affiliation = "Your affiliation", #affiliation,
    user_id = "1"
  )
  return(credits)
}

# 3.20
batch_habitats = function(species) {
  #hab = data.frame(description = habitatinput)
  #hab = merge(hab,habitatlist, by = "description")
  ID = species$IPNI_ID
  
  hab = data.frame(
    internal_taxon_id = 	ID,
    GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsName = "",
    GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsLookup = "",
    GeneralHabitats.GeneralHabitatsSubfield.suitability = "Suitable",
    GeneralHabitats.GeneralHabitatsSubfield.majorImportance	= "",
    GeneralHabitats.GeneralHabitatsSubfield.season = ""
  )
  return(hab)
}

# 3.21
batch_plantspecific = function(species) {
  
  ID = species$IPNI_ID
  
  gf = data.frame(
  internal_taxon_id = ID,
  PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup = "",
  PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName = ""
  )

  return(gf)
}

# 3.22
batch_taxonomy = function(species){
  
  full_name = species$name
  
  
  
  sp_key = gbif.key(full_name)
  sp_key = sp_key[1,1]
  
  nameinfo = rgbif::name_usage(sp_key)
  
  #powo = check.accepted.POWO(species)
  #powo2 = subset(powo, subset = powo$IPNI_ID == ID)
  
  ID = species$IPNI_ID
  
  tax = data.frame(
    internal_taxon_id = ID,	
    #kingdom	= "PLANTAE",
    #phylum = "",
    #classname = "",
    #ordername = "",
    family =  nameinfo$data$family,
    genus = nameinfo$data$genus,
    species = word(nameinfo$data$species,2),  
    taxonomicAuthority = species$author
    )
  
  #now merge with iucn taxonomy to get higher tax
  #colnames(taxonomy_iucn)[which(names(taxonomy_iucn) == "fam")] = "family"
  taxmerged = merge(tax, taxonomy_iucn, by = "family")
  taxmerged = taxmerged[c(2,6:9,1,3:5)]
  
  return(taxmerged)
}

# 3.23
batch_countries = function(species){
  
  ID = species$IPNI_ID
  
  #internal_taxon_id = ID  
  range = check.tdwg(ID) 
  # merge with IUCN country file
  colnames(TDWG_to_IUCN_version3_UTF)[which(names(TDWG_to_IUCN_version3_UTF) == "Level.3.code")] = "LEVEL3_COD"
  merged_range = merge(range, TDWG_to_IUCN_version3_UTF, by = "LEVEL3_COD")
  country_tab = merged_range
  country_tab$CountryOccurrence.CountryOccurrenceSubfield.presence = "Extant"
  country_tab$CountryOccurrence.CountryOccurrenceSubfield.origin = "Native"
  country_tab$CountryOccurrence.CountryOccurrenceSubfield.seasonaility = "Resident"
  country_tab$internal_taxon_id = ID
  country_tab
  
  country_tab = country_tab[c(19,10,9,16:18)]
  
  #countries = data.frame(internal_taxon_id = ID, country_tab)
  #countries = countries[c(2,6:9,1,3:5)]
  
}

# 3.24 biorealms
# to be added
#LC.biorealms = function(TDWG_realms, result.tdwg) {
#  #add realms using TDWG_realms table - first change column name to get match with result.tdwg
#  colnames(TDWG_realms) [which(names(TDWG_realms) == "LEVEL3_COD")] = "tdwgCode"
#  #merge result.tdwg and TDWG_realms so we have realms linked to POWO ID
#  realms = merge(TDWG_realms, result.tdwg, by.x = "tdwgCode", by.y = "tdwgCode")
#  # summarise and collapse to get single column with all unique realms - nice!
#  biorealm_summary = realms %>% group_by(POWO_ID) %>% summarise(newREALM = paste(unique(REALM), collapse =
#                                                                                   ","))
#  make_biorealms = data.frame(POWO_ID = biorealm_summary$POWO_ID,
#                              biogeographicrealm.realm = (gsub("," , "\\|", biorealm_summary$newREALM)))
#  return(make_biorealms)
#  
#}

# 3.25
# to be added
# #LC.references = function(LC.results){
#   #powoid = trees_7079[["ID"]]
#   #powoid = powoid[1:10]
#   powoid = LC.results[["POWO_ID"]]
#   
#   type = c("electronic source","electronic source","electronic source" )
#   author = c("Board of Trustees, RBG Kew","Moat, J.", "Chamberlain, S")
#   year = c("2018","2017", "2017")
#   title = c("Plants of the World Online Portal", "rCAT: Conservation Assessment Tools. R package version 0.1.5.","rgbif: Interface to the Global 'Biodiversity' Information Facility API. R package version
#             0.9.9.")
#   place_published = c("Richmond, UK", "", "")
#   url = c("http://powo.science.kew.org/","https://CRAN.R-project.org/package=rCAT", "https://CRAN.R-project.org/package=rgbif")
#   reference_type = c("Assessment","Assessment", "Assessment")
#   references = data.frame(type, author, year, title, place_published, url, reference_type)
#   references$internal_taxon_id = powoid[1] 
#   
#   # make loop to create table with refs below and add powoid to each table, then bind together
#   list = powoid[-1]
#   
#   for (l in list){
#     type = c("electronic source","electronic source","electronic source" )
#     author = c("Board of Trustees, RBG Kew","Moat, J.", "Chamberlain, S")
#     year = c("2018","2017", "2017")
#     title = c("Plants of the World Online Portal", 
#               "rCAT: Conservation Assessment Tools. R package version 0.1.5.",
#               "rgbif: Interface to the Global 'Biodiversity' Information Facility API. R package version
#               0.9.9.")
#     place_published = c("Richmond, UK", "", "")
#     url = c("http://powo.science.kew.org/","https://CRAN.R-project.org/package=rCAT", "https://CRAN.R-project.org/package=rgbif")
#     reference_type = c("Assessment","Assessment", "Assessment")
#     all.references = data.frame(type, author, year, title, place_published, url, reference_type)
#     all.references$internal_taxon_id = l 
#     references = rbind(references,all.references)
#   }
#   
#   return(references)
# }

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
                        
                      
                      #fluidRow(
                      #  column(9, DT::dataTableOutput('output$summarytab')),
                      #  column(3, verbatimTextOutput('print_tab'))
                      #)
                      
                      
             
             
             tabPanel("Help",
                      includeMarkdown("README.md")
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
  
  #######################################################
  
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
  
  #dt = multi
  #dt = subset(dt, EOO >= 100)
  

  #if (nrow(df) == 1)

  ####################
  ####################
  
  output$downloadbatch = downloadHandler(
    
     # download the results
    filename = function(){
      paste("batch_SIS_connect_", Sys.Date(), ".zip", sep = "" ) # change this to species name
    },
    content = function(file){

      dt = statsInput()
      
      # first get the full results out - this will include errors
      dtpath = paste0(getwd(), "/batchzip/results.csv")
      dttable = dt
      write.csv(dttable, dtpath, row.names = FALSE)
      
      drop_cols = "Warning"
      dt  = dt [ , !(names(dt ) %in% drop_cols)]
 
      # now you have to add the filters again, otherwise you get the full table
      dt = subset(dt, EOO >= eooValue())
      dt = subset(dt, AOO >= aooValue())
      dt = subset(dt, RecordCount >= recordsValue())
      dt = subset(dt, TDWGCount >= tdwgValue())
      
      # get the points
      multipoints = adply(dt, 1, all_batch_points) # run through each species
      multipoints = multipoints[ -c(1:10)] #drop first 11 columns
      pointspath = paste0(getwd(), "/batchzip/points.csv") # save path
      write.csv(multipoints, pointspath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # now the csv files
      # allfields
      batch_allfields = adply(dt, 1, batch_allfields)
      batch_allfields = batch_allfields[ -c(1:10)]
      batch_allfieldspath = paste0(getwd(), "/batchzip/allfields.csv")
      write.csv(batch_allfields, batch_allfieldspath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # assessments
      batch_assessments = adply(dt, 1, batch_assessments)
      batch_assessments = batch_assessments[ -c(1:10)]
      batch_assessmentspath = paste0(getwd(), "/batchzip/assessments.csv")
      write.csv(batch_assessments, batch_assessmentspath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # credits
      batch_credits = adply(dt, 1, batch_credits)
      batch_credits = batch_credits[ -c(1:10)]
      batch_creditspath = paste0(getwd(), "/batchzip/credits.csv")
      write.csv(batch_credits, batch_creditspath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # habitats
      batch_habitats = adply(dt, 1, batch_habitats)
      batch_habitats = batch_habitats[ -c(1:10)]
      batch_habitatspath = paste0(getwd(), "/batchzip/habitats.csv")
      write.csv(batch_habitats, batch_habitatspath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # plantspecific
      batch_plantspecific = adply(dt, 1, batch_plantspecific)
      batch_plantspecific = batch_plantspecific[ -c(1:10)]
      batch_plantspecificpath = paste0(getwd(), "/batchzip/plantspecific.csv")
      write.csv(batch_plantspecific, batch_plantspecificpath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # plantspecific
      batch_taxonomy = adply(dt, 1, batch_taxonomy)
      batch_taxonomy = batch_taxonomy[ -c(1:10)]
      batch_taxonomypath = paste0(getwd(), "/batchzip/taxonomy.csv")
      write.csv(batch_taxonomy, batch_taxonomypath, row.names = FALSE) # write it - row.names = FALSE! 
      
      # countries
      batch_countries = adply(dt, 1, batch_countries)
      batch_countries = batch_countries[ -c(1:10)]
      batch_countriespath = paste0(getwd(), "/batchzip/countries.csv")
      write.csv(batch_countries, batch_countriespath, row.names = FALSE) # write it - row.names = FALSE! 
    
      
      #write.csv(dt, file, row.names = FALSE)
      batchzipdir = paste0(getwd(), "/batchzip/")
      
      thefiles = c("batchzip/allfields.csv",
                   "batchzip/assessments.csv",
                   "batchzip/countries.csv",
                   "batchzip/credits.csv",
                   "batchzip/habitats.csv",
                   "batchzip/plantspecific.csv",
                   "batchzip/taxonomy.csv",
                   "batchzip/results.csv",
                   "batchzip/points.csv")

        # get all files in the directory
      
      zip::zipr('batchzip.zip', thefiles)
        
      # use copy to force the download
      file.copy("batchzip.zip", file)
      
      
    },
    contentType = "application/zip"
    

  )
  
    
  
  
}  


# Run the application 
shinyApp(ui = ui, server = server)

#rm(list=ls())

