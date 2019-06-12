
#### 2 - shapefiles/rasters/other files---------------
TDWGpolys = sf::read_sf(here("data","level3/level3.shp"))
#TDWGpolys = rgdal::readOGR("level3/level3.shp")
raster.tdwg = raster::raster(here("data","rasters/tdwg3.tiff"))
tdwg_raster <- read.csv(here("data","tdwg_raster.csv"))
plantgflist <- read.csv(here("data","plantgrowthformslookup.csv"), encoding="UTF-16")
habitatlist <- read.csv(here("data","habitatslookup.csv"), encoding="UTF-16")
taxonomy_iucn <- read.csv(here("data","taxonomy_iucn_lookup.csv"), encoding="UTF-16")
TDWG_to_IUCN_version3_UTF <- read.delim(here("data","TDWG_to_IUCN.txt"), encoding="UTF-16", na.strings="")



#### 3 - functions-------------
#test = gbif.key("Aloe zebrina")
#testpoints = gbif.points("2777656")

# 3.1 get the gbif key
gbif.key = function (full_name) {
  
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
    
    
    res = res$data
    res = as.data.frame(res)
    
    if (nrow(res) == 0) {
      
    } else {
      
      res$taxonKey = key
      
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
      res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "HUMAN_OBSERVATION", "HumanObservation")
      res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "LITERATURE", "")
      res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "LIVING_SPECIMEN", "LivingSpecimen")
      res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "MACHINE_OBSERVATION","MachineObservation")
      res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "OBSERVATION","" )
      res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "PRESERVED_SPECIMEN","PreservedSpecimen" )
      res$BasisOfRec = stringr::str_replace_all(res$BasisOfRec, "UNKNOWN","Unknown")
      
      # remove fossils? Literature and Unknown are not recognised in IUCN standards.
    }
  }
  return(res)
} 

# 3.3 check name against POWO
# takes binomial and checks against POWO (http://www.plantsoftheworldonline.org)
check.accepted.POWO = function(name_in) {
  
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
    fullname = ""
    results = data.frame(IPNI_ID, fullname, name_in,author, accepted)
    
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
    
    # when one or more name search results are not accepted - do name match 
    results = results[results$name_in %in%  name_in, ]   
    
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
      fullname = ""
      results = data.frame(IPNI_ID, fullname, name_in, author, accepted)
      
      
    }
    # make data frame
    results = as.data.frame(results)
    results = unite(results, fullname, name_in, author, sep = " ", remove = F )
    results = results[1,]
  }
  
  return(results)
  
}

# 3.4 get the TDWG native range from POWO
check.tdwg = function(ID){
  
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
  
  # prepare the point data as spatial
  decimalLongitude = points$DEC_LONG
  decimalLatitude = points$DEC_LAT
  coords = cbind(decimalLongitude, decimalLatitude)
  coords = base::data.matrix(coords)
  coords = base::unique(coords)
  sp = sp::SpatialPoints(coords)
  
  ## alternative vector solution
  #proj4string(sp) <- proj4string(TDWGpolys)
  #extracted = over(sp, TDWGpolys)
  #extract.out = cbind(extracted, coords)
  #clean_out = na.omit(extract.out)
  ## change colnames 
  #colnames(clean_out)[which(names(clean_out) == "decimalLongitude")] = "DEC_LONG"
  #colnames(clean_out)[which(names(clean_out) == "decimalLatitude")] = "DEC_LAT"
  ## select the subset you want 1.2.5.6
  #tdwg.merge.out = clean_out[c("LEVEL3_NAM","LEVEL3_COD","DEC_LONG", "DEC_LAT")]
  
  ##powo = "314824-1"
  #point.clean.bind = merge(points, tdwg.merge.out, by = c("DEC_LONG", "DEC_LAT"), all = TRUE)
  #sptdwg = check.tdwg(powo)
  #native_points = merge(point.clean.bind, sptdwg)
  #native_points
  
  
  # extract values from TDWG raster
  extract.out = raster::extract(raster.tdwg, sp, df = TRUE)
  clean_out = na.omit(extract.out)
  
  if (nrow(clean_out) == 0) {
    native_points = data.frame(blankdf = character())
  }
  
  else {  
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
}

# 3.6 SIS files allfields.csv
allfields = function(species, ID){
  
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
  
  # now get rid of the duplicates to get a clean list
  merged_range = merged_range[!duplicated(merged_range$countryoccurrence.countryoccurrencesubfield.countryoccurrencename), ]
  
  country_tab = merged_range[c(6,10,9)]
  country_tab$CountryOccurrence.CountryOccurrenceSubfield.presence = "Extant"
  country_tab$CountryOccurrence.CountryOccurrenceSubfield.origin = "Native"
  country_tab$CountryOccurrence.CountryOccurrenceSubfield.seasonaility = "Resident"
  country_tab
}

# 3.9 SIS files - credits.csv
credits = function(name,email,affiliation,species, ID) {
  
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

##############
#key = "5294468"
#species = "Aloe zebrina"
#ID = "530052-1"
################


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
  
  do.call(file.remove, list(list.files(here("data/singlezip/"), full.names = TRUE)))
  
  
  allfpath = here("data/singlezip/allfields.csv")
  allfields = allfields(species, powo)
  write.csv(allfields, allfpath, row.names = FALSE)
  
  assessmentspath = here("data/singlezip/assessments.csv")
  assessmentstable = assessments(species, powo)
  write.csv(assessmentstable, assessmentspath, row.names = FALSE)
  
  occspath = here("data/singlezip/countries.csv")
  occstable = countries(powo)
  write.csv(occstable, occspath, row.names = FALSE)
  
  credpath = here("data/singlezip/credits.csv")
  credits = credits(name, email, affiliation, species, powo)
  write.csv(credits, credpath, row.names = FALSE)
  
  habitatpath = here("data/singlezip/habitats.csv")
  hab = habitats(habitat, species, powo)
  write.csv(hab, habitatpath, row.names = FALSE)
  
  plantspath = here("data/singlezip/plantspecific.csv")
  plantspecific = plantspecific(growthform, species, powo)
  write.csv(plantspecific, plantspath, row.names = FALSE)
  
  taxpath = here("data/singlezip/taxonomy.csv")
  ##taxtable =  taxinput()
  taxtable = taxonomy(key, species, powo)
  write.csv(taxtable, taxpath, row.names = FALSE)
  
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


#######################
#full_name = "Arctium intermedium"
#ID = "11039-2"
#name_in = full_name#

#test = batch.POWO(full_name)

#testkey = gbif.key(full_name)



#applytest = lapply(bermuda_1_10$name_in,batch.POWO)
#result_batch = do.call(bind_rows, applytest)

#batch_list = result_batch[1:3,2]

#batch_name_check = lapply(batch_list, gbif.key)
#result_batch = do.call(bind_rows, batch_name_check)
###############

# 3.15 combine functions to get LC results - use apply on this
LC_comb = function(species) {
  
  full_name = species$fullname
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
  
  #################
  # add in this section when GBIF thinks the species is not accepted.
  else {
    
    search.df = c(
      "PROPARTE_SYNONYM",
      "DOUBTFUL",
      "HETEROTYPIC_SYNONYM",
      "HOMOTYPIC_SYNONYM",
      "MISAPPLIED",
      "SYNONYM"
    )
    
    if(sp_key$status %in%  search.df){
      
      Results = data.frame(
        EOO = NA,
        AOO = NA,
        RecordCount = NA,
        TDWGCount = NA,
        POWO_ID = ID,
        full_name = full_name,
        Warning = "Best name match against GBIF is not treated by GBIF as accepted")
      
    }
  
  
  #############
  
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
        
        str(native_clipped)
        
        # nrow(native_clipped
        if (length(native_clipped) < 1 | nrow(native_clipped) == 0) {
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
}

# 3.16 combine functions to get LC results - use apply on this
all_batch_points = function(species){
  
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
  
  if (nrow(taxmerged) <1) {
    taxmerged = data.frame(
      internal_taxon_id = ID,	
      kingdom	= "PLANTAE",
      phylum = "UNMATCHED",
      classname = "UNMATCHED",
      ordername = "UNMATCHED",
      family =  nameinfo$data$family,
      genus = nameinfo$data$genus,
      species = word(nameinfo$data$species,2),  
      taxonomicAuthority = species$author)
  }
  
  else {
    
    taxmerged = taxmerged[c(2,6:9,1,3:5)]
    
    return(taxmerged)
  }
  
}

# 3.23
batch_countries = function(species){
  
  #ID = "1078218-2"
  
  ID = species$IPNI_ID
  
  #internal_taxon_id = ID  
  range = check.tdwg(ID) 
  # merge with IUCN country file
  colnames(TDWG_to_IUCN_version3_UTF)[which(names(TDWG_to_IUCN_version3_UTF) == "Level.3.code")] = "LEVEL3_COD"
  merged_range = merge(range, TDWG_to_IUCN_version3_UTF, by = "LEVEL3_COD")
  country_tab = merged_range
  
  # now get rid of the duplicates to get a clean list
  merged_range = merged_range[!duplicated(merged_range$countryoccurrence.countryoccurrencesubfield.countryoccurrencename), ]
  
  country_tab$CountryOccurrence.CountryOccurrenceSubfield.presence = "Extant"
  country_tab$CountryOccurrence.CountryOccurrenceSubfield.origin = "Native"
  country_tab$CountryOccurrence.CountryOccurrenceSubfield.seasonaility = "Resident"
  country_tab$internal_taxon_id = ID
  country_tab
  
  country_tab = country_tab[c(6,9:13)]
  
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
