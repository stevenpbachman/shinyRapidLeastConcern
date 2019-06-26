
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
  
  options = data.frame(
    usageKey = NA_integer_,
    acceptedUsageKey = NA_character_,
    scientificName = NA_character_,
    rank = NA_character_,
    status = NA_character_,
    confidence = NA_integer_,
    family = NA_character_,
    acceptedSpecies = NA_character_
  )
  
  gbif_results = name_backbone(
    name = full_name,
    rank = 'species',
    kingdom = 'Plantae',
    strict = FALSE,
    verbose = TRUE #change to TRUE to get more options
  )
  
  # bind together in case there are missing data
  merged = bind_rows(gbif_results$alternatives, gbif_results$data)
  
  if (nrow(merged) > 1 | merged$matchType[1] != "HIGHERRANK") {
    # change col names
    merged = rename(merged, acceptedSpecies=species)
    
    if (!"acceptedUsageKey" %in% colnames(merged)) {
      merged$acceptedUsageKey = NA_character_
    }
    
    # subset the data with the fields you want
    options = select(merged, colnames(options))
  
    # arrange table in descending order to show best guess at top of table
    options = arrange(options, desc(confidence))
  }
  
  options
}

# 3.2 fetch the points using key
gbif.points = function(key) {
  res = tibble(
    BasisOfRec = NA_character_,
    BINOMIAL = NA_character_,
    DEC_LONG = -999,
    DEC_LAT = -999,
    EVENT_YEAR = -999L,
    CATALOG_NO = NA_character_,
    SPATIALREF = "WGS84",
    PRESENCE = "1",
    ORIGIN = "1",
    SEASONAL = "1",
    DATA_SENS = "No",
    SOURCE = NA_character_,
    YEAR = -999L,
    COMPILER = NA_character_,
    CITATION = NA_character_,
    recordedBy = NA_character_,
    recordNumber = NA_character_,
    issues = NA_character_,
    datasetKey = NA_character_
  )

  if (key == "") {
    return(res)
  }
  
  gbif_results = occ_data(
    taxonKey = key,
    hasGeospatialIssue = FALSE,
    hasCoordinate = TRUE,
    limit = 1000
  )
  
  gbif_points = gbif_results$data
  
  if (nrow(gbif_points) > 0) {
    
    gbif_points = rename(gbif_points,
      BasisOfRec=basisOfRecord,
      DEC_LAT=decimalLatitude,
      DEC_LONG=decimalLongitude,
      BINOMIAL=scientificName,
      EVENT_YEAR=year,
      CATALOG_NO=catalogNumber
    )
    
    columns_to_add = setdiff(colnames(res), colnames(gbif_points))
    default_data = as.list(res)
    gbif_points = tibble::add_column(gbif_points, !!! default_data[columns_to_add])
    
    gbif_points$YEAR = substring(Sys.Date(), 1, 4)
    gbif_points$SOURCE = paste0("https://www.gbif.org/dataset/", gbif_points$datasetKey, sep = "")
    
    # reformat to iucn standard
    gbif_points = mutate(gbif_points,
                          BasisOfRec=recode(BasisOfRec,
                            "FOSSIL_SPECIMEN"="FossilSpecimen",
                            "HUMAN_OBSERVATION"="HumanObservation",
                            "LITERATURE"="",
                            "LIVING_SPECIMEN"="LivingSpecimen",
                            "MACHINE_OBSERVATION"="MachineObservation",
                            "OBSERVATION"="",
                            "PRESERVED_SPECIMEN"="PreservedSpecimen",
                            "UNKNOWN"="Unknown",
                          ))
    
    res = select(gbif_points, colnames(res))
  }
  
  return(res)
}

# 3.3 check name against POWO
# takes binomial and checks against POWO (http://www.plantsoftheworldonline.org)
check.accepted.POWO = function(name_in) {

  powo_results <- tibble(
    IPNI_ID=NA_character_,
    name=NA_character_,
    author=NA_character_,
    accepted=NA
  )

  # use name full name to search API  
  full_url =  paste("http://plantsoftheworldonline.org/api/1/search?q=names:", name_in, sep = "")
  
  # encode
  full_url = utils::URLencode(full_url)
  
  # get raw json data
  raw_data <- readLines(full_url, warn = "F", encoding = "UTF-8")
  
  # organise
  rd = fromJSON(raw_data)
  
  if (length(rd$results) > 0) {

    # make data frame
    results = rd$results
    
    # get IPNI ID
    results = mutate(results, IPNI_ID=str_extract(url, "(?<=names\\:)[\\d\\-]+$"))

    # only include these fields - you don't want synonym of
    powo_results = select(results, colnames(powo_results))
  }
  
  return(powo_results)
  
}

# 3.3b check name against POWO
# takes binomial and checks against POWO (http://www.plantsoftheworldonline.org)

batch.POWO = function(name_in) {
  # lookup name in POWO
  powo_results = check.accepted.POWO(name_in)
  # remove anything that wasn't the name we wanted
  powo_results = filter(powo_results, name == name_in)
  
  powo_results = rename(powo_results, name_in=name)

  powo_results = unite(powo_results, fullname, name_in, author, sep=" ", remove=FALSE)
  # might error if all different names returned
  powo_results = powo_results[1,]
  
  return(powo_results)
}

# 3.4 get the TDWG native range from POWO
check.tdwg = function(ID){
  results = tibble(
      LEVEL3_COD=NA_character_,
      featureId=NA_character_,
      tdwgLevel=NA_integer_,
      establishment=NA_character_,
      LEVEL3_NAM=NA_character_,
      POWO_ID=NA_character_
  )

  full_url = paste0("http://plantsoftheworld.online/api/2/taxon/urn:lsid:ipni.org:names:", ID, "?fields=distribution")
  
  # encode
  full_url = utils::URLencode(full_url)
  
  # get raw json data
  raw_data = readLines(full_url, warn = "F", encoding = "UTF-8")
  
  # organise
  rd = fromJSON(raw_data)
  
  distribution = rd$distribution$natives
  
  if (! is.null(distribution)) {
    results = mutate(distribution, POWO_ID=ID)
    results = rename(results, LEVEL3_NAM=name, LEVEL3_COD=tdwgCode)
    results = mutate(results, LEVEL3_NAM=recode(LEVEL3_NAM, "á"="a"))
  }

  return(results)

deduplicate_by <- function(.data, ...) {
  group_vars <- enquos(...)
  .data %>%
    group_by(!!! group_vars) %>%
    filter(row_number() == 1) %>%
    ungroup()
}

deduplicate_by <- function(.data, ...) {
  group_vars <- enquos(...)
  .data %>%
    group_by(!!! group_vars) %>%
    filter(row_number() == 1) %>%
    ungroup()
}

# 3.5 native range clip
native.clip = function(points, TDWGpolys, powo){
  # TODO: maybe replace/add option for raster solution if more points provided
  # prepare the point data as spatial
  points <- deduplicate_by(points, DEC_LONG, DEC_LAT)
  point_sf <- st_as_sf(points, 
                       coords=c("DEC_LONG", "DEC_LAT"),
                       crs=st_crs(TDWGpolys), 
                       remove=FALSE)
  # get shapes of native range
  native_distribution <- check.tdwg(powo)
  native_tdwg <- filter(TDWGpolys, LEVEL3_COD %in% native_distribution$LEVEL3_COD)
  # clip points to native range with a spatial join
  native_points <- st_join(point_sf, native_tdwg)
  native_points <- filter(native_points, ! is.na(LEVEL3_COD))
  # convert back to normal data frame from sf
  native_points <- as_tibble(native_points)
  native_points <- select(native_points, -geometry)
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
  
  rationale_str = paste("This species has a very wide distribution,", 
                        "large population,", 
                        "is not currently experiencing any major threats", 
                        "and no significant future threats have been identified.", 
                        "This species is therefore assessed as Least Concern.")
  
  as = data.frame(
    internal_taxon_id = ID,	
    RedListRationale.value = rationale_str,	
    mapstatus.status = "Done",	
    RedListAssessmentDate.value = format(Sys.Date(), "%d/%m/%Y"),	
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

  range = check.tdwg(ID) 
  # merge with IUCN country file
  merged_range = left_join(range, TDWG_to_IUCN_version3_UTF, by =c("LEVEL3_COD"="Level.3.code"))
  
  # now get rid of the duplicates to get a clean list
  merged_range = deduplicate_by(merged_range, countryoccurrence.countryoccurrencesubfield.countryoccurrencename)
  
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
    firstName = word(name,1),
    lastName = word(name,2),
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
  
  hab = data.frame(description=habitatinput)
  
  hab = merge(hab, habitatlist, by="description")
  
  hab$internal_taxon_id = ID
  hab = rename(hab, 
               GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsName=description,
               GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsLookup=code)
  
  hab$GeneralHabitats.GeneralHabitatsSubfield.suitability = "Suitable"
  hab$GeneralHabitats.GeneralHabitatsSubfield.majorImportance	= ""
  hab$GeneralHabitats.GeneralHabitatsSubfield.season = ""	
  return(hab)
}

# 3.11 SIS files - plantspecific.csv
plantspecific = function(gfinput, species, ID) {
  
  gf = data.frame(description=gfinput)
  gf = merge(gf, plantgflist, by="description")
  gf$internal_taxon_id = ID
  
  gf = rename(gf,
              PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName=description,
              PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup=code)
  
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
  nameinfo = name_usage(key)
  powo = check.accepted.POWO(species)
  powo = filter(powo, IPNI_ID == ID)
  
  tax = data.frame(
    internal_taxon_id = ID,	
    family = nameinfo$data$family,
    genus = nameinfo$data$genus,
    species = word(nameinfo$data$species,2),  
    taxonomicAuthority = powo$author)
  
  #now merge with iucn taxonomy to get higher tax
  #colnames(taxonomy_iucn)[which(names(taxonomy_iucn) == "fam")] = "family"
  taxmerged = merge(tax, taxonomy_iucn, by="family")
  taxmerged = taxmerged[c(2, 6:8, 1, 3:5)]
  
  return(taxmerged)
}

# 3.13 save all SIS files
all_SIS = function(species, powo, name, email, affiliation, habitat, growthform, key){
  zip_folder = here("data/singlezip")
  
  if (file.exists(zip_folder)) {
    unlink(zip_folder, recursive=TRUE)
  }
  
  dir.create(zip_folder)
  
  allfields = allfields(species, powo)
  write_csv(allfields, here(zip_folder, "allfields.csv"))
  
  assessmentstable = assessments(species, powo)
  write_csv(assessmentstable, here(zip_folder, "assessments.csv"))
  
  occstable = countries(powo)
  write_csv(occstable, here(zip_folder, "countries.csv"))
  
  credits = credits(name, email, affiliation, species, powo)
  write_csv(credits, here(zip_folder, "credits.csv"))
  
  hab = habitats(habitat, species, powo)
  write_csv(hab, here(zip_folder, "habitats.csv"))
  
  plantspecific = plantspecific(growthform, species, powo)
  write_csv(plantspecific, here(zip_folder, "plantspecific.csv"))
  
  taxtable = taxonomy(key, species, powo)
  write_csv(taxtable, here(zip_folder, "taxonomy.csv"))
  
}

# 3.14 EOO and AOO
eoo.aoo = function(native) {
  
  mypointsll = data.frame(lat=native$DEC_LAT, long=native$DEC_LONG)
  centreofpoints <- trueCOGll(mypointsll)
  mypointsxy <- simProjWiz(mypointsll, centreofpoints)
  
  #EOO and AOO calculation
  EOOm2 <- EOOarea(mypointsxy)
  EOOkm2 <- EOOm2 / 1000000
  EOOkm2abs = abs(EOOkm2)
  rec_count = nrow(mypointsxy)
  cellsizem <- 10000
  AOOnocells <- AOOsimp(mypointsxy, cellsizem)
  AOOkm2 <- AOOnocells * (cellsizem / 1000)^2
  
  eoo.aoo.res = data.frame(
    EOO = round(EOOkm2abs, 0),
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
LC_comb = function(full_name, ID) {
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

    if(sp_key$status[1] %in%  search.df){
      
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
#   powoid = LC.results[["POWO_ID"]]s
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
