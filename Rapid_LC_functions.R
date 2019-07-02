
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

# 3.1 get the gbif key
check_gbif = function (full_name) {
  
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

get_gbif_key <- function(species_name) {
  bad_result_types <- c(
    "PROPARTE_SYNONYM",
    "DOUBTFUL",
    "HETEROTYPIC_SYNONYM",
    "HOMOTYPIC_SYNONYM",
    "MISAPPLIED",
    "SYNONYM"
  )

  warning <- NA_character_
  gbif_key <- NA_integer_

  gbif_matches <- check_gbif(species_name)

  if (is.na(gbif_matches$usageKey[1])) {
    warning <- "No name match in GBIF"
  }

  if (gbif_matches$status[1] %in% bad_result_types) {
    # Is this really something bad? As long as it's accepted in POWO?
    warning <- "Best name match against GBIF is not treated by GBIF as accepted"
  }

  if (is.na(warning)) {
    gbif_key <- gbif_matches$usageKey[1]
  }
 
  tibble(gbif_key=gbif_key,
         warning=warning)
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
    YEAR = NA_character_,
    COMPILER = NA_character_,
    CITATION = NA_character_,
    recordedBy = NA_character_,
    recordNumber = NA_character_,
    issues = NA_character_,
    datasetKey = NA_character_
  )

  if (key == "" | is.na(key)) {
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

get_accepted_name = function(name_in) {
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
}

deduplicate_by <- function(.data, ...) {
  group_vars <- enquos(...)
  .data %>%
    group_by(!!! group_vars) %>%
    filter(row_number() == 1) %>%
    ungroup()
}

# 3.5 native range clip


find.native = function(points, native_range, TDWGpolys){

  if (is.na(points$BINOMIAL[1])) {
    native_points <- mutate(points, native_range=NA_character_)
    return(native_points)
  }

  # TODO: maybe replace/add option for raster solution if more points provided
  # prepare the point data as spatial
  point_sf <- st_as_sf(points, 
                       coords=c("DEC_LONG", "DEC_LAT"),
                       crs=st_crs(TDWGpolys), 
                       remove=FALSE)
  # get shapes of native range
  native_tdwg <- filter(TDWGpolys, LEVEL3_COD %in% native_range$LEVEL3_COD)
  native_tdwg <- select(native_tdwg, LEVEL3_COD)
  # clip points to native range with a spatial join
  native_points <- st_join(point_sf, native_tdwg)
  native_points <- rename(native_points, native_range=LEVEL3_COD)
  # convert back to normal data frame from sf
  native_points <- as_tibble(native_points)
  native_points <- select(native_points, -geometry)
  
  native_points
}

# 3.6 SIS files allfields.csv
# generate info files
allfields = function(powo_id){
  tibble(
    internal_taxon_id = powo_id,	
    CurrentTrendDataDerivation.value = "Suspected",
    nothreats.nothreats = "TRUE",
    threatsunknown.value = "FALSE")
}

# 3.7 SIS files assessments.csv
assessments = function(powo_id){
  rationale_str = paste("This species has a very wide distribution,", 
                        "large population,", 
                        "is not currently experiencing any major threats", 
                        "and no significant future threats have been identified.", 
                        "This species is therefore assessed as Least Concern.")
  
  tibble(
    internal_taxon_id = powo_id,	
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
}

# 3.8 SIS files countries.csv
countries = function(native_range){
  # merge with IUCN country file
  country_table = left_join(native_range, TDWG_to_IUCN_version3_UTF, by=c("LEVEL3_COD"="Level.3.code"))
  
  # now get rid of the duplicates to get a clean list
  country_table = deduplicate_by(country_table, POWO_ID, countryoccurrence.countryoccurrencesubfield.countryoccurrencename)
  
  country_table$CountryOccurrence.CountryOccurrenceSubfield.presence = "Extant"
  country_table$CountryOccurrence.CountryOccurrenceSubfield.origin = "Native"
  country_table$CountryOccurrence.CountryOccurrenceSubfield.seasonaility = "Resident"

  select(country_table,
         POWO_ID,
         countryoccurrence.countryoccurrencesubfield.countryoccurrencename,
         CountryOccurrence.CountryOccurrenceSubfield.presence,
         CountryOccurrence.CountryOccurrenceSubfield.origin,
         CountryOccurrence.CountryOccurrenceSubfield.seasonaility)
}

# 3.9 SIS files - credits.csv
credits = function(powo_id, name="your_name your_name", email="your email", affiliation="your affiliation") {
  tibble(
    internal_taxon_id = powo_id,
    credit_type = "Assessor",
    firstName = word(name,1),
    lastName = word(name,2),
    initials = "",
    Order = "1",
    email = email,
    affiliation = affiliation,
    user_id = "1"
  )
}

# 3.10 SIS files - habitats.csv
habitats = function(powo_id, habitat=NA_character_) {
  
  species_habitats <- tibble(internal_taxon_id=powo_id, description=habitat)
  species_habitats <- left_join(species_habitats, habitatlist, by="description")
  
  species_habitats <- rename(species_habitats, 
                             GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsName=description,
                             GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsLookup=code)
  
  species_habitats$GeneralHabitats.GeneralHabitatsSubfield.suitability = "Suitable"
  species_habitats$GeneralHabitats.GeneralHabitatsSubfield.majorImportance	= ""
  species_habitats$GeneralHabitats.GeneralHabitatsSubfield.season = ""	
  return(species_habitats)
}

# 3.11 SIS files - plantspecific.csv
plantspecific = function(powo_id, growth_form=NA_character_) {
  
  gf = tibble(internal_taxon_id=powo_id, description=growth_form)
  gf = left_join(gf, plantgflist, by="description")
  
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
taxonomy = function(powo_id, gbif_key, powo_author){
  
  name_info = name_usage(gbif_key)

  tax = tibble(
    internal_taxon_id = powo_id,	
    family = name_info$data$family,
    genus = name_info$data$genus,
    species = word(name_info$data$species,2),  
    taxonomicAuthority = powo_author)

  tax = inner_join(tax, taxonomy_iucn, by="family")
  tax = tax[c(2, 6:8, 1, 3:5)]
  
  return(tax)
}

get_species_info <- function(input_info) {
  info_functions <- list(
    allfields=allfields,
    assessments=assessments,
    countries=function(x) countries(input_info$native_range),
    credits=function(x) credits(x, name=input_info$name, email=input_info$email, affiliation=input_info$affiliation),
    habitats=function(x) habitats(x, input_info$habinput),
    plantspecific=function(x) plantspecific(x, input_info$gfinput),
    taxonomy=function(x) taxonomy(x, input_info$key, "Geoff")
  )

  species_info <- purrr::map(info_functions, ~.x(input_info$powo))
  names(species_info) <- names(info_functions)

  return(species_info)
}

# 3.13 save all SIS files
prepare_sis_files = function(species_info, zip_folder=here("data/singlezip")){
 
  if (file.exists(zip_folder)) {
    unlink(zip_folder, recursive=TRUE)
  }
  
  dir.create(zip_folder)
  
  purrr::walk2(species_info, names(species_info), 
               ~write_csv(.x, paste(zip_folder, "/", .y, ".csv", sep="")))  
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
calculate_statistics = function(name, ipni_key, points, warning=NA_character_) {
  # get the gbif key or bail out if there is no match
  
  statistics <- tibble(
    EOO=NA_real_,
    AOO=NA_real_,
    RecordCount=NA_integer_,
    TDWGCount=NA_integer_,
    POWO_ID=ipni_key,
    full_name=name
  )
  
  points <- filter(points, ! is.na(native_range))

  if (nrow(points) == 0) {
    warning = "No GBIF points in native range"
  }

  if (is.na(warning)) {
    range_measures <- eoo.aoo(points)

    statistics <- mutate(statistics, 
                        RecordCount=nrow(points),
                        TDWGCount=length(unique(points$native_range)),
                        EOO=range_measures$EOO,
                        AOO=range_measures$AOO)
  }  
  statistics$Warning = warning

  return(statistics)
}
