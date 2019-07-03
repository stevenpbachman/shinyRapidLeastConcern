# Functions that gather or check information using API requests

get_accepted_name = function(name_in) {
  # lookup name in POWO
  powo_results = search_name_powo(name_in)
  # remove anything that wasn't the name we wanted
  powo_results = filter(powo_results, name == name_in)
  
  powo_results = rename(powo_results, name_in=name)

  powo_results = unite(powo_results, fullname, name_in, author, sep=" ", remove=FALSE)
  # might error if all different names returned
  powo_results = powo_results[1,]
  
  return(powo_results)
}

search_name_powo = function(name_in) {

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

get_native_range = function(ID){
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

check_if_native = function(points, native_range, range_polygons){

  if (is.na(points$BINOMIAL[1])) {
    native_points <- mutate(points, native_range=NA_character_)
    return(native_points)
  }

  # TODO: maybe replace/add option for raster solution if more points provided
  # prepare the point data as spatial
  point_sf <- st_as_sf(points, 
                       coords=c("DEC_LONG", "DEC_LAT"),
                       crs=st_crs(range_polygons), 
                       remove=FALSE)
  # get shapes of native range
  native_tdwg <- filter(range_polygons, LEVEL3_COD %in% native_range$LEVEL3_COD)
  native_tdwg <- select(native_tdwg, LEVEL3_COD)
  # clip points to native range with a spatial join
  native_points <- st_join(point_sf, native_tdwg)
  native_points <- rename(native_points, native_range=LEVEL3_COD)
  # convert back to normal data frame from sf
  native_points <- as_tibble(native_points)
  native_points <- select(native_points, -geometry)
  
  native_points
}

search_name_gbif = function (full_name) {
  
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
  
  gbif_matches <- search_name_gbif(species_name)

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


key = "5293942"
#lic_stip_points = get_gbif_points(key)


get_gbif_points = function(key) {
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
  
  if (gbif_results$meta$count == 0){
    return(res)
  }
  
  gbif_points = gbif_results$data
  
  if (nrow(gbif_points) > 0) {
    
    gbif_points = rename(gbif_points,
      BasisOfRec=basisOfRecord,
      DEC_LAT=decimalLatitude,
      DEC_LONG=decimalLongitude,
      BINOMIAL=scientificName,
      CATALOG_NO=catalogNumber
    )
    
    columns_to_add = setdiff(colnames(res), colnames(gbif_points))
    default_data = as.list(res)
    gbif_points = tibble::add_column(gbif_points, !!! default_data[columns_to_add])
    
    #gbif_points = rename(gbif_points, EVENT_YEAR=year)
    
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
                            "UNKNOWN"="Unknown"
                          ))
    
    res = select(gbif_points, colnames(res))
  }
  print(res[1,2])
  return(res)
}
