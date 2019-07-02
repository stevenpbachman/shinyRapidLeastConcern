# Functions used for calculations of some sort

deduplicate_by <- function(.data, ...) {
  group_vars <- enquos(...)
  .data %>%
    group_by(!!! group_vars) %>%
    filter(row_number() == 1) %>%
    ungroup()
}

calculate_range = function(points_df) {
  
  mypointsll = data.frame(lat=points_df$DEC_LAT, long=points_df$DEC_LONG)
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
  
  range_results = data.frame(
    EOO = round(EOOkm2abs, 0),
    AOO = AOOkm2,
    RecordCount = rec_count)
  
  return(range_results)
}

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
    range_measures <- calculate_range(points)

    statistics <- mutate(statistics, 
                        RecordCount=nrow(points),
                        TDWGCount=length(unique(points$native_range)),
                        EOO=range_measures$EOO,
                        AOO=range_measures$AOO)
  }  
  statistics$Warning = warning

  return(statistics)
}