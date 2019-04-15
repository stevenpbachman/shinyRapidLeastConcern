

testLC = LC_comb(species)
testLC = LC_comb(species = testfile)

species = test_names_Cola.altissima

testingEOO = eoo.aoo(native_clipped)

library(rCAT)

# 3.14 EOO and AOO
eoo.aoo = function(native) {
  
  #native = native_clipped
  
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
  
  #full_name = "Cola altissima"
  #ID = "822648-1"
  
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