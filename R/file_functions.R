# Functions that prepare and handle files needed for SIS

prepare_sis_files = function(species_info, zip_folder=here("data/singlezip")){
 
  if (file.exists(zip_folder)) {
    unlink(zip_folder, recursive=TRUE)
  }
  
  dir.create(zip_folder)
  
  purrr::walk2(species_info, names(species_info), 
               ~write_csv(.x, paste(zip_folder, "/", .y, ".csv", sep="")))  
}

get_species_info <- function(input_info) {
  # TODO: find a more satisfactory way to do this
  info_functions <- list(
    allfields=allfields,
    assessments=assessments,
    countries=function(x) countries(input_info$native_range),
    credits=function(x) credits(x, name=input_info$name, email=input_info$email, affiliation=input_info$affiliation),
    habitats=function(x) habitats(x, HABITAT_LOOKUP, input_info$habinput),
    plantspecific=function(x) plantspecific(x, GROWTHFORM_LOOKUP, input_info$gfinput),
    taxonomy=function(x) taxonomy(x, input_info$author, IUCN_TAXONOMY)
  )

  species_info <- purrr::map(info_functions, ~.x(input_info$powo))
  names(species_info) <- names(info_functions)

  return(species_info)
}

allfields = function(powo_id){
  
  tibble(
    internal_taxon_id = powo_id,	
    CurrentTrendDataDerivation.value = "Suspected",
    nothreats.nothreats = "TRUE",
    threatsunknown.value = "FALSE")
}

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

countries = function(native_range){
  
  # merge with IUCN country file
  country_table = left_join(native_range, TDWG_TO_IUCN, by=c("LEVEL3_COD"="Level.3.code"))
  
  #country_table$Internal_taxon_id = country_table$POWO_ID
  
  # now get rid of the duplicates to get a clean list
  country_table = deduplicate_by(country_table,POWO_ID, countryoccurrence.countryoccurrencesubfield.countryoccurrencename)
  
  country_table$CountryOccurrence.CountryOccurrenceSubfield.presence = "Extant"
  country_table$CountryOccurrence.CountryOccurrenceSubfield.origin = "Native"
  country_table$CountryOccurrence.CountryOccurrenceSubfield.seasonaility = "Resident"
  
  country_table <- rename(country_table, 
                           internal_taxon_id=POWO_ID)
  
  select(country_table,
         internal_taxon_id,
         countryoccurrence.countryoccurrencesubfield.countryoccurrencename,
         CountryOccurrence.CountryOccurrenceSubfield.presence,
         CountryOccurrence.CountryOccurrenceSubfield.origin,
         CountryOccurrence.CountryOccurrenceSubfield.seasonality,
         countryoccurrence.countryoccurrencesubfield.countryoccurrencelookup)
  

}

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

habitats = function(powo_id, habitat_lookup, habitat=NA_character_) {
  
  species_habitats <- tibble(internal_taxon_id=powo_id, description=habitat)

  species_habitats <- left_join(species_habitats, habitat_lookup, by="description")
  
  species_habitats <- rename(species_habitats, 
                             GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsName=description,
                             GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsLookup=code)
  
  species_habitats$GeneralHabitats.GeneralHabitatsSubfield.suitability = "Suitable"
  species_habitats$GeneralHabitats.GeneralHabitatsSubfield.majorImportance	= ""
  species_habitats$GeneralHabitats.GeneralHabitatsSubfield.season = ""	
  return(species_habitats)
}

plantspecific = function(powo_id, growthform_lookup, growth_form=NA_character_) {
  
  gf = tibble(internal_taxon_id=powo_id, description=growth_form)
  gf = left_join(gf, growthform_lookup, by="description")
  
  gf = rename(gf,
              PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName=description,
              PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup=code)
  
  return(gf)
}

taxonomy = function(powo_id, powo_author, taxonomy_lookup){
  
  name_info <- lookup_powo(powo_id)

  tax = tibble(
    internal_taxon_id = powo_id,	
    family = name_info$family,
    genus = name_info$genus,
    species = name_info$species,  
    taxonomicAuthority = powo_author)

  tax = inner_join(tax, taxonomy_lookup, by="family")
  tax = tax[c(1, 6:9, 2:5)]
  
  return(tax)
}

format_points = function(points, renaming_map=list()) {
  # TODO: change gbif point function to use this formatting function
  default_data <- list(
    BasisOfRec = NA_character_,
    BINOMIAL = NA_character_,
    DEC_LAT = -999,
    DEC_LONG = -999,
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
  
  formatted_points <- rename(points, !!! renaming_map)
   
  columns_to_add <- setdiff(names(default_data), colnames(formatted_points))
  formatted_points <- tibble::add_column(formatted_points, !!! default_data[columns_to_add])
        
  formatted_points$YEAR = format(Sys.Date(), "%Y")
  
  formatted_points <- select(formatted_points, colnames(formatted_points))
  
  return(formatted_points)
}


