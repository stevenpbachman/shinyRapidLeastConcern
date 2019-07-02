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
    taxonomy=function(x) taxonomy(x, IUCN_TAXONOMY, input_info$key, input_info$author)
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

taxonomy = function(powo_id, taxonomy_lookup, gbif_key, powo_author){
  
  name_info = name_usage(gbif_key)

  tax = tibble(
    internal_taxon_id = powo_id,	
    family = name_info$data$family,
    genus = name_info$data$genus,
    species = word(name_info$data$species,2),  
    taxonomicAuthority = powo_author)

  tax = inner_join(tax, taxonomy_lookup, by="family")
  tax = tax[c(2, 6:8, 1, 3:5)]
  
  return(tax)
}




