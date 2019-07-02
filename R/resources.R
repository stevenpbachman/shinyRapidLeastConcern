# This loads all of the data files we need when sourced
TDWG_LEVEL3 <- read_rds(here("data/tdwg_level3.rds"))
GROWTHFORM_LOOKUP <- read_rds(here("data/growthform_list.rds"))
HABITAT_LOOKUP <- read_rds(here("data/habitat_list.rds"))
IUCN_TAXONOMY <- read_rds(here("data/iucn_taxonomy.rds"))
TDWG_TO_IUCN <- read_rds(here("data/tdwg_to_iucn.rds"))