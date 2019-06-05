---
title: "Rapid Least Concern Help Notes"
author: Steven Bachman
date: 3rd March 2019
output:   
  html_document:
    theme: simplex
    toc: true
    toc_depth: 4
    toc_float:
      collapsed: false
      smooth_scroll: false
    
---

### Background: 

This is a first attempt at some Help notes to support use of **"Rapid LC"** - a tool to generate IUCN Red List Least Concern assessments.

#### What is Rapid Least Concern?
Some brief notes here about what it is, what is is trying  to do.  
Link to the paper...

#### Why create Rapid Least Concern? 
A brief overview - context etc. lack of plants on the red list etc.
Although comprehensive for some groups such as birds and mammals, the IUCN Red List only represents about 7% of known plant species diveristy. There is an urgent need to increase the coverage of plants on the Red List. The rate of new plant species assessments being published on the Red List is very slow, with only a few thousand being published each year. 
Recent changes to Red List rules such as the reduction in documentation requirement for Least Concern taxa and increased availablility of data through repositories like GBIF and POWO all the documetaiton process to be autoamted. 

### How to use:
There are two options for generating LC assessments: **single** and **batch**. The single option allows more control in selecting names and exploring occurrence distributions. The batch option allows users to upload a csv. file with a list of names that can be processed as a batch.

#### Single

**Tab 1. Search**  
**1** Enter a binomial (Genus species) into the search box. Two tables of results will appear in the main panel to the right. The upper table shows the results from a search of the binomial against the [Global Biodiversity Information Facility (GBIF)](https://www.gbif.org/species/search) names backbone. The best matches are listed in order of confidence. The scientificName field includes the author and can be used to make sure you find the species you are looking for. 


The lower table shows the results after running the name against the [Plants of the World (POWO)](http://plantsoftheworldonline.org/) names backbone.  

![Single name search window with search results from GBIF and Plants of the World Online (POWO) for search term *Aloe zebrina*](help\name_search.png)

**2 and 3** After deciding the best name match from the GBIF search, copy and paste the **'usageKey'** into the search box 2. (*# add click to select on next version to avoid cut and paste*). Similarly, copy and paste the **'IPNI_ID'** from the best name match to search box 3.

![Single name search window with 'usageKey' and 'IPNI_ID' copied into search box 2. and 3.](help\name_results.png)

**4, 5 and 6** If you would like name and citation details to be included in the assessment files then enter your name, email address and affiliation.  

With the keys entered, you can now click the **Draw map** button. A map will appear in the main panel to the right. The map shows all georeferenced occurrence points from GBIF (green markers) and the native range based on the TDWG system, as used by POWO (red polygons).   

![Occurrence points and native range mapped.](help\map.png)

**Tab 2. Clean**  
In this section there is the option to clean the occurrence data. The raw distribution map will show on the main panel to the right. Currently there is only one option to clean the data, which is to remove any points that are outside the native range. To do this, check the box **Remove non-native points** and click the **CLEAN** button. The updated map with non-native occurrence points removed will be shown below. 

![raw distribution map.](help\cleanmap1.png)
![clean distribution map.](help\cleanmap2.png)

**Tab 3. Download**  
In this section a series of tables are generated that provide the minimum information required to support a Least Concern Red List assessment.  
The first tab shows the occurrence point table. This has been formatted to the IUCN spatial data standards. This can be downloaded by clicking the **DOWNLOAD CLEAN POINT FILE** button.  

![Option to download occurence points.](help\downloadpoints.png)  

Two fields require user input: **habitat** and **plant growth form**. Use the multiple select options from the sidebar on the left to pick the relevant habitat and growth form. These are then added to the tables in the habitat and growth form tabs to the right. 

![Enter growth form and habitat from list.](help\habitats.png)   

The csv files can now be downloaded. Click the **DOWNLOAD SIS CONNECT FILES** button and should be able to download a compressed folder with all csv files included (# Taxonomy not working at the moment).

#### Batch
**1** The batch option requires users to upload a list of names that can be processed together. The file needs to be a csv. with a 'name_in' column. Click the **BROWSE** button to upload the file. Depending on how many species are in your file, it may take a moment or two before the results table appears in the main panel. The names will have been checked against the [POWO](http://plantsoftheworldonline.org/) names backbone. The results table includes the IPNI identifier, author and accepted status, as well as the original name_in field.   

**2** Investigate the results table either by clicking the 'next' and 'previous' buttons, or click the **DOWNLOAD TABLE** button to view the full list. Any names that do not find a match in POWO cannot be analysed further, so these need to be removed from the table and the resulting list of 'clean' names can then be reloaded using the **BROWSE** button.    

![Upload csv file and if necessary, download and then reload a 'clean' list of names.](help\batch_upload.png) 

**3** Click the **GET STATISTICS** button to generate the raw statistics. The results contain the original search results fields (IPNI identifier, author, accepted status and name_in) as well as several metrics relating to geographic range size:

* **EOO** = Extent of occurrence. Calculated as the area (km^2^) of a minimum convex polygon of all extant occurrence points within the native range. 
* **AOO** = Area of occupancy. Area (km^2^) calculated by summing the number of occupied cells based on occurence points within the native range by the area of the cells. A grid of 10 km x 10 km cells was used to account for georeference error as opposed to the standard 2 x 2 km reference scale. A single occupied cell would return an AOO value of 100 km^2^
* **RecordCount** = The number of unique occurrence records within the native range. 
* **TDWGCount** = The number of Level 3 TDWG regions the species occupies across its native range. 
* **Warning** = Indicates that there has been a problem with the calculation due to lack of data. For example there are no occurrence points, there are no points in the native range, the native range is unknown. 

![Result of analysis.](help\stats_out.png) 

**4** The result table will automatically be filtered based on the thresholds set by the slider bars on the side panel. Note that the three species upload in the example have been reduced to one species in the results table because the other species have a TDWG count less than 5, which is the default threshold.

![Sliders to adjust the threshold for determining Least Concern.](help\sliders.png) 

All sliders can be manually adjusted. As these metrics mostly only relate to range size, the user is asked to confirm that there is no other decline occurring (or past decline, or future decline expected) by ticking the check box: **No observed, estimated, projected, inferred, or suspected declines likely to trigger criteria A, B, C, D or E.** Unticking will provide a warning message, but will not affect the results. 

**5** When the result table contains species that you would like to docuemnt as Least Concern, click the **DOWNLOAD SIS CONNECT FILES** button. This will generate a Zip/Compressed file containing several csv. files 

![csv output files.](help\csv.png) 

The output files contain a line for each species and all files are linked through the uniqie identifier **internal_taxon_id**. The results.csv file is not required for the Red List assessment, but is porvided for information. The points.csv file contains all occurrence points in the native range and has been formatted to meet the IUCN Mapping standards (#Link). The points.csv file is required to support the Red List assessment, but is not currently accepted through the SIS Connect system (# see links below). All other files can be imported into SIS Connect. 

**What is missing?**
It is important to note that some editing is required prior to submitting the outputs provided through **SIS Connect**. 


### Next steps:

#### Add missing values
Currently, there are several required fields that cannot be generated automatically such as **Plant Growth Form** and **Habitat type**. These need to be filled for each species individually.  

* **Plant growth form**. For many species, the plant growth form can be found by querying the [World Checklist of Selected Plant Families ](https://wcsp.science.kew.org/).  

The lookup values for the **plantgrowthforms.plantgrowthformslookup** fields are:

Code          | description
------------- | -------------
T             | Tree - size unknown
TL            | Tree - large 
TS	          | Tree - small
S	            | Shrub - size unknown
SL	          | Shrub - large
SS	          | Shrub - small
F	            | Forb or Herb
A	            | Annual
GR	          | Graminoid
GE	          | Geophyte
V	            | Vines
H	            | Hydrophyte
P	            | Parasite
E	            | Epiphyte
L	            | Lithophyte
SC	          | Succulent - form unknown
SA	          | Succulent - annual
SH	          | Succulent - shrub
ST	          | Succulent - tree
PT	          | Fern
C	            | Cycad
M	            | Fungus
B	            | Moss
LC	          | Lichen


* **Habitat**. The habitat classification scheme can be found here: https://www.iucnredlist.org/resources/habitat-classification-scheme and additional descriptors here: https://nc.iucnredlist.org/redlist/content/attachment_files/dec_2012_guidance_habitats_classification_scheme.pdf

* **credits.csv**. The credits.csv file contains references to the people who helped to generate the assessment. Placeholder values have been added to the firstName, lastName, email and affiliation fields, but these must be filled in prior to submission. Please see the supporting information document for further details on crediting: https://www.iucnredlist.org/resources/supporting-information-guidelines

* **countries.csv**. Note that the list of countries should follow the IUCN standard. However, the generated list is derived from POWO, which uses the TDWG World Geographical Scheme for Recording Plant Distributions (WGSRPD) http://www.tdwg.org/standards/109. The Level 3 classification corresponds to small to medium sized countries in many cases, however, there are cases where there is no clear match. In these cases Rapid Least Concern reports an error and it is for the user to determine which countries should be selected. 

![crosswalk error between TDWG and IUCN Country list.](help\countries.png) 


#### Review assessments (SGs and RLAs)
All Red List assessments need to be reviewed by a relevant Red List Authority. A list of plant Red List Authorities or Specialist Groups can be found here (# link to plant dash). 

#### Submit to SIS connect - needs login  
When the csv. files have been updated with plant growth form, habitat and countries data in the correct format, the fiels can be uploaded to [SIS Connect](http://connect.iucnredlist.org/)


### Acknowledgements:

The following libraries were used to create this app:

[magrittr](https://CRAN.R-project.org/package=magrittr)  
[readr](https://CRAN.R-project.org/package=readr)  
[ggplot2](https://CRAN.R-project.org/package=ggplot2)  
[plotly](https://CRAN.R-project.org/package=plotly)  
[rgdal](https://CRAN.R-project.org/package=rgdal)  
[DT](https://CRAN.R-project.org/package=DT)  
[leaflet](https://CRAN.R-project.org/package=leaflet)  
[rgbif](https://CRAN.R-project.org/package=rgbif)  
[sp](https://CRAN.R-project.org/package=sp)  
[jsonlite](https://CRAN.R-project.org/package=jsonlite)  
[sf](https://CRAN.R-project.org/package=sf)  
[wicket](https://CRAN.R-project.org/package=rgdal)  
[shinythemes](https://CRAN.R-project.org/package=shinythemes)  
[zip](https://CRAN.R-project.org/package=zip)  
[httr](https://CRAN.R-project.org/package=httr)  
[raster](https://CRAN.R-project.org/package=raster)  
[tidyverse](https://CRAN.R-project.org/package=tidyverse)  
[stringr](https://CRAN.R-project.org/package=stringr)  
[shiny](https://CRAN.R-project.org/package=shiny)  
[plyr](https://CRAN.R-project.org/package=plyr)  
[rCAT](https://CRAN.R-project.org/package=rCAT)  


The following people helped:  
John Iacona
Justin Moat
Serene Hargreaves
Malin Rivers
Baz Walker

Testers: 

### License
To be determined
