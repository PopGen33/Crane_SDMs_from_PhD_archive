# This script downloads occurrence data from GBIF and does some initial filtering.
# After being run once, the commented-out code doesn't need to run again and we instead load the records from disk

# libraries
# automatically download uninstalled packages from the registry
libNames <- c('tidyverse', 'rgbif', 'ggthemes', 'gtools', 'viridis', 'RColorBrewer', 'pals', 'data.table')
for(i in 1:length(libNames)){
  if (!libNames[i] %in% rownames(installed.packages())){
    install.packages(libNames[i], dependencies = TRUE)
  }
  library(libNames[i], character.only=TRUE)
}

## Download GBIF data for Gruidae
# Get unique identifier for Gruidae
# GruidaeKey <- name_backbone(name = "Gruidae", rank = 'family')$usageKey

# Get all species keys in Gruidae that occur in GBIF
# GruidaeAllSpeciesKeys <- unique(name_lookup(higherTaxonKey =  GruidaeKey, rank = 'species')$data$speciesKey)

# Get records for all species keys (with some initial filtering)
# occ_download() needs auth, but my credentials are in .Renviron after this tutorial: https://docs.ropensci.org/rgbif/articles/gbif_credentials.html
# GruidaeAllDownload<- occ_download(
#   pred_in("taxonKey", GruidaeAllSpeciesKeys),
#   pred("hasGeospatialIssue", FALSE), 
#   pred("hasCoordinate", TRUE), 
#   pred("occurrenceStatus","PRESENT"), 
#   pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
#   pred_or(pred_lte("coordinateUncertaintyInMeters", 10000),pred_isnull("coordinateUncertaintyInMeters")) # allow null/NA coord uncertainty... at least for now
#   )

# Check download status
#occ_download_wait(GruidaeAllDownload)

# Download Key: 0050602-241126133413365
# DOI: 10.15468/dl.mdxctq
# https://www.gbif.org/occurrence/download/0050602-241126133413365
# USE THE ABOVE FOR CITATION / RE-DOWNLOAD ********************************************

# Print Citations Info (better way to do this? gbif_citation() on the download object doesn't work...)
#GruidaeAllDownload

# Get the download (first function puts it in pwd)
#GruidaeAllOccurences <- occ_download_get(GruidaeAllDownload) %>% occ_download_import()

# ... or import the records we already downloaded
GruidaeAllOccurences <- occ_download_import(as.download("0050602-241126133413365.zip"))

# Immediately drop the "footprintWKT" column; sometimes has ridiculous values
GruidaeAllOccurences <- GruidaeAllOccurences %>% select(-footprintWKT)

# Quick plot to characterize distribution of observation types
ggplot(GruidaeAllOccurences) + 
  geom_bar(aes(x = basisOfRecord), stat = 'count') + 
  scale_y_log10(labels = scales::comma, breaks = c(10^(0:6),5000000)) + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0)) + 
  labs(x = "", y = "Count") + 
  ggtitle("Basis of Record for all GBIF Observations of Gruidae")

# Quick plot to figure out where all those machine observations are coming from
ggplot(GruidaeAllOccurences %>% filter(basisOfRecord == "MACHINE_OBSERVATION")) + 
  geom_bar(aes(x = publisher), stat = 'count') + 
  scale_y_log10(labels = scales::comma, breaks = c(10^(0:6),300000)) + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0)) + 
  labs(x = "", y = "Count") + 
  ggtitle("Publishers of Machine Observations for all GBIF Observations of Gruidae")

# Quick Export to look at this in ArcGIS
write.csv(GruidaeAllOccurences %>% filter(basisOfRecord == "MACHINE_OBSERVATION"), file = "MachineObservations.csv")

# Data from PlutoF appears to be mostly individual tracking. While accurate to location, this kind of data gives single 
# individuals too much influence over the model even if some of those points are removed during spatial rarification
# So, let's filter that

GruidaeAllOccurences <- GruidaeAllOccurences %>% filter(publisher != "PlutoF")

# Quick plot to figure out what "Observation" is
ggplot(GruidaeAllOccurences %>% filter(basisOfRecord == "OBSERVATION")) + 
  geom_bar(aes(x = publisher), stat = 'count') + 
  scale_y_log10(labels = scales::comma, breaks = c(10^(0:6),3000)) + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0)) + 
  labs(x = "", y = "Count") + 
  ggtitle("Publishers of \"Observations\" for all GBIF Observations of Gruidae")

# Quick Export to look at this in ArcGIS
write.csv(GruidaeAllOccurences %>% filter(basisOfRecord == "OBSERVATION"), file = "ObservationNoQualifiers.csv")

# These seem fine, but some don't have month/day data. Will need to filter that later

# Quick plot to figure out what "Observation" is
ggplot(GruidaeAllOccurences %>% filter(basisOfRecord == "OCCURRENCE")) + 
  geom_bar(aes(x = publisher), stat = 'count') + 
  scale_y_log10(labels = scales::comma, breaks = c(10^(0:6),500)) + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0)) + 
  labs(x = "", y = "Count") + 
  ggtitle("Publishers of \"Occurrence\" for all GBIF Observations of Gruidae")

# Quick Export to look at this in ArcGIS
write.csv(GruidaeAllOccurences %>% filter(basisOfRecord == "OCCURRENCE"), file = "OccurrenceNoQualifiers.csv")

# These seem fine, but mostly older occurences that I may drop later depending on the timeframe of the climate data

# Quick plot to figure out what "Material Citation" is
ggplot(GruidaeAllOccurences %>% filter(basisOfRecord == "MATERIAL_CITATION")) + 
  geom_bar(aes(x = publisher), stat = 'count') + 
  scale_y_log10(labels = scales::comma, breaks = c(10^(0:6))) + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0)) + 
  labs(x = "", y = "Count") + 
  ggtitle("Publishers of Material Citation for all GBIF Observations of Gruidae")

# This is a minority of records, and I don't fully understand how this type of record is created, so I'll drop these.

GruidaeAllOccurences <- GruidaeAllOccurences %>% filter(basisOfRecord != "MATERIAL_CITATION")

# Quick plot to figure out what "Material Sample" is
ggplot(GruidaeAllOccurences %>% filter(basisOfRecord == "MATERIAL_SAMPLE")) + 
  geom_bar(aes(x = publisher), stat = 'count') + 
  scale_y_log10(labels = scales::comma, breaks = c(10^(0:6))) + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0)) + 
  labs(x = "", y = "Count") + 
  ggtitle("Publishers of Material Sample for all GBIF Observations of Gruidae")

# Again, very small number of occurrences, so okay to drop (seems to be mostly DNA work?)

GruidaeAllOccurences <- GruidaeAllOccurences %>% filter(basisOfRecord != "MATERIAL_SAMPLE")

# Need at least either eventDate OR (year AND month) to be not NA or NULL
# a convenient shortcut for checking NA or NULL is the invalid() function from gtools
GruidaeAllOccurences <- GruidaeAllOccurences %>% filter(!gtools::invalid(eventDate) | (!gtools::invalid(year) & !gtools::invalid(month)))

# Characterize counts per species (here using )
sort(unique(GruidaeAllOccurences$species))

# Compute a myEventDate column from eventDate
# This implicitly filters any record that does not have a month AND day in it's eventDate and records where eventDate is malformed (according to ISO 8601)
# note that darwinCore allows specifying a range here where the bounds are delimited by a slash (/); this implicitly takes the lower bound for myEventDate
# See: https://dwc.tdwg.org/list/#dwc_eventDate
GruidaeAllOccurences <- GruidaeAllOccurences %>% mutate(myEventDate = str_extract(eventDate, pattern = "[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}"))

# Plot distribution of observations over months
ggplot(GruidaeAllOccurences) + 
  geom_bar(aes(x = species, fill = lubridate::month(myEventDate, label = TRUE)), position = "dodge", stat = 'count') + 
  scale_y_log10(labels = scales::comma, breaks = c(10^(0:6), 300000)) + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0), legend.title = element_blank()) + 
  labs(x = "", y = "Count") + 
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Counts per Species of filtered GBIF Observations of Gruidae")

# Check for January 1st defaulting (i.e. year is known; arbitrarily entered as Jan 1st midnight)
Jan1st <- GruidaeAllOccurences %>% filter(lubridate::month(myEventDate) == 1 & lubridate::day(myEventDate) == 1)

# Most of these don't have eventTime set, so difficult to tell if these are cases of something defaulting to Jan 1
# These make up 25506 observations, which is ultimately a small number, so may purge these if they look weird in ArcGIS
# i.e. a bird in the middle of it's summer range in the northern hemisphere in January that happens to be a Jan 1
fwrite(Jan1st, file = "Jan1stOccs_TEMP.csv")
rm(Jan1st)

# After looking at this in ArcGIS, some of these are definitely a case of a date defaulting to Jan 1st.
# let's filter them and see how it impacts the dataset

GruidaeAllOccurences <- GruidaeAllOccurences %>% filter(!(lubridate::month(myEventDate) == 1 & lubridate::day(myEventDate) == 1))

# Plot distribution of observations over months
ggplot(GruidaeAllOccurences) + 
  geom_bar(aes(x = species, fill = lubridate::month(myEventDate, label = TRUE)), position = "dodge", stat = 'count') + 
  scale_y_log10(labels = scales::comma, breaks = c(10^(0:6), 300000)) + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0), legend.title = element_blank()) + 
  labs(x = "", y = "Count") + 
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Counts per Species of filtered GBIF Observations of Gruidae", subtitle = "No Jan 1")

# This seems to have removed the weird January peaks without removing too much data

# Let's see which GBIF issues exist in the remaining data. Actually kind of annoying to get this list.
# Rare use of rapply with help via https://stackoverflow.com/questions/16182272/extract-a-column-from-a-matrix-in-a-list-in-r
remainingGBIFissues <- lapply(GruidaeAllOccurences$issue, str_match_all, pattern = "([a-zA-Z_]+);?")
remainingGBIFissues <- unique(rapply(remainingGBIFissues, classes = "matrix", how = "unlist", f = function(x) x[, 2, drop = FALSE]))
sort(remainingGBIFissues)

# Some of these are worth looking into:
#what <- GruidaeAllOccurences %>% filter(grepl("RECORDED_DATE_INVALID", issue))
#write.csv(what %>% mutate(eventDate = paste0(" ", eventDate)), file = "what.csv")
# "OCCURRENCE_STATUS_INFERRED_FROM_INDIVIDUAL_COUNT" 5 records; count listed as 0? These seem fine otherwise. Notably these are older 'PRESERVED_SPECIMEN's
# "TAXON_MATCH_HIGHERRANK" all 54 records are attempts at reporting subspecies names, but they're malformed and fall back to species
# "RECORDED_DATE_INVALID" 455 records; eventDate seems fine? verbatimEventDate malformed for missing, so this can probably be ignored
# The above did reveal that things are sometimes entered as the first of the month when the verbatimEventDate is only partially parsable...
# "RECORDED_DATE_MISMATCH" 518 records; Seems like eventDate didn't parse into the day column for some of these; others I can't tell why it thinks it's invalid; all from SLU Artdatabanken
# "BASIS_OF_RECORD_INVALID" 46 records; basis of record is OBSERVATION; others (431 total) with the same basis aren't flagged?
#
# These seem fine. Full list:
# [1] "AMBIGUOUS_INSTITUTION"                             "BASIS_OF_RECORD_INVALID"                          
# [3] "COLLECTION_MATCH_FUZZY"                            "COLLECTION_MATCH_NONE"                            
# [5] "CONTINENT_COORDINATE_MISMATCH"                     "CONTINENT_COUNTRY_MISMATCH"                       
# [7] "CONTINENT_DERIVED_FROM_COORDINATES"                "CONTINENT_INVALID"                                
# [9] "COORDINATE_PRECISION_INVALID"                      "COORDINATE_REPROJECTED"                           
# [11] "COORDINATE_ROUNDED"                                "COORDINATE_UNCERTAINTY_METERS_INVALID"            
# [13] "COUNTRY_DERIVED_FROM_COORDINATES"                  "COUNTRY_INVALID"                                  
# [15] "COUNTRY_MISMATCH"                                  "ELEVATION_MIN_MAX_SWAPPED"                        
# [17] "FOOTPRINT_SRS_INVALID"                             "FOOTPRINT_WKT_INVALID"                            
# [19] "FOOTPRINT_WKT_MISMATCH"                            "GEODETIC_DATUM_ASSUMED_WGS"                       
# [21] "GEODETIC_DATUM_INVALID"                            "IDENTIFIED_DATE_INVALID"                          
# [23] "INDIVIDUAL_COUNT_CONFLICTS_WITH_OCCURRENCE_STATUS" "INDIVIDUAL_COUNT_INVALID"                         
# [25] "INSTITUTION_COLLECTION_MISMATCH"                   "INSTITUTION_MATCH_FUZZY"                          
# [27] "INSTITUTION_MATCH_NONE"                            "MODIFIED_DATE_INVALID"                            
# [29] "MODIFIED_DATE_UNLIKELY"                            "MULTIMEDIA_DATE_INVALID"                          
# [31] "MULTIMEDIA_URI_INVALID"                            "OCCURRENCE_STATUS_INFERRED_FROM_INDIVIDUAL_COUNT" 
# [33] "RECORDED_DATE_INVALID"                             "RECORDED_DATE_MISMATCH"                           
# [35] "REFERENCES_URI_INVALID"                            "TAXON_MATCH_FUZZY"                                
# [37] "TAXON_MATCH_HIGHERRANK"                            "TAXON_MATCH_SCIENTIFIC_NAME_ID_IGNORED"           
# [39] "TAXON_MATCH_TAXON_CONCEPT_ID_IGNORED"              "TAXON_MATCH_TAXON_ID_IGNORED"                     
# [41] "TYPE_STATUS_INVALID"   

#rm(what)

# Okay, final characterization of dataset prior to splitting it off into species

# No strong bias toward 1st of month dates after removing Jan 1; likely some defaulting to the 1st, though (highest count)
ggplot(GruidaeAllOccurences %>% group_by(species)) + 
  geom_bar(aes(x = lubridate::day(myEventDate), fill = species), stat = 'count') + 
  scale_y_continuous(labels = scales::comma, breaks = 25000*0:6, limits = c(0, 25000*6)) + 
  theme_clean() + 
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = 1:31) + 
  scale_fill_manual(values = unname(polychrome()[-(1:2)])) +
  labs(x = "", y = "Count") + 
  ggtitle("Counts per Day of Month", subtitle = "No Jan 1")

# ...Until you drop G. grus and G. canadensis; now it's obvious there *is* a bias toward 1st of the month
ggplot(GruidaeAllOccurences %>% filter(!(species %in% c("Grus canadensis", "Grus grus"))) %>% group_by(species)) + 
  geom_bar(aes(x = lubridate::day(myEventDate), fill = species), stat = 'count') + 
  scale_y_continuous(labels = scales::comma, breaks = 5000*0:4, limits = c(0, 5000*4)) + 
  theme_clean() + 
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = 1:31) + 
  scale_fill_manual(values = unname(polychrome()[-c(1:2, 8:9)])) +
  labs(x = "", y = "Count") + 
  ggtitle("Counts per Day of Month", subtitle = "No Jan 1; First of the Month Bias?")

# Write Occs to files for ArcGIS import
# Go ahead and split by species
# let's also export a file with no 1st of the month dates
spec <- unique(GruidaeAllOccurences$species)
for(sp in spec){
  message(paste("Writing", sp))
  name <- gsub(" ", "_", sp)
  dir.create(name)
  fwrite(x = GruidaeAllOccurences %>% filter(species == sp), file = file.path(name, paste0(name, ".csv")))
}

# After viewing this in ArcGIS, this will need some manual curation to remove points that are obviously outside of 
# the normal range (might be zoos or some other kind of "out of range" occs?)

# After that, move on to spatial rarefaction