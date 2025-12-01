# This script does spatial rarefation while taking temporal windows into account

# libraries
# automatically download uninstalled packages from the registry
libNames <- c('dtplyr', 'data.table', 'tidyverse', 'ggthemes', 'gtools', 'viridis', 'RColorBrewer', 'pals', 'fields', 'interp', 'terra')
for(i in 1:length(libNames)){
  if (!libNames[i] %in% rownames(installed.packages())){
    install.packages(libNames[i], dependencies = TRUE)
  }
  library(libNames[i], character.only=TRUE)
}

# Read the data
GruidaeAllOcc <- fread(file = "GruidaeAllOccs.csv")

# Found a bunch of out of range points in ArcGIS that I want to remove
# These are in individual files, but we can just read them all into the same dataframe/table
OccsToRemove <- rbindlist(lapply(list.files(path = "CraneSDMs_outOfRangeIDs", pattern = "*.csv$", full.names = TRUE), read.csv), fill = TRUE)

ggplot(OccsToRemove) + 
  geom_bar(aes(x = basisOfRecord), stat = 'count') + 
  scale_y_log10(labels = scales::comma, breaks = c(10^(0:6),5000000)) + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0)) + 
  labs(x = "", y = "Count") + 
  ggtitle("Basis of Record for Occs to be removed for Out of Range")

ggplot(OccsToRemove) + 
  geom_bar(aes(x = publisher), stat = 'count') + 
  scale_y_log10(labels = scales::comma, breaks = c(10^(0:6),5000000)) + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0)) + 
  labs(x = "", y = "Count") + 
  ggtitle("Publisher for Occs to be removed for Out of Range")

# Just need some of these columns
OccsToRemove <- OccsToRemove %>% select(gbifID, species, decimalLongitude, decimalLatitude)
fwrite(OccsToRemove, file = "GruidaeAllOccs_RemovedForOutOfRange.csv")

countToRemove <- nrow(OccsToRemove)
countBeforeRemove <- nrow(GruidaeAllOcc)

# Now, the actually filtering these Occs out of GruidaeAllOcc
GruidaeAllOcc <- GruidaeAllOcc %>% filter(!(gbifID %in% OccsToRemove$gbifID))

# This should return True if everything worked (it does)
nrow(GruidaeAllOcc) == countBeforeRemove - countToRemove

rm(OccsToRemove, countBeforeRemove, countToRemove)

# At this point, we just need gbifID, species, decimalLat, decimalLong, myEventDate
GruidaeAllOcc <- GruidaeAllOcc %>% select(gbifID, species, decimalLatitude, decimalLongitude, myEventDate)
gc(reset = TRUE, full = TRUE)

# Distribution of Dates
GruidaeAllOcc %>% mutate(myMonth = month(myEventDate), myYear = year(myEventDate)) %>% 
    group_by(myYear, myMonth) %>% summarise(myMonthMid = median(myEventDate), obvs = n()) %>% 
    ggplot(aes(x=myMonthMid, y = obvs)) + 
    geom_bar(stat = "identity") + 
    scale_y_log10(breaks = c(10,100,1000,10000,100000)) +
    scale_x_date(breaks = '1 year' ,limits = as.Date(c('1980-01-01', '2025-12-31'))) +
    theme_calc() + 
    theme(axis.text.x = element_text(angle = -90))

# Distribution of Obvs across months
GruidaeAllOcc_byMonth <- GruidaeAllOcc %>% 
    mutate(myMonth = format(myEventDate, "%B")) %>% 
    group_by(myMonth) %>% summarise(obvs = n())
GruidaeAllOcc_byMonth$myMonth <- factor(GruidaeAllOcc_byMonth$myMonth, ordered = TRUE, levels = c("January",
                                                                                                  "February",
                                                                                                  "March",
                                                                                                  "April",
                                                                                                  "May",
                                                                                                  "June",
                                                                                                  "July",
                                                                                                  "August",
                                                                                                  "September",
                                                                                                  "October",
                                                                                                  "November",
                                                                                                  "December"))
GruidaeAllOcc_byMonth %>%
    ggplot(aes(x=myMonth, y = obvs)) + 
    geom_bar(stat = "identity") + # ((365*3)+366)/(12*4)
    scale_x_discrete() +
    theme_calc() + 
    theme(axis.text.x = element_text(angle = -90, vjust = 0))
rm(GruidaeAllOcc_byMonth)

# Now, need to actually do spatial rarefaction. For this, need vectors of species to iterate over.

spacialRarefaction <- function(.data, latCol = "decimalLatitude", lonCol = "decimalLongitude", distanceKM = 10){
    # Check .data is valid
    if((length(.data) == 0) | (!inherits(.data, "data.frame"))){
        stop("'data' must be set and be of class 'data.frame'")
    }
    message("Beginning spatial rarefaction...\n")
    # spacial rarefaction while accounting for dplyr groups
    # vars with . appended to the end to avoid a weird recursive reference thing
    .data %>% group_modify(function(df, y, latCol. = latCol, lonCol. = lonCol, distanceKM. = distanceKM){
        startTime <- Sys.time()
        groupVarNames <- names(y)
        for(name in groupVarNames){
            message(paste0("Grouping variable: ", name, ", Value: ", y[name]))
        }
        message(paste0("Size Of original dataset for group: ", nrow(df)))
        # Need to create buffers of distanceKM around points
        #df <- df %>% distinct(!!sym(latCol.), !!sym(lonCol.), .keep_all = TRUE) # get rid of exact duplicate points; This removes *way* more points than it should, what?
        pointsToCheck <- vect(df, geom = c(lonCol., latCol.), crs="+proj=longlat +datum=WGS84") #NOTE: we're assuming WGS84 as CRS here
        buffers <- buffer(pointsToCheck, width = distanceKM. * 1000, quadsegs = 20) # width expects distance in meters; hence, multiplying by 1000
        # Remove points < distanceKM apart and then repeat until no points < distanceKM apart
        # Do this by sampling a random point and its buffer. Points that intersect that buffer are < distanceKM 
        # away. Remove those points from pointsToCheck and from the original df, but retain the point used to 
        # produce the buffer in the original df. Repeat until pointsToCheck is depleted
        intersectIndices <- as.data.frame(relate(pointsToCheck, buffers, relation = "intersects", pairs = TRUE)) # col 1 = buffer index; col 2 = pointsToCheck index
        pointsToElim <- c()
        while(nrow(intersectIndices != 0)){
            # By sampling from this column without calling unique() first, we tend to deal with the buffers with 
            # the most conflicts first
            curBuff <- sample(intersectIndices$id.x, size = 1)
            # pointsToElim is all points inside the buffer that aren't one defining the buffer; cull them
            newPointsToElim <- unique(intersectIndices %>% filter(id.x == curBuff, id.y != curBuff) %>% pull(id.y))
            pointsToElim <- c(pointsToElim, newPointsToElim)
            # Remove curBuff and pointsToElim from intersectIndices; they've been dealt with
            intersectIndices <- intersectIndices %>% filter(!id.x %in% c(newPointsToElim, curBuff), !id.y %in% c(pointsToElim, curBuff))
            #break
        }
        df <- df %>% filter(!row_number() %in% pointsToElim)
        message(paste0("Size after removal of points less than ", distanceKM, " km apart: ", nrow(df)))
        message(paste0("Run time for group: ", format(difftime(Sys.time(), startTime)), "\n"))
        return(df)
    }
    )
}


# Investigate variance in points retained (variance is expected, just need to see how bad it is)
# First, generate a dataset and get summary counts; then repeat and get a distribution of those counts
Grus_antigone_only <- GruidaeAllOcc %>% 
    filter(species == "Grus antigone")
Grus_antigone_spacRare <- Grus_antigone_only %>% 
    group_by(species, myMonth = month(myEventDate)) %>% 
    spacialRarefaction(distanceKM = 10)
Grus_antigone_spacRare_bindTo <- Grus_antigone_spacRare %>% summarise(Count = n())
for(i in 1: 99){
    message(paste0("*~*~*~*~*~ This is iteration ", i))
    Grus_antigone_spacRare <- Grus_antigone_sub %>% group_by(species, myMonth = month(myEventDate)) %>% spacialRarefaction(distanceKM = 20)
    Grus_antigone_spacRare_bindTo <- bind_rows(Grus_antigone_spacRare_bindTo, Grus_antigone_spacRare %>% summarise(Count = n()))
}

# Violin plots for each month to show variation in the points retained
plots <- vector('list', 12)
for(i in 1:12){
    plots[[i]] <- ggplot(data = Grus_antigone_spacRare_bindTo %>% filter(myMonth == i), aes(x = factor(myMonth), y = Count)) + 
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
        ggtitle(paste0("Month ", i)) +
        #theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
}
gridExtra::grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], 
                        plots[[5]], plots[[6]], plots[[7]], plots[[8]], 
                        plots[[9]], plots[[10]], plots[[11]], plots[[12]], 
                        nrow = 3, top = grid::textGrob("Counts of points retained after repeated spatial rarefaction for Grus antigone (repeats = 100)", gp=grid::gpar(fontsize=15,font=3)))

# Numeric summary of the same information
Grus_antigone_spacRare_bindTo %>% group_by(myMonth) %>% summarize(nCounts = n(),
                                                                  meanCount = mean(Count),
                                                                  medianCount = median(Count),
                                                                  maxCount = max(Count),
                                                                  minCount = min(Count),
                                                                  varianceCount = var(Count),
                                                                  sdCount = sd(Count),
                                                                  widthOf95percentInterval = (mean(Count) + 1.96 * sd(Count)) - (mean(Count) - 1.96 * sd(Count)))
# A tibble: 12 × 9
# myMonth nCounts meanCount medianCount maxCount minCount varianceCount sdCount widthOf95percentInterval
# <dbl>   <int>     <dbl>       <dbl>    <int>    <int>         <dbl>   <dbl>                    <dbl>
# 1       1     100      228.        228       235      223          5.26    2.29                     8.99
# 2       2     100      268.        267       273      260          8.31    2.88                    11.3 
# 3       3     100      202.        202.      208      196          5.43    2.33                     9.13
# 4       4     100      166.        166       170      163          2.81    1.68                     6.57
# 5       5     100      173.        173       176      169          2.33    1.53                     5.98
# 6       6     100      162.        162       170      157          5.42    2.33                     9.12
# 7       7     100      168.        168       172      163          3.13    1.77                     6.94
# 8       8     100      170.        169       175      166          3.95    1.99                     7.79
# 9       9     100      160.        160       164      156          3.85    1.96                     7.69
# 10      10     100      199.        200       206      192          6.77    2.60                    10.2 
# 11      11     100      237.        237       244      228          6.97    2.64                    10.3 
# 12      12     100      252.        252       259      245          7.57    2.75                    10.8

rm(Grus_antigone_only, Grus_antigone_spacRare, Grus_antigone_spacRare_bindTo)

# *~*~*~ Split into <2000, 2000-2018, and 2018-2023
GruidaeAllOcc_before2000 <- GruidaeAllOcc %>% filter(as_date(myEventDate) <= as_date("1999-12-31"))
GruidaeAllOcc_2000_2018 <- GruidaeAllOcc %>% filter(as_date(myEventDate) > as_date("1999-12-31") & 
                                                    as_date(myEventDate) <= as_date("2018-12-31"))
GruidaeAllOcc_2019_2023 <- GruidaeAllOcc %>% filter(as_date(myEventDate) > as_date("2018-12-31") & 
                                                    as_date(myEventDate) <= as_date("2023-12-31"))

rm(GruidaeAllOcc)

# *~*~*~ SPATIAL RAREFACTION ON WHOLE DATASET ~*~*~*~*
# The spatial rarefaction method unfortunately runs out of memory when filtering the G. canadensis months that have 100,000+ observations
# We need to subset to something more reasonable first for both G. canadensis and G. grus
# GruidaeAllOcc_gCand_subset <- GruidaeAllOcc %>% 
#     filter(species == "Grus canadensis") %>% 
#     group_by(myMonth = month(myEventDate)) %>% 
#     slice_sample(n=20000)
# GruidaeAllOcc_gCand_subset %>% summarize(Count = n())
# 
# GruidaeAllOcc_gGrus_subset <- GruidaeAllOcc %>% 
#     filter(species == "Grus grus") %>% 
#     group_by(myMonth = month(myEventDate)) %>% 
#     slice_sample(n=20000)
# GruidaeAllOcc_gGrus_subset %>% summarize(Count = n())
# 
# GruidaeAllOcc_reasonableSubset <- bind_rows(GruidaeAllOcc_gGrus_subset, GruidaeAllOcc_gCand_subset, GruidaeAllOcc %>% filter(!species %in% c("Grus canadensis", "Grus grus")))

# Above sub-setting for G. canadensis and G. grus removed because turned out to not be necessary?

GruidaeAllOcc_before2000_spacRare10km <- GruidaeAllOcc_before2000 %>% 
    group_by(species, myMonth = month(myEventDate)) %>% 
    spacialRarefaction(distanceKM = 10)
rm(GruidaeAllOcc_before2000)

GruidaeAllOcc_2000_2018_spacRare10km <- GruidaeAllOcc_2000_2018 %>% 
    group_by(species, myMonth = month(myEventDate)) %>% 
    spacialRarefaction(distanceKM = 10)
rm(GruidaeAllOcc_2000_2018)

GruidaeAllOcc_2019_2023_spacRare10km <- GruidaeAllOcc_2019_2023 %>% 
    group_by(species, myMonth = month(myEventDate)) %>% 
    spacialRarefaction(distanceKM = 10)
rm(GruidaeAllOcc_2019_2023)

# Write the spatially rarefied datasets to file
fwrite(GruidaeAllOcc_before2000_spacRare10km, "GruidaeAllOcc_before2000_spacRare10km.csv")
fwrite(GruidaeAllOcc_2000_2018_spacRare10km, "GruidaeAllOcc_2000_2018_spacRare10km.csv")
fwrite(GruidaeAllOcc_2019_2023_spacRare10km, "GruidaeAllOcc_2019_2023_spacRare10km.csv")

# Write Occs to files for ArcGIS import
# Go ahead and split by species
spec <- unique(GruidaeAllOcc_before2000_spacRare10km$species)
for(sp in spec){
    message(paste("Writing", sp))
    name <- gsub(" ", "_", sp)
    dir.create(name)
    fwrite(x = GruidaeAllOcc_before2000_spacRare10km %>% filter(species == sp), file = file.path(name, paste0(name, "_before2000_spacRare10km.csv")))
    fwrite(x = GruidaeAllOcc_2000_2018_spacRare10km %>% filter(species == sp), file = file.path(name, paste0(name, "_2000_2018_spacRare10km.csv")))
    fwrite(x = GruidaeAllOcc_2019_2023_spacRare10km %>% filter(species == sp), file = file.path(name, paste0(name, "_2019_2023_spacRare10km.csv")))
}

###############################################
# *~*~*~*~*~*~*~ DONE ~*~*~*~*~*~*~*~*~*~*~* ##
###############################################
# Below was pointless sketching that didn't pan out

# will need a "bounds matrix" defining the minLat, maxLat, minLong, and maxLong
# This is simple for latitude, but this can cause issues when a range spans 180 longitude
# This isn't strictly true, but, for this dataset, we can define a function that checks if the 
# difference between the east and west bounds is >180 and flips them if that's true
# getBounds <- function(.data, specCol = NULL, latCol = "decimalLatitude", lonCol = "decimalLongitude"){
#   # Check .data is valid
#   if((length(.data) == 0) | (!inherits(.data, "data.frame"))){
#     stop("'data' must be set and be of class 'data.frame'")
#   }
#   # Account for grouping by species
#   if(length(specCol) > 0){
#     .data <- .data %>% group_by(!!sym(specCol))
#   }
#   # Figure out if data span eastern and western hemisphere
#   # This could mean the range crosses the antimeridan, which is a problem that needs special attention
#   mixedHemi <- .data %>% summarize(
#     isMixed = !(all(!!sym(lonCol) >= 0) | all(!!sym(lonCol) < 0))
#   )
#   # Temp return to see if this works
#   #return(mixedHemi)
#   # Get bounds
#   bounds <- .data %>% summarise(
#     top = max(!!sym(latCol)),
#     bottom = min(!!sym(latCol)),
#     right = max(!!sym(lonCol)),
#     left = min(!!sym(lonCol))
#   )
#   return(bounds)
# }
# 
# # We need to subset by species at this point for the spatial rarification (I think?)
# allSpecies <- unique(GruidaeAllOcc$species)
# 
# # FOR TESTING: Just limit this to one species (G. canadensis has a ton of records, so good test)*********
# spec <- allSpecies[1]
# 
# # In the final version, this will be encapsulated in a loop over all species; linear for now
# # for(species in allSpecies){
# #   # Code
# # }
# 
# # For now, just Grus canadensis since it's the most challenging
# GruidaeAllOcc_spec <- GruidaeAllOcc %>% filter(species == spec)
# 
# # Dump the full dataset b/c memory on my laptop :)
# rm(GruidaeAllOcc)
# gc()
# 
# # Okay, this will require a very specialized approach to be space efficient (approach I found online would need 32000 Gb of memory)
# 
# 
# 
# ###################################
# # Get distance matrix (fields::RdistEarth() is a C implementation that's more memory efficient, but surprisingly a bit slower (see help page))
# # Leaving x2 null gives all pairwise distances; miles = FALSE gives distances in km (why is TRUE the default?)
# # This expects a matrix of col1=long, col2=lat for x1
# 
# # OKAY, this won't work with this many occs; limit to >= 1990?
# GruidaeAllOcc_spec <- GruidaeAllOcc_spec %>% filter(year(myEventDate) >= 1990)
# # That barely does anything... Random sample to reduce dataset size by an order of magnitude?
# set.seed(33)
# GruidaeAllOcc_spec <- GruidaeAllOcc_spec %>% slice_sample(prop = 0.1)
# # Still too big... half it? (THIS RUNS ON THE LARGE MEM NODE)
# GruidaeAllOcc_spec <- GruidaeAllOcc_spec %>% slice_sample(prop = 0.5)
# # Still too big... half it?
# GruidaeAllOcc_spec <- GruidaeAllOcc_spec %>% slice_sample(prop = 0.5)
# 
# distMat <- RdistEarth(x1 = data.matrix(GruidaeAllOcc_spec %>% select(decimalLongitude, decimalLatitude)), miles = FALSE)
