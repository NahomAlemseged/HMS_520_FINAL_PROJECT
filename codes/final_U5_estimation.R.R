########################################################################
##Title: Under five mortality rate using EDHS 2016 datasets
#######################################################################

setwd("C:/Users/kemalo/Desktop/HMS520-Autumn2022/HMS_520_FINAL_PROJECT/codes")

#Second, we load the package and the necessary data
library(sp) # To provide classes and methods for spatial data types
library(SUMMER)# To provides methods for spatial and spatio-temporal smoothing of demographic and health indicators using survey data
library(ggplot2) # To Create Elegant Data Visualisations Using the Grammar of Graphics
library(gridExtra) # To Provides a number of user-level functions to work with "grid" graphics
library(patchwork) # To make it ridiculously simple to combine separate ggplots into the same graphic
library(rdhs) # for management and analysis of Demographic and Health Survey (DHS) data
library(haven) # Import foreign statistical formats into R
library(rgdal) # To provides bindings to the Geospatial Data Abstraction Library 
library(Matrix) #  To provides classes for real (stored as double precision), logical and so-called "pattern" (binary) dense and sparse matrices.
library(parallel)# can perform tasks in parallel by providing the ability to allocate cores to R
library(foreach)# To Provides Foreach Looping Construct
library(INLA) # Do approximate Bayesian inference for Latent Gaussian Models
library(readstata13)# Function to read and write the 'Stata' file format.
source('fuction.R')




##----Data prepare##

#The DHS data can be obtained from the DHS program website at https://dhsprogram.com/data/dataset/ETHIOPIA_Standard-DHS_2016.
#For the analysis of U5MR, we will use the Births Recode in .dta format
#The map files for this DHS can be freely downloaded from http://spatialdata.dhsprogram.com/boundaries/.
#With both the DHS birth record data and the corresponding shapefiles saved in the local directory. 
#We can load them into with packages readstata13 and rgdal. 
#We also automatically generates the spatial adjacency matrix Amat using the function getAmat().

## ------------------------------------------------------------------------


##----- Prepare EDHS 2016 Dataset----

##-----Import the Births Recode in .dta format from Sources---------

filename.2016<- "C:/Users/kemalo/Desktop/Health matrics files/HSM 520/ETBR71DT/ETBR71FL.DTA"
survey2016 <- read.dta13(filename.2016, generate.factors = TRUE)   

##----Import the corresponding shapefiles saved in the local directory-----
mapfilename.2016 <- "C:/Users/kemalo/Desktop/Health matrics files/HSM 520/map2016/shps/sdr_subnational_boundaries.shp"
geo.2016 <- readOGR(mapfilename.2016, verbose = FALSE)

## ----we now load the GPS location of each clusters and map them to the corresponding regions----
loc.2016 <- readOGR("C:/Users/kemalo/Desktop/Health matrics files/HSM 520/ETGE71FL/ETGE71FL.shp", verbose = FALSE)

#######################################################################################################



Amat.2016 <- getAmat(geo.2016, geo.2016$REGNAME)
loc.dat.2016 <- data.frame(cluster = loc.2016$DHSCLUST, long = loc.2016$LONGNUM, lat = loc.2016$LATNUM)
gps.2016 <- mapPoints(loc.dat.2016, geo = geo.2016, long = "long", lat = "lat", names = c("REGNAME"))
colnames(gps.2016)[4] <- "region"
sum(is.na(gps.2016$region))

## ---- we remove these clusters without GPS coordinates----
unknown_cluster.2016 <- gps.2016$cluster[which(is.na(gps.2016$region))]
gps.2016 <- gps.2016[gps.2016$cluster %in% unknown_cluster.2016 == FALSE, ]
survey2016 <- survey2016[survey2016$v001 %in% unknown_cluster.2016 == FALSE, ]
survey2016 <- merge(survey2016, gps.2016[, c("cluster", "region")], by.x = "v001", by.y = "cluster", all.x = TRUE)
survey2016$v024 <- survey2016$region


##-----Prepare person-month data----
# We first demonstrate the method that smooths the direct estimates of subnational-level U5MR. For this analysis, we consider the 8 Admin-1 region groups. In order to calculate the direct estimates of U5MR, we need the full birth history data in the format so that every row corresponds to a birth and columns that contain:

#Indicators corresponding to survey design, e.g., strata (v023), cluster (v001), and household (v002)
#Survey weight (v025)
#Date of interview in century month codes (CMC) format, i.e., the number of the month since the beginning of 1990 (v008)
#Date of child's birth in CMC format (b3)
#Indicator for death of child (b5)
#Age of death of child in months (b7) #

#We reorganize the data into the 'person-month' format with getBirths function and reorder the columns for better readability.
## ------------------------------------------------------------------------
##----EDHS 2016----
dat.2016 <- getBirths(data = survey2016, strata = c("v023"),  year.cut = seq(1982, 2012, by = 1)) 
dat.2016 <- dat.2016[,c("v001", "v002",  "v024","time","age","v005","strata","died")]
colnames(dat.2016) <- c("clustid", "id", "region","time","age","weights","strata","died")
years <- levels(dat.2016$time)
head(dat.2016)
###
direct <- getDirect(births =dat.2016, years = years, regionVar = "region",
                    timeVar = "time", clusterVar = "~clustid + id", ageVar = "age", weightsVar = "weights", geo.recode = NULL)

##################################################
# PLOTTING THE DIRECT MORTALITY ESTIMATION
##################################################
dir_plot(direct1)

##################################################
# PLOTTING THE DIRECT SMOOTHED ESTIMATION
##################################################
dir_smooth(direct)
##################################################

# SUBNATIONAL ESTIMATION

#############################################################
# PLOTTING THE SMOOTHED SUBTNATIONAL MORTALITY ESTIMATION
############################################################

fit3 <- smoothDirect(data = direct, Amat = Amat.2016, year_label = years, year_range = c(1982, 2011), time.model = "rw2", type.st = 4, m = 1)
out3 <- getSmoothed(fit3)

subnat_plot(direct)

#############################################################
# PLOTTING THE SPATIO-TEMPORTAL ESTIMATION (MAP)
############################################################

map_plot(direct)

##############################  END  #######################################

