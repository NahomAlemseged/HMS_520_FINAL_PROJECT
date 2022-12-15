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
library(rigr)
library(ggpubr)
library(dbplyr)
library(data.table)
library(tidyverse)
library(table1)
source('fuction.R')

############################################################################
ibrary(readstata13)
filename.2016<- "C:/Users/dyada/Desktop/HMS520-Autumn2022/HMS_520_FINAL_PROJECT/data/ETBR71FL.DTA"
survey <- read.dta13(filename.2016, generate.factors = TRUE)   

#-----Data code under five dataset
# using data.table package
survey.1<-as.data.table(survey)
class(survey.1)
under_five1<-survey.1[b19 <60]

#using dbplyr package
under_five<-survey[survey$b19 <60,]

#####Variables information#####

#region = v024, clust_no = v001, bir_int = b11, 
#num_anc = m14, fac_del = m15, outcome = b5,type_res = v025, 
#size_child = m18)
#strata (v023), cluster (v001), and household (v002)
#Survey weight (v025)
#Date of child's birth in CMC format (b3)
#Indicator for death of child (b5)

#

#To check up missing for outcome(b5) and select variables
birth<-under_five[, c('v001', 'v002', 'v023','v024', 'v025', 'b3','b5','b11',
                      'm15','m18')]
df<-birth[complete.cases(birth$b5), ]
df<-birth

# Code as a factor
df$b5<-as.factor(df$b5)
df$v024<-as.factor(df$v024)
df$v025<-as.factor(df$v025)
# code place of delivery (m15)
X <- c('home',"respondent's home",'other home','public sector','government hospital',
       'government health center','government health post',
       'other public sector','private sector','private hospital','private clinic',
       'other private sector','ngo','ngo: health facility','ngo: other health facility','other')

df$m15 <- factor(df$m15, levels = c(X),
                 labels = c(0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1))

# code size of Child (m18)
df$m18 <- factor(df$m18, levels = c(unique(df$m18)),
                 labels = c(3,4,5,2,1,NA))
df$m18<-as.factor(df$m18)
#
# Replace missing for size of Child (m18)

#df$m18 %>% 
#mutate_if(is.factor, funs(replace(.,is.na(.), Mode(na.omit(.)))))

#mutate(m18 = if_else(is.na(m18), calc_mode(m18), m18))

# GROUP using cut for birth interval

df$b11 <- cut(df$b11, breaks = c(-Inf,24,Inf), labels = c(0,1), include.lowest = FALSE, an.rm = TRUE)
df$b11<-as.factor(df$b11)

# Replace missing for birth interval(b11)
df$b11[is.na(df$b11)]<-sample(c("1","0"),sum(is.na(df$b11)),replace=TRUE,
                              prob=c(sum(na.omit(df$b11=="1"))/sum(!is.na(df$b11)),sum(na.omit(df$b11=="0"))/sum(!is.na(df$b11)))) 


#lable for the variables

label(df$b5)<- "Outcome"
label(df$v024)<- "Region"
label(df$v025)  <- "Residence"
label(df$b11)  <- "Birth interval"
label(df$m15)  <- "Place of delivery"

df$b5 <-factor(df$b5, 
               levels=c("no","yes"),
               labels=c("Dead","Alive"))
df$v025 <-factor(df$v025, 
                 levels=c("urban","rural"),
                 labels=c("Urban","Rural"))
df$b11 <-factor(df$b11, 
                levels=c(0,1),
                labels=c("Less than 2 years","More than 2 years"))
df$m15 <-factor(df$m15, 
                levels=c(0,1),
                labels=c("Out of facility","Facility"))

# Descriptive statistics in table 1
table1(~ v024 + v025 + b11 + m15| b5, data=df, 
       overall = "Total")


#Create a function to compute the p-value for continuous or categorical variables.

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}
table1(~ v024 + v025 +b11 +m15 | b5, data=df, 
       overall = "Total", extra.col=list(`P-value`=pvalue))

###the binary logistic model
df$v024 <- relevel(df$v024, ref = "addis adaba")
logistic_model <- glm(b5 ~ v025+ v024 + b11 + m15, family = binomial(), df)
summary(logistic_model)
# Checking the model
summary(logistic_model)
# Predicting in the test dataset
pred_prob <- predict(logistic_model, test, type = "response")



#######################################################################################################

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

