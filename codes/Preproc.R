rm(list = ls())
library('readstata13') #library to read
library('data.table')
library('tidyverse')
library('dplyr')
library('mice')

dr <- ("C:/Users/nahomw/Desktop/assignments/First Quarter/data_wrangling/project_U5M/HMS_520_FINAL_PROJECT/data/ETBR71FL.DTA")
setwd("C:/Users/nahomw/Desktop/assignments/First Quarter/data_wrangling/project_U5M/HMS_520_FINAL_PROJECT/data/")
df_gps <- read.csv("gps_data.csv")
df <- read.dta13(dr,generate.factors = TRUE)
df <- setDT(df)
View(df)

temp <- df

#################################################
# filter data for age <60 month old to obtain five years cohort
##############################################
df <- df[(b19 < 60)]
df <- df[ ,.(region = v024, clust_no = v001, bir_int = b11, num_anc = m14, fac_del = m15, outcome = b5,type_res = v025, size_child = m18)]
summary(df)

####################################################
# we can see that a 357 anc-visit is missing for dead class
####################################################
df <- df[, -c("num_anc")]
df_lab <- df

########################################################
# REFACTOR DF
######################################################
# TWO DATASETS ARE TO GENERATE HERE ARE TWO:
#     df: VALUES FACTORED AS NUMBERS
#     df_lab: VALUES AS LABELS 
##########################################################
# generating df
####################################################
### RENAME GROUP (convert to numerical variables)
#######################################################

X <- c(unique(df$region))
df$region <- factor(df$region, levels = X, labels = c(1:c(length(X))))

df$outcome <- factor(df$outcome, levels = c('yes','no'), labels = c(0:1))
df$type_res <- factor(df$type_res, levels = c('urban','rural'), labels = c(0:1))
View(df)
############################################################3
# Group then re-factor
#############################################################
X <- unique(df$fac_del)
X <- c('home',"respondent's home",'other home','public sector','government hospital',
       'government health center','government health post',
       'other public sector','private sector','private hospital','private clinic',
       'other private sector','ngo','ngo: health facility','ngo: other health facility','other')

df$fac_del <- factor(df$fac_del, levels = c(X),
                       labels = c(0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1)
)
###########################################################################
# GROUP using cut for birth interval
#########################################################################
birint <- function(data){
  data$bir_int <- cut(data$bir_int, breaks = c(-Inf,24,Inf), labels = c(0,1), include.lowest = FALSE, na.rm = TRUE)
  return(data)
} 
df <- birint(df)


###########################################################################
# GROUP using cut for size-child
#########################################################################

df$size_child <- factor(df$size_child, levels = c(unique(df$size_child)),
                          labels = c(3,4,5,2,1,NA)
)
##########################################################################
# IMPUTATION FOR MISSING DATA
#########################################

imp <- mice(df, m=5, maxit = 50, method = 'pmm', seed = 500)

df <- complete(imp,5)

print(paste('rows = ',nrow(df), ':', '',sum(is.na(df))))
############################################
# TO BE DELETED
############################################

df <- df[,c(1:4,6:7,5)]
View(df)
####################################################
# Exporting for further analysis
###################################################

##################################################
# df_lab for labeled variables
#################################################d
df_lab <- birint(df_lab)
df_lab$fac_del <- factor(df_lab$fac_del, levels = c(X),
                     labels = c(0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1))
df_lab$fac_del <- factor(df_lab$fac_del, levels = c(0,1),
                         labels = c('out_of_fasc', 'within'))
df_lab$bir_int <- factor(df_lab$bir_int, levels = c(0,1),
                         labels = c('<2', '2+'))    ###this code is repetitive but small line of code,no need to modularize.


imp_1 <- mice(df_lab, m=5, maxit = 50, method = 'pmm', seed = 500)
df_lab <- complete(imp_1,5)
View(df_lab)
##########################################################
############################################
# WRITE (EXPORT)  CSV FILES
############################################
write_csv(df_lab,"Prep_data.csv")
write_csv(df,"logreg_data.csv")



