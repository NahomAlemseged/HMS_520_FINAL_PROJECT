rm(list = ls())
# install.packages('readstata13')
library('readstata13')
library('data.table')
library('tidyverse')
library('dplyr')

ab <- ("C:/Users/nahomw/Desktop/assignments/First Quarter/data_wrangling/project_U5M/HMS_520_FINAL_PROJECT/data/ETBR71FL.DTA")
df <- read.dta13(ab,generate.factors = TRUE)
View(df)
df_viz <- copy(df)
df <- setDT(df)
df_viz <- setDT(df_viz)
df <- df_viz

#################################################
# filter data for age <60 month old.packages
##############################################
df <- df[(b19 < 60)]
df_viz <- df_viz[(b19 < 60)]
df <- df[ ,.(region = v024, clust_no = v001, bir_int = b11, num_anc = m14, fac_del = m15, outcome = b5,type_res = v025, size_child = m18)]
View(df)
summary(df)
########################################
df_no <- df[outcome == 'no'] 
View(df_no)
####################################################
# we can see that a 357 anc-visit is missing for dead class
####################################################
df <- df[, -c("num_anc")]
########################################################
# REFACTOR DF
######################################################
### RENAME GROUP (convert to numerical variables)
#######################################################
temp <- df
X <- c(unique(temp$region))
temp$region <- factor(temp$region, levels = X, labels = c(1:c(length(X))))
temp$outcome <- factor(temp$outcome, levels = c('yes','no'), labels = c(0:1))
temp$type_res <- factor(temp$type_res, levels = c('urban','rural'), labels = c(0:1))
View(temp)
############################################################3
# Group then re-factor
#############################################################
X <- unique(temp$fac_del)
X <- c('home',"respondent's home",'other home','public sector','government hospital',
       'government health center','government health post',
       'other public sector','private sector','private hospital','private clinic',
       'other private sector','ngo','ngo: health facility','ngo: other health facility','other')


temp$fac_del <- factor(temp$fac_del, levels = c(X),
                       labels = c(0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1)
)
###########################################################################
# GROUP using cut for birth interval
#########################################################################
temp$bir_int <- cut(temp$bir_int, breaks = c(-Inf,24,Inf), labels = c(0,1), include.lowest = FALSE, na.rm = TRUE)

###########################################################################
# GROUP using cut for size-child
#########################################################################
temp1 <- temp
temp$size_child <- factor(temp$size_child, levels = c(unique(temp$size_child)),
                          labels = c(3,4,5,2,1,NA)
)
temp <- na.omit(temp)

temp <- temp[,c(1:4,6:7,5)]
View(temp)
write_csv(temp,"Prep_data.csv")





