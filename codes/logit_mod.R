rm(list = ls())
# install.packages('readstata13')
library('readstata13')
library('data.table')
library('tidyverse')
library('dplyr')


setwd("C:/Users/nahomw/Desktop/assignments/First Quarter/data_wrangling/project_U5M/HMS_520_FINAL_PROJECT/data")
df <- read.csv("Prep_data.csv")

#################################################
# MODEL BUILDING
#################################################
df <- setDT(df)
set.seed(1)
#Use 70% of dataset as training set and remaining 30% as testing set
sample(df,0.7)
View(df)

sample <- sample(c(1,0), nrow(df), replace=TRUE, prob=c(0.7,0.3))
df[, "train" := sample]
###############################################################
#   train model
##############################################################
df$region <- as.factor(df$region)
df$fac_del <- as.factor(df$fac_del)
df$type_res <- as.factor(df$type_res)


model <- glm(outcome~region+bir_int+fac_del+type_res+size_child, family="binomial", data=df[train == 1])
model <- glm(outcome~region+bir_int+fac_del+type_res+size_child, family="binomial", data=df)
summary(model)



# train <- data[sample, ]
# test <- data[!sample, ]  
#  
fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))
