####################################################
#Relationships Between Jobs And The arts: A Statistical Chicken Or The Egg Approach 
#Cristina Y. Sakamoto
#The University of Chicago
#2014
#version 4.0: the only difference between this version and 3.0 is that the missing data
#Fixed some mistakes from version 4. This one is perfect! Only 11% missing data! 
####################################################
#Notes:
#T = total; N = narrow; W = wide 
####################################################
#Change the current working directory to this one:
#setwd("/Users/cristinasakamoto/Documents/R")

#Libraries needed:
library(foreign)
library(gdata)
library(MASS)
library(QuantPsyc)
library(psych)
library(data.table) 

#Limiting the number of digits after the decimal place. 
options(scipen = 999, digits = 4)

##########################################################################################
#READING TABLES
##########################################################################################

data <- read.csv("FULL_DATA_2016-08-24", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")

###############################################################
# MAKING THE VARIABLES FILE FROM CORE AND GEOLYTICS. 
############################################################### 

# 1. GETTING THE POPULATION CENSUS 2000 VARIABLE FROM MERGEB-34.
#Read the .txt Code Data file extracted from SPSS.I selected only the POP2000 case from the MergeB-34.sav file, to make a separate lighter file. The original file and read.spss was giving me an error where the zipcodes would repeat at 44410 until the end, losing half of the data. 
merge_file <- read.csv("pop2000.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
#Change the name of the zipcode column. 
names(merge_file) <- c("zip", "Pop2000")

# 2. GETTING THE CORE DATASET. 
#Read the .txt Code Data file extracted from SPSS.
core <- read.csv("CoreData.csv", header = TRUE, sep = ",", na.strings = "NA")
#Change the name of the zipcode column
names(core)[1] <- "zip"

# 3. GETTING THE GEOLYTICS DATASET. 
geo <- read.csv("Geolytics.txt", header = TRUE, sep = ",", na.strings = "NA")
#Change the name of the zipcode column
names(geo)[1] <- "zip"

# 4. MERGING ALL THE THREE DATASETS INTO ONE.
merge_file <- merge(merge_file, core, by = "zip", all = TRUE)
merge_file <- merge(geo, merge_file, by = "zip", all = FALSE)

# 5. CLEANING COLUMNS. 
merge_file$population1990 <- NULL

# 6. Dropping the zip codes that are not real
merge_file <- merge_file[!(merge_file$zip < 1001), ]
merge_file <- merge_file[!(merge_file$zip == 99999), ]

# 7. ADDING THE NAMES OF LOCATIONS TO THE FILE
zip.names <- read.csv("zip_names.csv", header = TRUE, sep = ",", na.strings = "NA")
zip.names <- zip.names[c("zip", "City", "State", "Location")]
merge_file <- merge(merge_file, zip.names, by="zip", all = FALSE)

# 8.WRITING THE VARIABLES DATASET. 
#This file contains all the variables from core, geolytics, and the pop2000 from the mergeb-34.sav file. 
write.csv(merge_file, file=paste("variables",Sys.Date(),sep = "_"), na = "", row.names=FALSE)

# 9. MERGING ALL DATA FRAMES TOGETHER
data <- merge(data, merge_file, by = "zip", all = FALSE)
# data <- merge(data, est_df, by="zip")
# data <- merge(data, ind_df, by="zip")

##########################################################################################
##EXCLUDING ROWS WITH MISSING DATA
##########################################################################################
#This code creates a new column showing if there is missing data (FALSE) or not (TRUE). 
data$missing <- complete.cases(data)

#Excluding the rows that have missing data, to create a new data frame. N=36944. 
datamiss <- subset(data, data$missing == "TRUE")

#To confirm that there are no missing data in this new data frame:
sum(is.na(datamiss))
#or
1 - (sum(complete.cases(data) / nrow(data)))
1 - (sum(complete.cases(datamiss) / nrow(datamiss)))

#Cleaning some columns. 
data$missing <- NULL
datamiss$missing <- NULL
data$X <- NULL
data$X.x <- NULL
data$X.y <- NULL

# ###############################################################
# #WRITING TABLES
# ###############################################################
# 
# write.csv(data, file=paste("zip_arts", Sys.Date(),sep = "_"))
# write.csv(datamiss, file=paste("zip_arts_clean",Sys.Date(),sep = "_"))
# 
# ###############################################################
# #WRITING TO STATA
# ###############################################################
# write.dta(data, "data.dta")
# write.dta(datamiss, "datamiss.dta")
#
# ###############################################################
# #TRANSFORM WIDE FORMAT TO LONG FORMAT
# ###############################################################
# library(reshape)
# 
# data.long <- reshape(data, varying=c(2:103), direction="long", idvar="zip", sep="_")
# datamiss.long <- reshape(datamiss, varying=c(2:103), direction="long", idvar="zip", sep="_")
# write.csv(data.long, file=paste("data.long",Sys.Date(),sep = "_"))
# write.csv(datamiss.long, file=paste("datamiss.long",Sys.Date(),sep = "_"))
#  
# ###############################################################
# # ORDERING COLUMNS BY NAME IN DATA FRAME. 
# ###############################################################
# inorder <- data[, order(names(data))]
# 
# sink("results.txt")
# describe(inorder)
# sink()
# 
# ###############################################################
# # EVALUATING DATASET ON CONDITION
# ###############################################################
# 
# #Subset from data if population is bigger than zero.
# pos_pop <- subset(data, Pop1990 > 0)
# 
# #Exclude row.names column
# rownames(pos_pop) <- NULL
# 
# #Calculate the percentage of missing data
# 1 - (sum(complete.cases(data)) / nrow(data))
# 
# #Look for N = 31803
# describe(pos_pop$zip)
# 
# #Number of rows, should be n=31906
# nrow(pos_pop) 
# 
# ##########################################################################################
# ##SCALING THE DATA FRAME 
# ##########################################################################################
# #TO OBTAIN A SCALED DATA FRAME. IF THE AS.DATA.FRAME IS NOT USED, THEN IT WILL BECOME A LARGE MATRIX, AND HARD TO WORK ON.
# #cbind keeps the zip code column intact, while lapply scales the other columns. 
# data.scale <- cbind(data[1], lapply(data[2:112], scale))
# x <- cbind(datamiss[1], lapply(datamiss[2:112], scale))
# 
# write.csv(y, file=paste("DATA_SCALED_all",Sys.Date(),sep = "_"))
# write.csv(x, file=paste("DATA_SCALED_clean",Sys.Date(),sep = "_"))
# 

