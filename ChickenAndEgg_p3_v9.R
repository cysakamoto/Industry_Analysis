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

#Preparing variables:
#For years: a sequence of every number from 1998 to 2013. 
years <- seq(1998, 2014)
#For years of naics codes changes:
naics_years <- c("1998", "2002", "2007", "2012")
#Arts Wide NAICS in 1998 codes. 
wide_1998 <- c(323117, 334612, 339992, 443130, 451140, 451211, 451220, 453920, 487110, 487210, 487990, 511110, 511120, 511130, 512110, 512120, 512131, 512191, 512199, 512210, 512220, 512230, 512240, 512290, 513111, 513112, 513120, 513210, 514110, 514120, 532292, 532490, 541310, 541340, 541370, 541410, 541420, 541430, 541490, 541830, 541840, 541850, 541870, 541890, 541921, 541922, 561591, 561920, 561990, 611610, 611620, 711110, 711120, 711130, 711190, 711211, 711219, 711310, 711320, 711410, 711510, 712110, 712120, 712130, 712190, 713110, 713120, 713210, 713290, 713920, 713940, 713990, 722110, 722211, 722212, 722213, 722310, 722320)
#Arts Wide NAICS in 2002 codes. 
wide_2002 <- c(323117, 334612, 339992, 443130, 451140, 451211, 451220, 453920, 487110, 487210, 487990, 511110, 511120, 511130, 512110, 512120, 512131, 512191, 512199, 512210, 512220, 512230, 512240, 512290, 515111, 515112, 515120, 515210, 519110, 519120, 532292, 532490, 541310, 541340, 541370, 541410, 541420, 541430, 541490, 541830, 541840, 541850, 541870, 541890, 541921, 541922, 561591, 561920, 561990, 611610, 611620, 711110, 711120, 711130, 711190, 711211, 711219, 711310, 711320, 711410, 711510, 712110, 712120, 712130, 712190, 713110, 713120, 713210, 713290, 713920, 713940, 713990, 722110, 722211, 722212, 722213, 722310, 722320)
#Arts Wide NAICS in 2007 codes. 
wide_2007 <- c(323117, 334612, 339992, 443130, 451140, 451211, 451220,453920, 487110, 487210, 487990, 511110, 511120, 511130, 512110, 512120, 512131, 512191, 512199, 512210, 512220, 512230, 512240, 512290, 515111, 515112, 515120, 515210, 519110, 519120, 532292, 532490, 541310, 541340, 541370, 541410, 541420, 541430, 541490, 541830, 541840, 541850, 541870, 541890, 541921, 541922, 561591, 561920, 561990, 611610, 611620, 711110, 711120, 711130, 711190, 711211, 711219, 711310, 711320, 711410, 711510, 712110, 712120, 712130, 712190, 713110, 713120, 713210, 713290, 713920, 713940, 713990, 722110, 722211, 722212, 722213, 722310, 722320)
#Arts Wide NAICS in 2012 codes. 
wide_2012 <- c(323117, 334614, 339992, 443142, 451140, 451211, 443142, 453920, 487110, 487210, 487990, 511110, 511120, 511130, 512110, 512120, 512131, 512191, 512199, 512210, 512220, 512230, 512240, 512290, 515111, 515112, 515120, 515210, 519110, 519120, 532292, 532490, 541310, 541340, 541370, 541410, 541420, 541430, 541490, 541830, 541840, 541850, 541870, 541890, 541921, 541922, 561591, 561920, 561990, 611610, 611620, 711110, 711120, 711130, 711190, 711211, 711219, 711310, 711320, 711410, 711510, 712110, 712120, 712130, 712190, 713110, 713120, 713210, 713290, 713920, 713940, 713990, 722511, 722513, 722514, 722515, 722310, 722320)
#Arts Narrow NAICS in 1998 codes. 
narrow_1998 <- c(334612, 512191, 512199, 512210, 512230, 512290, 513111, 513112, 514110, 514120, 541310, 541340, 541410, 541420, 541430, 541490, 611610, 611620, 711110, 711120, 711130, 711190, 711219, 711310, 711320, 711410, 711510, 712110, 712190, 713210, 713290, 713940, 713990)
#Arts Narrow NAICS in 2002 codes. 
narrow_2002 <- c(334612, 512191, 512199, 512210, 512230, 512290, 515111, 515112, 519110, 519120, 541310, 541340, 541410, 541420, 541430, 541490, 611610, 611620, 711110, 711120, 711130, 711190, 711219, 711310, 711320, 711410, 711510, 712110, 712190, 713210, 713290, 713940, 713990)
#Arts Narrow NAICS in 2007 codes. 
narrow_2007 <- c(334612, 512191, 512199, 512210, 512230, 512290, 515111, 515112, 519110, 519120, 541310, 541340, 541410, 541420, 541430, 541490, 611610, 611620, 711110, 711120, 711130, 711190, 711219, 711310, 711320, 711410, 711510, 712110, 712190, 713210, 713290, 713940, 713990)
#Arts Narrow NAICS in 2012 codes. 
narrow_2012 <- c(334614, 512191, 512199, 512210, 512230, 512290, 515111, 515112, 519110, 519120, 541310, 541340, 541410, 541420, 541430, 541490, 611610, 611620, 711110, 711120, 711130, 711190, 711219, 711310, 711320, 711410, 711510, 712110, 712190, 713210, 713290, 713940, 713990)

csvfiles <- list.files("/home/cysakamoto/Data/sample_data")

for(csvfile in csvfiles) {
  tmp <- read.csv(csvfile, header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
  tmp_name <- as.character(csvfile)
  tmp_name <- strsplit(tmp_name, "\\.") [[1]]
  print(tmp_name[1])
  colnames(tmp) <- tolower(colnames(tmp))
  tmp$naics <- as.numeric(as.character(tmp$naics))
  tmp <- subset(tmp, naics > 0)
  tmp$n1_4 <- tmp$n1_4 * 3
  tmp$n5_9 <- tmp$n5_9 * 7
  tmp$n10_19 <- tmp$n10_19 * 15
  tmp$n20_49 <- tmp$n20_49 * 35
  tmp$n50_99 <- tmp$n50_99 * 75
  tmp$n100_249 <- tmp$n100_249 * 175
  tmp$n250_499 <- tmp$n250_499 * 375
  tmp$n500_999 <- tmp$n500_999 * 750
  tmp$n1000 <- tmp$n1000 * 2000
  tmp$jobs <- tmp$n1_4 + tmp$n5_9 + tmp$n10_19 + tmp$n20_49 + tmp$n50_99 + tmp$n100_249 + tmp$n250_499 + tmp$n500_999 + tmp$n1000
  tmp <- tmp[c("zip", "naics", "jobs")]
  tmp_total <- aggregate(tmp$jobs, by=list(tmp$zip), sum)
  names(tmp_total) <- c("zip", paste("jobs_total", tmp_name[1]))
  if (tmp_name[1] == "zbp1998" | tmp_name[1] == "zbp1999" | tmp_name[1] == "zbp2000" | tmp_name[1] == "zbp2001") {
    tmp_narrow <- tmp[tmp$naics %in% narrow_1998, ] 
  } else if (tmp_name[1] == "zbp2002" | tmp_name[1] == "zbp2003" | tmp_name[1] == "zbp2004" | tmp_name[1] == "zbp2005" | tmp_name[1] == "zbp2006") {
    tmp_narrow <- tmp[tmp$naics %in% narrow_2002, ]
  } else if (tmp_name[1] == "zbp2007" | tmp_name[1] == "zbp2008" | tmp_name[1] == "zbp2009" | tmp_name[1] == "zbp2010" | tmp_name[1] == "zbp2011") {
    tmp_narrow <- tmp[tmp$naics %in% narrow_2007, ]
  } else if (tmp_name[1] == "zbp2012" | tmp_name[1] == "zbp2013" | tmp_name[1] == "zbp2014") {
    tmp_narrow <- tmp[tmp$naics %in% narrow_2012, ]
  }    
  tmp_narrow <- aggregate(tmp_narrow$jobs, by=list(tmp_narrow$zip), sum)
  names(tmp_narrow) <- c("zip", paste("arts_narrow", tmp_name[1])) 
  if (tmp_name[1] == "zbp1998" | tmp_name[1] == "zbp1999" | tmp_name[1] == "zbp2000" | tmp_name[1] == "zbp2001") {
    tmp_wide <- tmp[tmp$naics %in% wide_1998, ] 
  } else if (tmp_name[1] == "zbp2002" | tmp_name[1] == "zbp2003" | tmp_name[1] == "zbp2004" | tmp_name[1] == "zbp2005" | tmp_name[1] == "zbp2006") {
    tmp_wide <- tmp[tmp$naics %in% wide_2002, ]
  } else if (tmp_name[1] == "zbp2007" | tmp_name[1] == "zbp2008" | tmp_name[1] == "zbp2009" | tmp_name[1] == "zbp2010" | tmp_name[1] == "zbp2011") {
    tmp_wide <- tmp[tmp$naics %in% wide_2007, ]
  } else if (tmp_name[1] == "zbp2012" | tmp_name[1] == "zbp2013" | tmp_name[1] == "zbp2014") {
    tmp_wide <- tmp[tmp$naics %in% wide_2012, ]
  }    
  tmp_wide <- aggregate(tmp_wide$jobs, by=list(tmp_wide$zip), sum)
  names(tmp_wide) <- c("zip", paste("arts_wide", tmp_name[1])) 
  tmp_final <- merge(tmp_total, tmp_wide, by="zip", all=TRUE)
  tmp_final <- merge(tmp_final, tmp_narrow, by="zip", all=TRUE)
  if (csvfile == "zbp1998.txt") {
    data <- tmp_final
  } else {
    data <- merge(data, tmp_final, by=c("zip"), all=TRUE)} 
}

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

# 6. ADDING THE NAMES OF LOCATIONS TO THE FILE
zip.names <- read.csv("zip_names.csv", header = TRUE, sep = ",", na.strings = "NA")
zip.names <- zip.names[c("zip", "City", "State", "Location")]
merge_file <- merge(merge_file, zip.names, by="zip", all = FALSE)

# 7.WRITING THE VARIABLES DATASET. 
#This file contains all the variables from core, geolytics, and the pop2000 from the mergeb-34.sav file. 
write.csv(merge_file, file=paste("variables",Sys.Date(),sep = "_", qmethod="Double"))

# 8. MERGING ALL DATA FRAMES TOGETHER
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
# write.csv(data, file=paste("rdata",Sys.Date(),sep = "_"))
# write.csv(datamiss, file=paste("rdatamiss",Sys.Date(),sep = "_"))
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
# ##########################################################################################
# #READING TABLES
# ##########################################################################################
# 
# data <- read.csv("ARTS_VAR_2016-08-24", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
# data <- read.csv("FULL_DATA_2016-08-24", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
# 
# ##########################################################################################
# #WRITING TABLES
# ##########################################################################################
# 
# write.csv(datamiss, file=paste("CE_CORE_GEO_NOMISS",Sys.Date(),sep = "_"))
# write.csv(data.sample, file=paste("Arts_Data",Sys.Date(),sep = "_"))
# write.csv(data, file=paste("Full_Arts_Data",Sys.Date(),sep = "_"))
# write.csv(zip, file=paste("Physical_Zips_Data",Sys.Date(),sep = "_"))
# write.csv(datamiss, file=paste("Full_Arts_Data",Sys.Date(),sep = "_"))
# 
