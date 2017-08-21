####################################################
# Relationships Between Jobs And The arts: A Statistical Chicken Or The Egg Approach 
# Cristina Y. Sakamoto
# The University of Chicago
# 2014
# version 4.0: the only difference between this version and 3.0 is that the missing data
# Fixed some mistakes from version 4. This one is perfect! Only 11% missing data! 
####################################################
# Notes:
# T = total; N = narrow; W = wide 
####################################################
# Change the current working directory to this one:
# setwd("/Users/cristinasakamoto/Documents/R")
####################################################
# The resulting product of the for loop below is the standard chicken and egg dataset. 
# Arts narrow, arts wide, total jobs 
####################################################
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

# Get the name of every datafile in the following folder and record it. 
csvfiles <- list.files("/home/cysakamoto/Data/raw_data")

# for each file in the csvfiles list, run the following actions. 
for(csvfile in csvfiles) {
  
  # read the file
  tmp <- read.csv(csvfile, header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
  
  # grab the name of the file as a string
  tmp_name <- as.character(csvfile)
  
  # split the name of the file according to the location of the . 
  tmp_name <- strsplit(tmp_name, "\\.") [[1]]
  
  # print the name of the file, so see the year
  print(tmp_name[1])
  
  #Creating a name for the variables columns
  tmp_jobs_name <- paste("jobs_total", tmp_name[1], sep="_")
  tmp_narrow_name <- paste("arts_narrow", tmp_name[1], sep="_")
  tmp_wide_name <- paste("arts_wide", tmp_name[1], sep="_")
  tmp_jnarrow_name <- paste("jobs_narrow", tmp_name[1], sep="_")
  tmp_jwide_name <- paste("jobs_wide", tmp_name[1], sep="_")
  
  #change the columns names all to lower case (to avoid errors)
  colnames(tmp) <- tolower(colnames(tmp))
  
  # turn the strings in naics to numeric
  tmp$naics <- as.numeric(as.character(tmp$naics))
  
  # get all the rows where naics is a number, not a dot
  tmp <- subset(tmp, naics > 0)
  
  # sum the estimations by zip code and by naics code 
  tmp$jobs <- (tmp$n1_4 * 3) + (tmp$n5_9 * 7) + (tmp$n10_19 * 15) + (tmp$n20_49 * 35) + (tmp$n50_99 * 75) + (tmp$n100_249 * 175) + 
    (tmp$n250_499 * 375) + (tmp$n500_999 * 750) + (tmp$n1000 * 2000)
  
  # retain only the zip code, naics, and number of jobs columns 
  tmp <- tmp[c("zip", "naics", "jobs")]
  
  # aggregate the total number of jobs, by sum 
  tmp_total <- aggregate(tmp$jobs, by=list(tmp$zip), sum)
  
  # change the name of the variables 
  names(tmp_total) <- c("zip", tmp_jobs_name)
  
  # getting the arts narrows jobs, by using the naics categories according to the years
  if (tmp_name[1] == "zbp1998" | tmp_name[1] == "zbp1999" | tmp_name[1] == "zbp2000" | tmp_name[1] == "zbp2001") {
    tmp_narrow <- tmp[tmp$naics %in% narrow_1998, ] 
  } else if (tmp_name[1] == "zbp2002" | tmp_name[1] == "zbp2003" | tmp_name[1] == "zbp2004" | tmp_name[1] == "zbp2005" | tmp_name[1] == "zbp2006") {
    tmp_narrow <- tmp[tmp$naics %in% narrow_2002, ]
  } else if (tmp_name[1] == "zbp2007" | tmp_name[1] == "zbp2008" | tmp_name[1] == "zbp2009" | tmp_name[1] == "zbp2010" | tmp_name[1] == "zbp2011") {
    tmp_narrow <- tmp[tmp$naics %in% narrow_2007, ]
  } else if (tmp_name[1] == "zbp2012" | tmp_name[1] == "zbp2013" | tmp_name[1] == "zbp2014" | tmp_name[1] == "zbp2015") {
    tmp_narrow <- tmp[tmp$naics %in% narrow_2012, ] }    
  
  # aggregating the number of jobs by zip code for the arts narrow
  tmp_narrow <- aggregate(tmp_narrow$jobs, by=list(tmp_narrow$zip), sum)
  
  # change the name of the variables 
  names(tmp_narrow) <- c("zip", tmp_narrow_name) 
  
  # getting the arts wide jobs, by using the naics categories according to the years
  if (tmp_name[1] == "zbp1998" | tmp_name[1] == "zbp1999" | tmp_name[1] == "zbp2000" | tmp_name[1] == "zbp2001") {
    tmp_wide <- tmp[tmp$naics %in% wide_1998, ] 
  } else if (tmp_name[1] == "zbp2002" | tmp_name[1] == "zbp2003" | tmp_name[1] == "zbp2004" | tmp_name[1] == "zbp2005" | tmp_name[1] == "zbp2006") {
    tmp_wide <- tmp[tmp$naics %in% wide_2002, ]
  } else if (tmp_name[1] == "zbp2007" | tmp_name[1] == "zbp2008" | tmp_name[1] == "zbp2009" | tmp_name[1] == "zbp2010" | tmp_name[1] == "zbp2011") {
    tmp_wide <- tmp[tmp$naics %in% wide_2007, ]
  } else if (tmp_name[1] == "zbp2012" | tmp_name[1] == "zbp2013" | tmp_name[1] == "zbp2014"| tmp_name[1] == "zbp2015") {
    tmp_wide <- tmp[tmp$naics %in% wide_2012, ] }    
  
  # aggregating the number of jobs by zip code for the arts wide
  tmp_wide <- aggregate(tmp_wide$jobs, by=list(tmp_wide$zip), sum)
  
  # change the name of the variables 
  names(tmp_wide) <- c("zip", tmp_wide_name) 
  
  # merging the total and wide dataframes into a final dataframe 
  tmp_final <- merge(tmp_total, tmp_wide, by="zip", all=TRUE)
  
  # merging the final dataframe with the narrow 
  tmp_final <- merge(tmp_final, tmp_narrow, by="zip", all=TRUE)
  
  # Finding the number of non arts jobs wide.
  tmp_final$wide <- tmp_final[,2] - tmp_final[,3]
  colnames(tmp_final)[colnames(tmp_final) == "wide"] <- tmp_jwide_name

  # Finding the number of non arts jobs narrow
  tmp_final$narrow <- tmp_final[,2] - tmp_final[,4]
  colnames(tmp_final)[colnames(tmp_final) == "narrow"] <- tmp_jnarrow_name
  
  # saving and merging all the years together 
  if (csvfile == "zbp1998.txt") {
    data <- tmp_final
  } else {
    data <- merge(data, tmp_final, by=c("zip"), sort=TRUE, all=TRUE)} 
}

# writing the final csv file 
write.csv(data, file=paste("zip_arts", Sys.Date(),sep = "_"), na = "", row.names=FALSE, append = TRUE, sep = ",")

# Getting some years for the maps 
subdata <- data[, c("zip", "arts_wide_zbp2000", "jobs_wide_zbp2000", "arts_wide_zbp2005", "jobs_wide_zbp2005", "arts_wide_zbp2010", "jobs_wide_zbp2010", "arts_wide_zbp2015", "jobs_wide_zbp2015")]
write.csv(subdata, file=paste("zip_arts_5", Sys.Date(),sep = "_"), na = "", row.names=FALSE, append = TRUE, sep = ",")
subdata <- data[, c("zip", "jobs_wide_zbp2000", "jobs_wide_zbp2005", "jobs_wide_zbp2010", "jobs_wide_zbp2015")]


library(reshape)

names(subdata) <- c("zip", "arts_2000", "arts_2005", "arts_2010", "arts_2015")
data2 <- reshape(subdata, varying=c(2:5), direction="long", idvar = "zip", sep="_", timevar="time")
write.csv(data2, file=paste("jobs_time", Sys.Date(),sep = "_"), na = "", row.names=FALSE, append = TRUE, sep = ",")
