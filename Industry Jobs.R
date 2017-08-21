####################################################
# Which industries had the biggest decrease in number of jobs? And where? 
# Cristina Y. Sakamoto
# The University of Chicago
# 2017
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

####################################################

# Setting the folder and getting the name of every datafile in the following folder and record it. 
# Full data:
setwd("/home/cysakamoto/Data/raw_data")
csvfiles <- list.files("/home/cysakamoto/Data/raw_data")

# Sample data:
setwd("/home/cysakamoto/Data/sample_data")
csvfiles <- list.files("/home/cysakamoto/Data/sample_data")

# for each file in the csvfiles list, run the following actions. 
for (csvfile in csvfiles) {
  
  ######
  # Creating names. Getting the year. 
  # grab the name of the file as a string
  tmp_name <- as.character(csvfile)
  
  # split the name of the file according to the location of the . 
  tmp_name <- strsplit(tmp_name, "\\.") [[1]]
  
  # print the name of the file, so see the year
  print(tmp_name[1])
  ######
  
  # read the file
  tmp <- read.csv(csvfile, header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
  
  #change the columns names all to lower case (to avoid errors)
  colnames(tmp) <- tolower(colnames(tmp))
  
  # Removing all the dashes from the column
  tmp$industries <- gsub("-", "", tmp$naics)
  
  # turn the strings in naics to numeric
  tmp$industries <- as.numeric(as.character(tmp$industries))
  
  # get all the rows where naics is a number, not a dot
  tmp <- subset(tmp, industries < 90)
  
  # Need to change the codes for some industries: Combining the codes into the lowest value. 
  # 31-33	Manufacturing
  tmp$industries[tmp$industries==32] <- 31
  tmp$industries[tmp$industries==33] <- 31
  # 44-45	Retail Trade
  tmp$industries[tmp$industries==45] <- 44
  # 48-49	Transportation and Warehousing
  tmp$industries[tmp$industries==49] <- 48
  
  # sum the estimations by zip code and by naics code 
  tmp$jobs <- (tmp$n1_4 * 3) + (tmp$n5_9 * 7) + (tmp$n10_19 * 15) + (tmp$n20_49 * 35) + (tmp$n50_99 * 75) + (tmp$n100_249 * 175) + 
    (tmp$n250_499 * 375) + (tmp$n500_999 * 750) + (tmp$n1000 * 2000)
  
  # retain only the zip code, naics, and number of jobs columns 
  tmp <- tmp[c("zip", "industries", "jobs")]
  
  # aggregate the total number of jobs, by sum 
  tmp_total <- aggregate(tmp$jobs, by=list(tmp$industries), sum)
  
  # change the name of the variables 
  names(tmp_total) <- c("industries", paste("totaljobs_", tmp_name[1], sep=""))
  
  # saving and merging all the years together 
  if (csvfile == "zbp1998.txt") {
    data <- tmp_total
  } else {
    data <- merge(data, tmp_total, by=c("industries"), all=TRUE)}
}


######
# Adding the name of the sectors to the table 
sector <- read.table('/home/cysakamoto/Data/NAICS_Sectors', sep="\t", header=TRUE)

# Changing the name of the variables in sector 
names(sector) <- c("industries", "sector")

# Merging data with the names of sectors 
data <- merge(data, sector, by="industries", all=TRUE)

###### 
# Using 1998 as the base year 
data_base <- data
data_base[3:ncol(data_base)] <- data_base[3:ncol(data_base)] - data_base[,2]
data_base <- merge(data_base, sector, by="industries", all=TRUE)

######
n_columns <- seq(3, 18)

data_delta <- data
data_delta2 <- data

for (n in n_columns){
  data_delta2[n] <- data_delta[n] - data_delta[(n-1)]
} 

data_delta2 <- merge(data_delta2, sector, by="industries", all=TRUE)


data_delta$pct99_98 <- NULL
data_delta$difference <- NULL

data_delta2$pct99_98 <- NULL
data_delta2$difference <- NULL

##### 

n_columns <- seq(18, 3)

data_pct <- data

for (n in n_columns){
  data_pct[n] <- (data_pct[n] - data_pct[(n-1)])*100 / data_pct[(n-1)]
  print(data_pct[n])
} 

data_pct <- merge(data_pct, sector, by="industries", all=TRUE)

######

n_columns <- seq(18, 3)

data_pct98 <- data

for (n in n_columns){
  data_pct98[n] <- (data_pct98[n] - data_pct98[(2)])*100 / data_pct98[(2)]
} 

data_pct98 <- merge(data_pct98, sector, by="industries", all=TRUE)



###### 
# Calculating growth by percent 

data$pct99_98 <- ((data$totaljobs_zbp1999 - data$totaljobs_zbp1998) / data$totaljobs_zbp1998) * 100



######
# Calculating differences between years 
data$difference <- data$totaljobs_zbp2014 - data$totaljobs_zbp1998



######
# writing the final csv file 
write.csv(data, file=paste("sector_jobs", Sys.Date(),sep = "_"), na = "", row.names=FALSE, append = TRUE, sep = ",")
