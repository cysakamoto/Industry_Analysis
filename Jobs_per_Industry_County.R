####################################################
# Which industries had the biggest decrease in number of jobs? And where? 
# Cristina Y. Sakamoto
# The University of Chicago
# 2017
# County Level, so we can use data since 1986. 
# Looking at number of jobs change, and correlation of what kind of sectors go together. 
####################################################

# Setting the folder and getting the name of every datafile in the following folder and record it. 
setwd("/home/cysakamoto/Data/data_county")
csvfiles <- list.files("/home/cysakamoto/Data/data_county")

library(stringr)

for (csvfile in csvfiles) {
  
  ######
  # Creating names and getting the year. 
  # grab the name of the file as a string.
  tmp_name <- as.character(csvfile)
  
  # split the name of the file according to the location of the . 
  tmp_name <- strsplit(tmp_name, "\\.") [[1]]
  
  # print the name of the file, so see the year
  print(tmp_name[1])
  
  # defining the year we're working on
  tmp_year <- str_sub(tmp_name[1], start= -4)
  
  ######
  # Getting the data. 
  # read the file
  tmp <- read.csv(csvfile, header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
  
  ######
  # Some cleaning
  # Change the columns names all to lower case (to avoid errors)
  colnames(tmp) <- tolower(colnames(tmp))
  
  # Keeping the number of digits in each county code consistent.
  tmp$fipstate <- sprintf("%02d", tmp$fipstate)
  tmp$fipscty <- sprintf("%03d", tmp$fipscty)
  
  ######
  # Calculating the estimation of number of jobs by county and sector
  tmp$total_jobs <- (tmp$n1_4*3) + (tmp$n5_9 * 7) + (tmp$n10_19 * 15) + (tmp$n20_49 * 35) + (tmp$n50_99 * 75) + (tmp$n100_249 * 175) +  (tmp$n250_499 * 375) + (tmp$n500_999 * 750) + (tmp$n1000 * 2000)
  
  # Removing all the dashes from the SIC column, and write them in a new column called industries
  if (tmp_year <= 1994) {
    tmp$industries <- gsub("-", "", tmp$sic)
  } else {
    tmp$industries <- gsub("-", "", tmp$naics)
  }
  
  # Turn the strings in naics to numeric
  tmp$industries <- as.numeric(as.character(tmp$industries))
  
  # Keeping only important variables
  tmp <- tmp[c("fipstate", "fipscty", "total_jobs", "industries")]
  
  # Delete all the rows where the SIC code is a broad code, not a specific code 
  if (tmp_year <= 1994) {
    # Removing the detailed codes
    tmp <- tmp[!(tmp$industries > 100), ] 
    # Replacing the codes of the industries to numeric codes 
    tmp$industries[tmp$industries > 01 & tmp$industries < 09] <- 01
    tmp$industries[tmp$industries >= 10 & tmp$industries < 14] <- 10
    tmp$industries[tmp$industries >= 15 & tmp$industries < 17] <- 15
    tmp$industries[tmp$industries >= 20 & tmp$industries < 39] <- 20
    tmp$industries[tmp$industries >= 40 & tmp$industries < 49] <- 40
    tmp$industries[tmp$industries >= 50 & tmp$industries < 51] <- 50
    tmp$industries[tmp$industries >= 52 & tmp$industries < 59] <- 52
    tmp$industries[tmp$industries >= 60 & tmp$industries < 67] <- 60
    tmp$industries[tmp$industries >= 70 & tmp$industries < 89] <- 70
    tmp$industries[tmp$industries >= 91 & tmp$industries < 99] <- 91
    tmp$sector[tmp$industries > 01 & tmp$industries < 09] <- "Agriculture, Forestry & Fishing"
    tmp$sector[tmp$industries >= 10 & tmp$industries < 14] <- "Mining"
    tmp$sector[tmp$industries >= 15 & tmp$industries < 17] <- "Construction"
    tmp$sector[tmp$industries >= 20 & tmp$industries < 39] <- "Manufacturing"
    tmp$sector[tmp$industries >= 40 & tmp$industries < 49] <- "Transportation, Communications, Electric, Gas & Sanitary Services"
    tmp$sector[tmp$industries >= 50 & tmp$industries < 51] <- "Wholesale Trade"
    tmp$sector[tmp$industries >= 52 & tmp$industries < 59] <- "Retail Trade"
    tmp$sector[tmp$industries >= 60 & tmp$industries < 67] <- "Finance, Insurance & Real Estate"
    tmp$sector[tmp$industries >= 70 & tmp$industries < 89] <- "Services"
    tmp$sector[tmp$industries >= 91 & tmp$industries < 99] <- "Public Administration"
  } else {
    # Removing the detailed codes
    tmp <- tmp[!(tmp$industries > 100), ] 
    # Replacing the codes of the industries to numeric codes 
    tmp$industries[tmp$industries >= 31 & tmp$industries < 34] <- 31
    tmp$industries[tmp$industries >= 44 & tmp$industries < 46] <- 44
    tmp$industries[tmp$industries >= 48 & tmp$industries < 49] <- 48
    tmp$sector[tmp$industries == 11] <- "Agriculture, Forestry, Fishing and Hunting"
    tmp$sector[tmp$industries == 21] <- "Mining"
    tmp$sector[tmp$industries == 22] <- "Utilities"
    tmp$sector[tmp$industries == 23] <- "Construction"
    tmp$sector[tmp$industries == 31] <- "Manufacturing"
    tmp$sector[tmp$industries == 42] <- "Wholesale Trade"
    tmp$sector[tmp$industries == 44] <- "Retail Trade"
    tmp$sector[tmp$industries == 48] <- "Transportation and Warehousing"
    tmp$sector[tmp$industries == 51] <- "Information"
    tmp$sector[tmp$industries == 52] <- "Finance and Insurance"
    tmp$sector[tmp$industries == 53] <- "Real Estate Rental and Leasing"
    tmp$sector[tmp$industries == 54] <- "Professional, Scientific, and Technical Services"
    tmp$sector[tmp$industries == 55] <- "Management of Companies and Enterprises"
    tmp$sector[tmp$industries == 56] <- "Administrative and Support and Waste Management and Remediation Services"
    tmp$sector[tmp$industries == 61] <- "Educational Services"
    tmp$sector[tmp$industries == 62] <- "Health Care and Social Assistance"
    tmp$sector[tmp$industries == 71] <- "Arts, Entertainment, and Recreation"
    tmp$sector[tmp$industries == 72] <- "Accommodation and Food Services"
    tmp$sector[tmp$industries == 81] <- "Other Services (except Public Administration)"
    tmp$sector[tmp$industries == 92] <- "Public Administration"
  }
  
  ############ THIS IS THE TABLE YOU WANT! #############
  # Aggregating the number of jobs by sector, industries (the same), and county codes, to a new object called tmp_jobs. 
  tmp_jobs <- aggregate(tmp$total_jobs, by = list(tmp$industries, tmp$fipstate, tmp$fipscty, tmp$sector), sum)
  
  # Renaming the variables to what they are. 
  names(tmp_jobs) <- c("industries", "fipstate", "fipscty", "sector", paste("totaljobs_", tmp_name[1], sep=""))
  ######################################################
  
  # Adding the name of the counties to the table. 
  tmp_names <- read.csv(paste0('/home/cysakamoto/Data/county_names/name_', tmp_name[1], ".txt"), header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
  
  # Keeping the number of digits in each county code consistent.
  tmp_names$fipstate <- sprintf("%02d", tmp_names$fipstate)
  tmp_names$fipscty <- sprintf("%03d", tmp_names$fipscty)
  
  # Merging the data and name objects.
  data  <- merge(tmp_jobs, tmp_names, by=c("fipstate", "fipscty"))
  assign(paste0("data_", tmp_name[1]), data)
  
}