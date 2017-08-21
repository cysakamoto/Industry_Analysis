# Setting the folder and getting the name of every datafile in the following folder and record it. 
setwd("/home/cysakamoto/Data/data_zip_details")
csvfiles <- list.files("/home/cysakamoto/Data/data_zip_details")

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
  
  ######
  # Calculating the estimation of number of jobs by county and sector
  tmp$total_jobs <- (tmp$n1_4*3) + (tmp$n5_9 * 7) + (tmp$n10_19 * 15) + (tmp$n20_49 * 35) + (tmp$n50_99 * 75) + (tmp$n100_249 * 175) +  (tmp$n250_499 * 375) + (tmp$n500_999 * 750) + (tmp$n1000 * 2000)
  
  # Removing all the dashes from the SIC column, and write them in a new column called industries
  if (tmp_year <= 1997) {
    tmp$industries <- gsub("-", "", tmp$sic)
  } else {
    tmp$industries <- gsub("-", "", tmp$naics)
  }
  
  # Turn the strings in naics to numeric
  tmp$industries <- as.numeric(as.character(tmp$industries))
  
  # Keeping only important variables
  tmp <- tmp[c("zip", "total_jobs", "industries")]
  
  # Delete all the rows where the SIC code is a broad code, not a specific code 
  if (tmp_year <= 1997) {
    # Removing the detailed codes
    tmp <- tmp[!(tmp$industries > 100), ] 
    # Replacing the codes of the industries to numeric codes 
    tmp$industries[tmp$industries > 01 & tmp$industries < 09] <- 11
    tmp$industries[tmp$industries >= 10 & tmp$industries < 14] <- 21
    tmp$industries[tmp$industries >= 15 & tmp$industries < 17] <- 23
    tmp$industries[tmp$industries >= 20 & tmp$industries < 39] <- 31
    tmp$industries[tmp$industries >= 40 & tmp$industries < 49] <- 48
    tmp$industries[tmp$industries >= 50 & tmp$industries < 51] <- 42
    tmp$industries[tmp$industries >= 52 & tmp$industries < 59] <- 44
    tmp$industries[tmp$industries >= 60 & tmp$industries < 89] <- 60
    tmp$industries[tmp$industries >= 91 & tmp$industries < 99] <- 91
    tmp$sector[tmp$industries > 01 & tmp$industries < 09] <- "Agriculture"
    tmp$sector[tmp$industries >= 10 & tmp$industries < 14] <- "Mining"
    tmp$sector[tmp$industries >= 15 & tmp$industries < 17] <- "Construction"
    tmp$sector[tmp$industries >= 20 & tmp$industries < 39] <- "Manufacturing"
    tmp$sector[tmp$industries >= 40 & tmp$industries < 49] <- "Utilities"
    tmp$sector[tmp$industries >= 50 & tmp$industries < 51] <- "Wholesale_Trade"
    tmp$sector[tmp$industries >= 52 & tmp$industries < 59] <- "Retail_Trade"
    tmp$sector[tmp$industries >= 60 & tmp$industries < 89] <- "Management"
    tmp$sector[tmp$industries >= 91 & tmp$industries < 99] <- "Public_Adm"
  } else {
    # Removing the detailed codes
    tmp <- tmp[!(tmp$industries > 100), ] 
    # Replacing the codes of the industries to numeric codes 
    tmp$industries[tmp$industries >= 48 & tmp$industries < 49] <- 22 # Transportation, to Utilities
    tmp$industries[tmp$industries >= 31 & tmp$industries < 34] <- 31 # Manufacturing
    tmp$industries[tmp$industries >= 44 & tmp$industries < 46] <- 44 # Retail Trade
    tmp$industries[tmp$industries >= 51 & tmp$industries < 56] <- 60 # Management
    tmp$sector[tmp$industries == 11] <- "Agriculture"
    tmp$sector[tmp$industries == 21] <- "Mining"
    tmp$sector[tmp$industries == 22] <- "Utilities"
    tmp$sector[tmp$industries == 23] <- "Construction"
    tmp$sector[tmp$industries == 31] <- "Manufacturing"
    tmp$sector[tmp$industries == 42] <- "Wholesale_Trade"
    tmp$sector[tmp$industries == 44] <- "Retail_Trade"
    tmp$sector[tmp$industries == 60] <- "Management"
    tmp$sector[tmp$industries == 61] <- "Education"
    tmp$sector[tmp$industries == 62] <- "Health_Care"
    tmp$sector[tmp$industries == 71] <- "Arts"
    tmp$sector[tmp$industries == 72] <- "Hospitality"
    tmp$sector[tmp$industries == 81] <- "Other_Services"
    tmp$sector[tmp$industries == 92] <- "Public_Administration"
  }
  
  ############ THIS IS THE TABLE YOU WANT! #############
  # Aggregating the number of jobs by sector, industries (the same), and county codes, to a new object called tmp_jobs. 
  tmp_jobs <- aggregate(tmp$total_jobs, by = list(tmp$industries, tmp$zip, tmp$sector), sum)
  
  # Renaming the variables to what they are. 
  names(tmp_jobs) <- c("industries", "zip", "sector", paste("totaljobs_", tmp_name[1], sep=""))
  
  write.csv(tmp_jobs, file=paste("jobs_ind", tmp_name[1], sep = "_"), na = "", row.names=FALSE, append = TRUE)
  
  ######################################################
  
  # Creating individual year dataframe.
  assign(paste0("data_", tmp_name[1]), tmp_jobs)
  
  ######################################################
  
  # Merging a unified file for all years
  if (csvfile == "zbp1998.txt") {
    data <- tmp_jobs 
  } else {
    data <- merge(data, tmp_jobs, by=c("zip", "sector", "industries"), all=TRUE) 
  }
} 

write.csv(data, file=paste("jobs_ind", Sys.Date(),sep = "_"), na = "", row.names=FALSE, append = TRUE, sep = ",")




