####################################################
# Agriculture Data 
# Cristina Y. Sakamoto
# The University of Chicago
# 2017
# Zipcode Level, so we can use data since 1986. 
# Looking at number of jobs change, and correlation of what kind of sectors go together. 
####################################################

# To run after Ranking_Industry_Zip.R

# Setting the folder and getting the name of every datafile in the following folder and record it. 
setwd("/home/cysakamoto/Data/data_zip_details")
csvfiles <- list.files("/home/cysakamoto/Data/data_zip_details")

data <- data_zbp1998


agriculture_1998 <- data_zbp1998[which(data_zbp1998$industries == 11 | data_zbp1998$industries == 21), c("zip", "totaljobs_zbp1998")] 
agriculture_1998 <- aggregate(agriculture_1998$totaljobs_zbp1998, by = list(agriculture_1998$zip), sum)
names(agriculture_1998) <- c("zip", "agri_1998")

agriculture_2015 <- data_zbp2015[which(data_zbp2015$industries == 11 | data_zbp2015$industries == 21), c("zip", "totaljobs_zbp2015")] 
agriculture_2015 <- aggregate(agriculture_2015$totaljobs_zbp2015, by = list(agriculture_2015$zip), sum)
names(agriculture_2015) <- c("zip", "agri_2015")

agriculture <- merge(agriculture_1998, agriculture_2015, by="zip")

agriculture$delta <- agriculture$agri_2015 - agriculture$agri_1998

library(psych)
describe(agriculture)

write.csv(agriculture, file=paste("agriculture"), na = "", row.names=FALSE)
