####################################################
# Ranking. Biggest industry in a County or Zipcode. 
# Cristina Y. Sakamoto
# The University of Chicago
# 2017
# Zipcode Level, so we can use data since 1986. 
# Looking at number of jobs change, and correlation of what kind of sectors go together. 
####################################################

# Load these libraries! 
library(dplyr)
library(data.table)

# Sort the data by zipcode, and by number of total jobs. 
sorted <- data %>% 
  arrange(-totaljobs_zbp1998) %>%
  group_by(zip) %>%
  mutate(rank=row_number())

# Keeping only the rows where rank equals 1. 
rank1 <- sorted[which(sorted$rank == 1), ]

# Writing the final table 
write.csv(rank1, file=paste("rank1_1998", tmp_name[1]), na = "", row.names=FALSE)

####################################################

# What frequency of industries in the ranking 1?
sectors_1 <- as.data.frame(table(rank1$sector))

# Write it to a file.
write.csv(sectors_1, file=paste("sectors_1", tmp_name[1]), na = "", row.names=FALSE)

####################################################



