####################################################
# Ranking. Biggest industry in a County. 
# Cristina Y. Sakamoto
# The University of Chicago
# 2017
# County Level, so we can use data since 1986. 
####################################################

# Load these libraries! 
library(dplyr)
library(data.table)

# Sort the data by County, and by number of total jobs. 
sorted <- data %>% 
  arrange(-totaljobs_zbp2015) %>%
  group_by(zip) %>%
  mutate(rank=row_number())

# Keeping only the rows where rank equals 1. 
rank1 <- sorted[which(sorted$rank == 1), ]

# In the county case, we need to create a "code" field to join FIPSTATE and FIPSCTY, and be able to join with the map. 
rank1$code <- paste0(rank1$fipstate, rank1$fipscty)

# Writing the final table 
write.csv(rank1, file=paste("rank1", tmp_name[1]), na = "", row.names=FALSE)

