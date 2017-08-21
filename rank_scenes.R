# Making maps of the highest scenes score per zipcode 


library(foreign)
library(reshape)
library(tidyr)
library(reshape2)
library(dplyr)
library(data.table)

#Reading the Scenes data file
scenes <- read.spss("MergeB-34.sav", use.value.labels = TRUE, to.data.frame = TRUE, reencode = NA)

scenes$zip <- as.numeric(as.character(scenes$zipcode))

scenes_sel <- scenes[c("zipcode", "BZ01TradPerf", "BZ01SEPerf", "BZ01UtilPerf", "BZ01CharPerf", "BZ01EgalPerf", "BZ01NeighPerf", 
                       "BZ01FormPerf", "BZ01ExhiPerf", "BZ01GlamPerf", "BZ01TransPerf", "BZ01RatPerf", "BZ01LocalPerf", 
                       "BZ01StatePerf", "BZ01CorpPerf", "BZ01EthnPerf")]

# Changing variable names
names(scenes_sel)[names(scenes_sel)=="BZ01TradPerf"] <- "Traditionalistic"
names(scenes_sel)[names(scenes_sel)=="BZ01SEPerf"] <- "Self_Expressive"
names(scenes_sel)[names(scenes_sel)=="BZ01UtilPerf"] <- "Utilitarian"
names(scenes_sel)[names(scenes_sel)=="BZ01CharPerf"] <- "Charismatic"
names(scenes_sel)[names(scenes_sel)=="BZ01EgalPerf"] <- "Egalitarian"
names(scenes_sel)[names(scenes_sel)=="BZ01NeighPerf"] <- "Neighborly"
names(scenes_sel)[names(scenes_sel)=="BZ01FormPerf"] <- "Formality"
names(scenes_sel)[names(scenes_sel)=="BZ01ExhiPerf"] <- "Exhibitionism"
names(scenes_sel)[names(scenes_sel)=="BZ01GlamPerf"] <- "Glamorous"
names(scenes_sel)[names(scenes_sel)=="BZ01TransPerf"] <- "Transgressive"
names(scenes_sel)[names(scenes_sel)=="BZ01RatPerf"] <- "Rational"
names(scenes_sel)[names(scenes_sel)=="BZ01LocalPerf"] <- "Locality"
names(scenes_sel)[names(scenes_sel)=="BZ01StatePerf"] <- "State_Scenes"
names(scenes_sel)[names(scenes_sel)=="BZ01CorpPerf"] <- "Corporateness"
names(scenes_sel)[names(scenes_sel)=="BZ01EthnPerf"] <- "Ethnicity"

#Reshaping the data from wide to long
scenes_sel <- gather(scenes_sel, scenes, value, -zipcode)

# Sort the data by County, and by number of total jobs. 
sorted <- scenes_sel %>% 
  arrange(-value) %>%
  group_by(zipcode) %>%
  mutate(rank=row_number())

# Keeping only the rows where rank equals 1. 
rank1 <- sorted[which(sorted$rank == 1), ]

# What frequency of industries in the ranking 1?
sectors_1 <- as.data.frame(table(rank1$key))

# Writing the final table 
write.csv(rank1, "scenes_rank1", na = "", row.names=FALSE)

