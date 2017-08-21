# Getting Scenes data 

library(foreign)
library(reshape)

setwd("/home/cysakamoto/Data/results")

#Reading the Scenes data file
# scenes <- read.spss("MergeB-34.sav", to.data.frame=TRUE)
scenes <- read.dta("Merge34.dta")
scenes <- read.spss("MergeB-34.sav", use.value.labels = TRUE, to.data.frame = TRUE, reencode = NA)

scenes <- scenes[!(data$zipcode > 1000), ]

# Getting the Scenes data
# scenes <- scenes[c("zipcode","esNAICS_AP11", "esNAICS_AP12", "esNAICS_AP13", "esNAICS_AP14", "esNAICS_AP15", "esNAICS_AP21", "esNAICS_AP22", "esNAICS_AP23", "esNAICS_AP24", "esNAICS_AP25", "esNAICS_AP26", "esNAICS_AP31", "esNAICS_AP32", "esNAICS_AP33", "esNAICS_AP34")]

scenes_sel <- scenes[c("zipcode", "BZ01TradPerf", "BZ01SEPerf", "BZ01UtilPerf", "BZ01CharPerf", "BZ01EgalPerf", "BZ01NeighPerf", 
                       "BZ01FormPerf", "BZ01ExhiPerf", "BZ01GlamPerf", "BZ01TransPerf", "BZ01RatPerf", "BZ01LocalPerf", 
                       "BZ01StatePerf", "BZ01CorpPerf", "BZ01EthnPerf")]
####
scenes_sel <- scenes[c("zipcode", "diversityfb2k", "CoV_CDI")]
write.csv(scenes_sel, file="diversity", na = "\\N", row.names=TRUE)
####

sel_describe <- describe(scenes_sel[2:16], na.rm=TRUE)
sel_cor <- cor(scenes_sel[2:16], use="pairwise.complete.obs")

# Calculating the correlations between the variables. 
cor_scenes <- cor(scenes[sapply(scenes, is.numeric)], use="pairwise.complete.obs")

#Formatting the zipcode column
scenes$zip <- as.numeric(as.character(scenes$zipcode))

# Changing variable names
names(scenes)[names(scenes)=="BZ01TradPerf"] <- "Traditionalistic"
names(scenes)[names(scenes)=="BZ01SEPerf"] <- "Self_Expressive"
names(scenes)[names(scenes)=="BZ01UtilPerf"] <- "Utilitarian"
names(scenes)[names(scenes)=="BZ01CharPerf"] <- "Charismatic"
names(scenes)[names(scenes)=="BZ01EgalPerf"] <- "Egalitarian"
names(scenes)[names(scenes)=="BZ01NeighPerf"] <- "Neighborly"
names(scenes)[names(scenes)=="BZ01FormPerf"] <- "Formality"
names(scenes)[names(scenes)=="BZ01ExhiPerf"] <- "Exhibitionism"
names(scenes)[names(scenes)=="BZ01GlamPerf"] <- "Glamorous"
names(scenes)[names(scenes)=="BZ01TransPerf"] <- "Transgressive"
names(scenes)[names(scenes)=="BZ01RatPerf"] <- "Rational"
names(scenes)[names(scenes)=="BZ01LocalPerf"] <- "Locality"
names(scenes)[names(scenes)=="BZ01StatePerf"] <- "State_Scenes"
names(scenes)[names(scenes)=="BZ01CorpPerf"] <- "Corporateness"
names(scenes)[names(scenes)=="BZ01EthnPerf"] <- "Ethnicity"

sel_describe <- describe(scenes, na.rm=TRUE)
sel_cor <- cor(scenes, use="pairwise.complete.obs")

# names(scenes)[names(scenes)=="esNAICS_AP11"] <- "Traditionalistic"
# names(scenes)[names(scenes)=="esNAICS_AP12"] <- "Self_Expressive"
# names(scenes)[names(scenes)=="esNAICS_AP13"] <- "Utilitarian"
# names(scenes)[names(scenes)=="esNAICS_AP14"] <- "Charismatic"
# names(scenes)[names(scenes)=="esNAICS_AP15"] <- "Egalitarian"
# names(scenes)[names(scenes)=="esNAICS_AP21"] <- "Neighborly"
# names(scenes)[names(scenes)=="esNAICS_AP22"] <- "Formality"
# names(scenes)[names(scenes)=="esNAICS_AP23"] <- "Exhibitionism"
# names(scenes)[names(scenes)=="esNAICS_AP24"] <- "Glamorous"
# names(scenes)[names(scenes)=="esNAICS_AP25"] <- "Transgressive"
# names(scenes)[names(scenes)=="esNAICS_AP26"] <- "Rational"
# names(scenes)[names(scenes)=="esNAICS_AP31"] <- "Locality"
# names(scenes)[names(scenes)=="esNAICS_AP32"] <- "State_Scenes"
# names(scenes)[names(scenes)=="esNAICS_AP33"] <- "Corporateness"
# names(scenes)[names(scenes)=="esNAICS_AP34"] <- "Ethnicity"

#Reading the arts jobs data file
arts <- read.csv("data_2016-11-04", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")

#Merging scenes data with arts data. 
data <- merge(arts, scenes, by="zip", all=FALSE)
data$X <- NULL

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

### Writing Files 
# write.csv(data, file=paste("full_scenes_data", Sys.Date(), sep = "_"))
# write.csv(datamiss, file=paste("nomissing_scenes_data", Sys.Date(), sep = "_"))

## Getting sample of data 
sample <- datamiss[sample(1:nrow(datamiss), 500, replace=FALSE), ]
write.csv(sample, file=paste("nomiss_sample", Sys.Date(), sep = "_"))

#Reshaping the data from wide to long
datamiss.long <- reshape(sample, varying=c(2:103), direction="long", idvar="zip", sep="_")

#Writing a file
write.csv(datamiss.long, file=paste("nomisslong_sample", Sys.Date(), sep = "_"))
write.csv(merge_file, file="scenes", na = "\\N", row.names=TRUE)

