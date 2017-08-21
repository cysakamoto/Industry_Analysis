# Calculating industry diversity. 

# Setting the folder 
setwd("/home/cysakamoto/Data/data_zip_details")

# Getting only the 2015 file. 
csvfile = "zbp2015.txt"

# Reading the raw data file
tmp <- read.csv(csvfile, header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")

# Turn the strings in naics to numeric
tmp$naics <- as.numeric(as.character(tmp$naics))

# Get all the rows where naics is a number, not a dot
tmp <- subset(tmp, naics > 0)

# Sum the estimations by zip code and by naics code 
tmp$jobs <- (tmp$n1_4 * 3) + (tmp$n5_9 * 7) + (tmp$n10_19 * 15) + (tmp$n20_49 * 35) + (tmp$n50_99 * 75) + (tmp$n100_249 * 175) + 
  (tmp$n250_499 * 375) + (tmp$n500_999 * 750) + (tmp$n1000 * 2000)

# Retain only the zip code, naics, and number of jobs columns 
tmp <- tmp[c("zip", "naics", "jobs")]

# Getting only Chicago data
chi_data <- tmp[which(tmp$zip >= 60601 & tmp$zip <= 60661), ]

# Counting how many times a zipcode shows up, or counting how many industries there are in each zipcode
chi_ind <- as.data.frame(table(chi_data$zip))

# Changing the name of the variables
names(chi_ind) <- c("Zipcode", "industries")

# Writing the data frame to file 
write.csv(chi_ind, file="chi_ind", na = "", row.names=FALSE)

# Merging data 
chicago <- merge(chi_data, chi_ind, by="Zipcode", all=TRUE)

# Writing data frame to file
write.csv(chicago, file="chicago", na = "", row.names=FALSE)

# Running linear regression
fit1 <- lm(chicago$industries ~ chicago$counting)
summary(fit1) # show results
# Plotting a scatterplot with the points 
plot(chicago$counting, chicago$industries, main = "Industries as DV, Ancestry as IV", xlab = "Number of Ethnicities", ylab = "Number of Industries")
abline(fit1, col="red") # Fitting the line to scatterplot

# Running linear regression
fit2 <- lm(chicago$counting ~ chicago$industries)
summary(fit2) # show results
# Plotting a scatterplot with the points 
plot(chicago$industries, chicago$counting, main = "Ancestry as DV, Industry as IV", xlab = "Number of Industries", ylab = "Number of Ethnicities")
abline(fit2, col="red") # Fitting the line to scatterplot 
  


