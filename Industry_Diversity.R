# Calculating industry diversity. 

setwd("/home/cysakamoto/Data/data_zip_details")

csvfile = "zbp2015.txt"

tmp <- read.csv(csvfile, header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")

# turn the strings in naics to numeric
tmp$naics <- as.numeric(as.character(tmp$naics))

# get all the rows where naics is a number, not a dot
tmp <- subset(tmp, naics > 0)

# sum the estimations by zip code and by naics code 
tmp$jobs <- (tmp$n1_4 * 3) + (tmp$n5_9 * 7) + (tmp$n10_19 * 15) + (tmp$n20_49 * 35) + (tmp$n50_99 * 75) + (tmp$n100_249 * 175) + 
  (tmp$n250_499 * 375) + (tmp$n500_999 * 750) + (tmp$n1000 * 2000)

# retain only the zip code, naics, and number of jobs columns 
tmp <- tmp[c("zip", "naics", "jobs")]

chi_data <- tmp[which(tmp$zip >= 60601 & tmp$zip <= 60661), ]

chi_ind <- as.data.frame(table(chi_data$zip))

names(chi_ind) <- c("Zipcode", "industries")

write.csv(chi_ind, file="chi_ind", na = "", row.names=FALSE)


chicago <- merge(chi_data, chi_ind, by="Zipcode", all=TRUE)

write.csv(chicago, file="chicago", na = "", row.names=FALSE)

fit1 <- lm(chicago$industries ~ chicago$counting)
summary(fit1) # show results
plot(chicago$counting, chicago$industries, main = "Industries as DV, Ancestry as IV", xlab = "Number of Ethnicities", ylab = "Number of Industries")
abline(fit1, col="red")

fit2 <- lm(chicago$counting ~ chicago$industries)
summary(fit2) # show results
plot(chicago$industries, chicago$counting, main = "Ancestry as DV, Industry as IV", xlab = "Number of Industries", ylab = "Number of Ethnicities")
abline(fit2, col="red")
  


