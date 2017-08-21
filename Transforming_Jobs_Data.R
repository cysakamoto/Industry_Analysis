library(reshape)
library(foreign)

csvfiles <- list.files("/home/cysakamoto/Data/raw_data")

for(csvfile in csvfiles) {
  tmp <- read.csv(csvfile, header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")
  tmp_name <- as.character(csvfile)
  tmp_name <- strsplit(tmp_name, "\\.") [[1]]
  print(tmp_name[1])
  colnames(tmp) <- tolower(colnames(tmp))
  tmp$naics <- as.numeric(as.character(tmp$naics))
  tmp <- subset(tmp, naics > 0)
  tmp$n1_4 <- tmp$n1_4 * 3
  tmp$n5_9 <- tmp$n5_9 * 7
  tmp$n10_19 <- tmp$n10_19 * 15
  tmp$n20_49 <- tmp$n20_49 * 35
  tmp$n50_99 <- tmp$n50_99 * 75
  tmp$n100_249 <- tmp$n100_249 * 175
  tmp$n250_499 <- tmp$n250_499 * 375
  tmp$n500_999 <- tmp$n500_999 * 750
  tmp$n1000 <- tmp$n1000 * 2000
  tmp$Jobs <- tmp$n1_4 + tmp$n5_9 + tmp$n10_19 + tmp$n20_49 + tmp$n50_99 + tmp$n100_249 + 
    tmp$n250_499 + tmp$n500_999 + tmp$n1000
  tmp <- tmp[c("zip", "naics", "Jobs")]
  tmp <- rename(tmp, c(Jobs = tmp_name[1]))
  if (csvfile == "zbp1998.txt") {
    data <- tmp
  } else {
    data <- merge(data, tmp, by=c("zip", "naics"), all=TRUE)} 
}


write.csv(data, file="industry_zip", na="\\N", row.names=TRUE)


