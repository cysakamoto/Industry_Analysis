#####################################
# For the Asian Studies Paper 
# Analyzing the correlation between sectors and the arts
#####################################
library(reshape)
library(Hmisc)
library(corrplot)
library(psych)
#####################################
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

# Defining color palette to gray
col1 <- colorRampPalette(c("gray90", "gray20"))

#####################################

options(scipen = 999, digits = 3)

setwd("/home/cysakamoto/Data")

#####################
# MSA data 
#####################
# Reading the 2010 MSA dataset
tmp <- read.csv("/home/cysakamoto/Data/msa_data/cbp2010msa.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")

# Change the columns names all to lower case (to avoid errors)
colnames(tmp) <- tolower(colnames(tmp))

# Calculating the total number of jobs. 
tmp$total_jobs <- (tmp$n1_4*3) + (tmp$n5_9 * 7) + (tmp$n10_19 * 15) + (tmp$n20_49 * 35) + (tmp$n50_99 * 75) + (tmp$n100_249 * 175) +  (tmp$n250_499 * 375) + (tmp$n500_999 * 750) + (tmp$n1000 * 2000)

# Removing all the dashes from the SIC column, and write them in a new column called industries
tmp$industries <- gsub("-", "", tmp$naics)

# Turn the strings in naics to numeric
tmp$industries <- as.numeric(as.character(tmp$industries))

# Collapsing categories together. Excluding some categories. 
# Delete all the rows where the SIC code is a broad code, not a specific code 
tmp <- tmp[!(tmp$industries < 310000), ] 
tmp <- tmp[!(tmp$industries > 730000 & tmp$industries < 813000), ]
tmp <- tmp[!(tmp$industries > 814000), ]
# Summarized categories 
tmp$sector[tmp$industries >= 310000 & tmp$industries < 340000] <- "Manufacturing"
tmp$sector[tmp$industries >= 420000 & tmp$industries < 440000] <- "Wholesale"
tmp$sector[tmp$industries >= 440000 & tmp$industries < 460000] <- "Retail"
tmp$sector[tmp$industries >= 480000 & tmp$industries < 500000] <- "Wholesale"
tmp$sector[tmp$industries >= 510000 & tmp$industries < 600000] <- "Services"
tmp$sector[tmp$industries >= 610000 & tmp$industries < 620000] <- "Education"
tmp$sector[tmp$industries >= 620000 & tmp$industries < 700000] <- "Health Care"
tmp$sector[tmp$industries >= 710000 & tmp$industries < 730000] <- "Arts"
tmp$sector[tmp$industries >= 813000 & tmp$industries < 814000] <- "Religion"

# Keeping only important variables
tmp <- tmp[c("msa", "total_jobs", "sector")]

# Aggregating the number of jobs by sector, industries (the same), and county codes, to a new object called tmp_jobs. 
tmp_jobs <- aggregate(tmp$total_jobs, by = list(tmp$sector, tmp$msa), sum)

# Renaming the variables to what they are. 
names(tmp_jobs) <- c("sector", "msa", "totaljobs")

# Reshaping to wide 
data_msa <- reshape(tmp_jobs, timevar = "sector", idvar = "msa", direction = "wide")
names(data_msa) <- gsub("totaljobs.", "", names(data_msa))

# To get a table with all the correlations
cor_table_msa <- cor(data_msa[2:ncol(data_msa)], use = "pairwise.complete.obs")

# Adding the significance level to the graph
res1 <- cor.mtest(data_msa[2:ncol(data_msa)],0.95)
res2 <- cor.mtest(data_msa[2:ncol(data_msa)],0.99)
# Making the fancy chart
corrplot(cor_table_msa, p.mat = res1[[1]], sig.level=0.05, method="shade", type="lower", order="hclust", addCoef.col = "black", tl.pos="ld", tl.col = "black", tl.offset = 0.5, insig = "blank", mar=c(0,0,1,0))

# Making a factor analysis sorted table
msa_sort <- mat.sort(cor_table_msa)

# To find the significance level and N. The other correlation table is easier to edit on Excel, but doesn't provide this information. 
results_msa <- rcorr(as.matrix(data_msa[2:ncol(data_msa)]))

# Saving the correlation table to a csv file
write.table(cor_table_msa, "/home/cysakamoto/Data/cor_msa", na = "", row.names=FALSE, append = TRUE, sep = ",")

#####################
# For the zipcode analysis 
#####################

# Reading the 2010 zipcode dataset
tmp <- read.csv("/home/cysakamoto/Data/raw_data/zbp2010.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", na.strings="NA")

# Calculating the total number of jobs. 
tmp$total_jobs <- (tmp$n1_4*3) + (tmp$n5_9 * 7) + (tmp$n10_19 * 15) + (tmp$n20_49 * 35) + (tmp$n50_99 * 75) + (tmp$n100_249 * 175) +  (tmp$n250_499 * 375) + (tmp$n500_999 * 750) + (tmp$n1000 * 2000)

# Removing all the dashes from the SIC column, and write them in a new column called industries
tmp$industries <- gsub("-", "", tmp$naics)

# Turn the strings in naics to numeric
tmp$industries <- as.numeric(as.character(tmp$industries))

# Collapsing categories together. Excluding some categories. 
# Delete all the rows where the SIC code is a broad code, not a specific code 
tmp <- tmp[!(tmp$industries < 310000), ] 
tmp <- tmp[!(tmp$industries > 720000 & tmp$industries < 813000), ]
tmp <- tmp[!(tmp$industries > 814000), ]
# Summarized categories 
tmp$sector[tmp$industries >= 310000 & tmp$industries < 340000] <- "Manufacturing"
tmp$sector[tmp$industries >= 420000 & tmp$industries < 440000] <- "Wholesale"
tmp$sector[tmp$industries >= 440000 & tmp$industries < 460000] <- "Retail"
tmp$sector[tmp$industries >= 480000 & tmp$industries < 500000] <- "Wholesale"
tmp$sector[tmp$industries >= 510000 & tmp$industries < 600000] <- "Services"
tmp$sector[tmp$industries >= 610000 & tmp$industries < 620000] <- "Education"
tmp$sector[tmp$industries >= 620000 & tmp$industries < 700000] <- "Health Care"
tmp$sector[tmp$industries >= 710000 & tmp$industries < 730000] <- "Arts"
tmp$sector[tmp$industries >= 813000 & tmp$industries < 814000] <- "Religion"

# Keeping only important variables
tmp <- tmp[c("zip", "total_jobs", "sector")]

# Aggregating the number of jobs by sector 
tmp <- aggregate(tmp$total_jobs, by = list(tmp$sector, tmp$zip), sum)

# Changing the name of the variables
names(tmp) <- c("sector", "zip", "totaljobs")

# Changing the shape of the data to wide
data_zip <- reshape(tmp, timevar = "sector", idvar = "zip", direction = "wide")
names(data_zip) <- gsub("totaljobs.", "", names(data_zip))

# To get a table with all the correlations
cor_table_zip <- cor(data_zip[2:ncol(data_zip)], use = "pairwise.complete.obs")
  
# Making the fancy circle graph with the correlations
# Calculating the significance levels
res1 <- cor.mtest(data_zip[2:ncol(data_zip)],0.95)
res2 <- cor.mtest(data_zip[2:ncol(data_zip)],0.99)
# corrplot(cor_table_zip, method="circle", order = "alphabet")
## specialized the insignificant value according to the significant level
corrplot(cor_table_zip, p.mat = res1[[1]], sig.level=0.05, col=col1(50), method="shade", type="lower", order="hclust", addCoef.col = "black", tl.pos="ld", tl.col = "black", tl.offset = 0.5, insig = "blank", mar=c(0,0,1,0))

# ############################
# # Making a factor analysis sorted table
# zip_sort <- mat.sort(cor_table_zip)
# 
# # To find the significance level and N. The other correlation table is easier to edit on Excel, but doesn't provide this information. 
results_zip <- rcorr(as.matrix(data_zip[2:ncol(data_msa)]))
# 
# # Saving the correlation table to a csv file
# write.table(cor_table_zip, "/home/cysakamoto/Data/cor_zip", na = "", row.names=FALSE, append = TRUE, sep = ",")

################## 
# For Chicago

# Getting the rows with Chicago zip codes 
tmp2 <- subset(tmp, zip == 60290  | zip ==60601  | zip ==60602  | zip ==60603  | zip ==60604  | zip ==60605  | zip ==60606  | zip ==60607  
               | zip ==60608  | zip ==60610  | zip ==60611  | zip ==60614  | zip ==60615  | zip ==60618  | zip ==60619  | zip ==60622  | 
                 zip ==60623  | zip ==60624  | zip ==60628  | zip ==60609  | zip ==60612  | zip ==60613  | zip ==60616  | zip ==60617  | 
                 zip ==60620  | zip ==60621  | zip ==60625  | zip ==60626  | zip ==60629  | zip ==60630  | zip ==60632  | zip ==60636  | 
                 zip ==60637  | zip ==60631  | zip ==60633  | zip ==60634  | zip ==60638  | zip ==60641  | zip ==60642  | zip ==60643  | 
                 zip ==60646  | zip ==60647  | zip ==60652  | zip ==60653  | zip ==60656  | zip ==60660  | zip ==60661  | zip ==60664  | 
                 zip ==60639  | zip ==60640  | zip ==60644  | zip ==60645  | zip ==60649  | zip ==60651  | zip ==60654  | zip ==60655  | 
                 zip ==60657  | zip ==60659  | zip ==60666  | zip ==60668  | zip ==60673  | zip ==60677  | zip ==60669  | zip ==60670  | 
                 zip ==60674  | zip ==60675  | zip ==60678  | zip ==60680  | zip ==60681  | zip ==60682  | zip ==60686  | zip ==60687  | 
                 zip ==60688  | zip ==60689  | zip ==60694  | zip ==60695  | zip ==60697  | zip ==60699  | zip ==60684  | zip ==60685  | 
                 zip ==60690  | zip ==60691  | zip ==60693  | zip ==60696  | zip ==60701)

# Reshaping the dataset to wide 
data_chi <- reshape(tmp2, timevar = "sector", idvar = "zip", direction = "wide")
names(data_chi) <- gsub("totaljobs.", "", names(data_zip))

# To get a table with all the correlations
cor_table_chi <- cor(data_chi[2:ncol(data_zip)], use = "pairwise.complete.obs")

# Making the fancy circle graph with the correlations
# Adding the significance level to the graph
res1 <- cor.mtest(data_chi[2:ncol(data_chi)],0.95)
res2 <- cor.mtest(data_chi[2:ncol(data_chi)],0.99)
# corrplot(cor_table_chi, p.mat = res1[[1]], sig.level=0.05, type="lower", tl.pos="d", tl.col = "black", tl.offset = 0.5)
# corrplot(cor_table_chi, p.mat = res1[[1]], sig.level=0.05, method="shade", order="hclust", type="lower", tl.pos="d", tl.col = "black", tl.offset = 0.5)
# corrplot(cor_table_chi, p.mat = res1[[1]], insig="blank", method="shade", order="hclust", type="lower", tl.pos="d", tl.col = "black", tl.offset = 0.5)
corrplot(cor_table_chi, p.mat = res1[[1]], sig.level=0.05, method="shade", col=col1(50), type="lower", order="AOE", addCoef.col = "black", tl.pos="ld", tl.col = "black", tl.offset = 0.5, insig = "blank", mar=c(0,0,1,0))

# Making a factor analysis sorted table
chi_sort <- mat.sort(cor_table_chi)

# To find the significance level and N. The other correlation table is easier to edit on Excel, but doesn't provide this information. 
results_chi <- rcorr(as.matrix(data_chi[2:ncol(data_chi)]))

# Saving the correlation table to a csv file
write.table(cor_table_chi, "/home/cysakamoto/Data/cor_chi", na = "", row.names=FALSE, append = TRUE, sep = ",")


######
# For all the NAICS sectors
# Delete all the rows where the SIC code is a broad code, not a specific code 
tmp <- tmp[!(tmp$industries < 100), ] 
# Changing name of the sectors by code range
tmp$sector[tmp$industries > 110000 & tmp$industries < 210000] <- "Agriculture, Forestry, Fishing and Hunting"
tmp$sector[tmp$industries >= 210000 & tmp$industries < 220000] <- "Mining, Quarrying, and Oil and Gas Extraction"
tmp$sector[tmp$industries >= 220000 & tmp$industries < 230000] <- "Utilities"
tmp$sector[tmp$industries >= 230000 & tmp$industries < 310000] <- "Construction"
tmp$sector[tmp$industries >= 310000 & tmp$industries < 340000] <- "Manufacturing"
tmp$sector[tmp$industries >= 420000 & tmp$industries < 440000] <- "Wholesale Trade"
tmp$sector[tmp$industries >= 440000 & tmp$industries < 460000] <- "Retail Trade"
tmp$sector[tmp$industries >= 480000 & tmp$industries < 500000] <- "Transportation and Warehousing"
tmp$sector[tmp$industries >= 510000 & tmp$industries < 520000] <- "Information"
tmp$sector[tmp$industries >= 520000 & tmp$industries < 530000] <- "Finance and Insurance"
tmp$sector[tmp$industries >= 530000 & tmp$industries < 540000] <- "Real Estate and Rental and Leasing"
tmp$sector[tmp$industries >= 540000 & tmp$industries < 550000] <- "Professional, Scientific, and Technical Services"
tmp$sector[tmp$industries >= 550000 & tmp$industries < 560000] <- "Management and Companies and Enterprises"
tmp$sector[tmp$industries >= 560000 & tmp$industries < 600000] <- "Administrative and Support and Waste Management and Remediation Services"
tmp$sector[tmp$industries >= 610000 & tmp$industries < 620000] <- "Educational Services"
tmp$sector[tmp$industries >= 620000 & tmp$industries < 700000] <- "Health Care and Social Assistance"
tmp$sector[tmp$industries >= 710000 & tmp$industries < 730000] <- "Arts, Entertainment, and Recreation"
tmp$sector[tmp$industries >= 810000 & tmp$industries < 920000] <- "Other Services (except Public Administration)"
tmp$sector[tmp$industries >= 920000] <- "Public Administration"
######
# # Changing the name of the sectors by range of codes.
# tmp$sector[tmp$industries >= 310000 & tmp$industries < 340000] <- "Manufacturing"
# tmp$sector[tmp$industries >= 420000 & tmp$industries < 440000] <- "Wholesale Trade, Transportation and Warehousing"
# tmp$sector[tmp$industries >= 440000 & tmp$industries < 460000] <- "Retail Trade"
# tmp$sector[tmp$industries >= 480000 & tmp$industries < 500000] <- "Wholesale Trade, Transportation and Warehousing"
# tmp$sector[tmp$industries >= 510000 & tmp$industries < 520000] <- "Information, Professional, Scientific, and Technical Services"
# tmp$sector[tmp$industries >= 520000 & tmp$industries < 530000] <- "Administrative, Finance, and Management"
# tmp$sector[tmp$industries >= 530000 & tmp$industries < 540000] <- "Real Estate and Rental and Leasing"
# tmp$sector[tmp$industries >= 540000 & tmp$industries < 550000] <- "Information, Professional, Scientific, and Technical Services"
# tmp$sector[tmp$industries >= 550000 & tmp$industries < 560000] <- "Administrative, Finance, and Management"
# tmp$sector[tmp$industries >= 560000 & tmp$industries < 600000] <- "Administrative, Finance, and Management"
# tmp$sector[tmp$industries >= 610000 & tmp$industries < 620000] <- "Educational Services"
# tmp$sector[tmp$industries >= 620000 & tmp$industries < 700000] <- "Health Care and Social Assistance"
# tmp$sector[tmp$industries >= 710000 & tmp$industries < 730000] <- "Arts, Entertainment, and Recreation"
# tmp$sector[tmp$industries >= 813000 & tmp$industries < 814000] <- "Religious, Grantmaking, Civic, Professional, and Similar Organizations"
######
# Summarized categories and labels
tmp$sector[tmp$industries >= 310000 & tmp$industries < 340000] <- "Manufacturing"
tmp$sector[tmp$industries >= 420000 & tmp$industries < 440000] <- "Wholesale"
tmp$sector[tmp$industries >= 440000 & tmp$industries < 460000] <- "Retail"
tmp$sector[tmp$industries >= 480000 & tmp$industries < 500000] <- "Wholesale"
tmp$sector[tmp$industries >= 510000 & tmp$industries < 600000] <- "Services"
tmp$sector[tmp$industries >= 610000 & tmp$industries < 620000] <- "Education"
tmp$sector[tmp$industries >= 620000 & tmp$industries < 700000] <- "Health Care"
tmp$sector[tmp$industries >= 710000 & tmp$industries < 730000] <- "Arts"
tmp$sector[tmp$industries >= 813000 & tmp$industries < 814000] <- "Religion"

