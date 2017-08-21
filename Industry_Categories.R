####################################################
# Which industries had the biggest decrease in number of jobs? And where? 
# Cristina Y. Sakamoto
# The University of Chicago
# 2017
# Industry Categories 
#################################################### 

#################################################### 
# NAICS 

# Full name of broader categories 
tmp$sector[tmp$industries == 11] <- "Agriculture, Forestry, Fishing and Hunting"
tmp$sector[tmp$industries == 21] <- "Mining"
tmp$sector[tmp$industries == 22] <- "Utilities"
tmp$sector[tmp$industries == 23] <- "Construction"
tmp$sector[tmp$industries == 31] <- "Manufacturing"
tmp$sector[tmp$industries == 42] <- "Wholesale Trade"
tmp$sector[tmp$industries == 44] <- "Retail Trade"
tmp$sector[tmp$industries == 48] <- "Transportation and Warehousing"
tmp$sector[tmp$industries == 51] <- "Information"
tmp$sector[tmp$industries == 52] <- "Finance and Insurance"
tmp$sector[tmp$industries == 53] <- "Real Estate Rental and Leasing"
tmp$sector[tmp$industries == 54] <- "Professional, Scientific, and Technical Services"
tmp$sector[tmp$industries == 55] <- "Management of Companies and Enterprises"
tmp$sector[tmp$industries == 56] <- "Administrative and Support and Waste Management and Remediation Services"
tmp$sector[tmp$industries == 61] <- "Educational Services"
tmp$sector[tmp$industries == 62] <- "Health Care and Social Assistance"
tmp$sector[tmp$industries == 71] <- "Arts, Entertainment, and Recreation"
tmp$sector[tmp$industries == 72] <- "Accommodation and Food Services"
tmp$sector[tmp$industries == 81] <- "Other Services (except Public Administration)"
tmp$sector[tmp$industries == 92] <- "Public Administration"

#################################################### 
# Summarized categories and labels
tmp$sector[tmp$industries == 11] <- "Agriculture"
tmp$sector[tmp$industries == 21] <- "Mining"
tmp$sector[tmp$industries == 22] <- "Utilities"
tmp$sector[tmp$industries == 23] <- "Construction"
tmp$sector[tmp$industries == 31] <- "Manufacturing"
tmp$sector[tmp$industries == 42] <- "Wholesale_Trade"
tmp$sector[tmp$industries == 44] <- "Retail_Trade"
tmp$sector[tmp$industries == 48] <- "Transportation"
tmp$sector[tmp$industries == 51] <- "Information"
tmp$sector[tmp$industries == 52] <- "Finance_Insurance"
tmp$sector[tmp$industries == 53] <- "Real_Estate"
tmp$sector[tmp$industries == 54] <- "Professional_Services"
tmp$sector[tmp$industries == 55] <- "Management"
tmp$sector[tmp$industries == 56] <- "Management"
tmp$sector[tmp$industries == 61] <- "Education"
tmp$sector[tmp$industries == 62] <- "Health_Care"
tmp$sector[tmp$industries == 71] <- "Arts"
tmp$sector[tmp$industries == 72] <- "Hospitality"
tmp$sector[tmp$industries == 81] <- "Other_Services"
tmp$sector[tmp$industries == 92] <- "Public_Administration"

#################################################### 
# Smaller categories, Broader names 
tmp$sector[tmp$industries >= 310000 & tmp$industries < 340000] <- "Manufacturing"
tmp$sector[tmp$industries >= 420000 & tmp$industries < 440000] <- "Wholesale"
tmp$sector[tmp$industries >= 440000 & tmp$industries < 460000] <- "Retail"
tmp$sector[tmp$industries >= 480000 & tmp$industries < 500000] <- "Wholesale"
tmp$sector[tmp$industries >= 510000 & tmp$industries < 600000] <- "Services"
tmp$sector[tmp$industries >= 610000 & tmp$industries < 620000] <- "Education"
tmp$sector[tmp$industries >= 620000 & tmp$industries < 700000] <- "Health Care"
tmp$sector[tmp$industries >= 710000 & tmp$industries < 730000] <- "Arts"
tmp$sector[tmp$industries >= 813000 & tmp$industries < 814000] <- "Religion" 

#################################################### 
# SIC 

if (tmp_year < 1997) {
  tmp$sector[tmp$industries > 01 & tmp$industries < 09] <- "Agriculture, Forestry & Fishing"
  tmp$sector[tmp$industries >= 10 & tmp$industries < 14] <- "Mining"
  tmp$sector[tmp$industries >= 15 & tmp$industries < 17] <- "Construction"
  tmp$sector[tmp$industries >= 20 & tmp$industries < 39] <- "Manufacturing"
  tmp$sector[tmp$industries >= 40 & tmp$industries < 49] <- "Transportation, Communications, Electric, Gas & Sanitary Services"
  tmp$sector[tmp$industries >= 50 & tmp$industries < 51] <- "Wholesale Trade"
  tmp$sector[tmp$industries >= 52 & tmp$industries < 59] <- "Retail Trade"
  tmp$sector[tmp$industries >= 60 & tmp$industries < 67] <- "Finance, Insurance & Real Estate"
  tmp$sector[tmp$industries >= 70 & tmp$industries < 89] <- "Services"
  tmp$sector[tmp$industries >= 91 & tmp$industries < 99] <- "Public Administration"
} else {
  # Changing NAICS codes to the name of sectors
  tmp$sector[tmp$industries == 11] <- "Agriculture, Forestry, Fishing and Hunting"
  tmp$sector[tmp$industries == 21] <- "Mining"
  tmp$sector[tmp$industries == 22] <- "Utilities"
  tmp$sector[tmp$industries == 23] <- "Construction"
  tmp$sector[tmp$industries == 31] <- "Manufacturing"
  tmp$sector[tmp$industries == 42] <- "Wholesale Trade"
  tmp$sector[tmp$industries == 44] <- "Retail Trade"
  tmp$sector[tmp$industries == 48] <- "Transportation and Warehousing"
  tmp$sector[tmp$industries == 51] <- "Information"
  tmp$sector[tmp$industries == 52] <- "Finance and Insurance"
  tmp$sector[tmp$industries == 53] <- "Real Estate Rental and Leasing"
  tmp$sector[tmp$industries == 54] <- "Professional, Scientific, and Technical Services"
  tmp$sector[tmp$industries == 55] <- "Management of Companies and Enterprises"
  tmp$sector[tmp$industries == 56] <- "Administrative and Support and Waste Management and Remediation Services"
  tmp$sector[tmp$industries == 61] <- "Educational Services"
  tmp$sector[tmp$industries == 62] <- "Health Care and Social Assistance"
  tmp$sector[tmp$industries == 71] <- "Arts, Entertainment, and Recreation"
  tmp$sector[tmp$industries == 72] <- "Accommodation and Food Services"
  tmp$sector[tmp$industries == 81] <- "Other Services (except Public Administration)"
  tmp$sector[tmp$industries == 92] <- "Public Administration"
  } 
