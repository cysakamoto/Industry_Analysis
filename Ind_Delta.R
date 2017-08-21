####################################################
# Delta for all industries 
# Cristina Y. Sakamoto
# The University of Chicago
# 2017
# Zipcode Level, so we can use data since 1986. 
# Looking at number of jobs change, and correlation of what kind of sectors go together. 
####################################################

# To run after TransformLong_Wide.R

library(reshape2)
library(tidyr)

attach(data)

# Calculating the deltas
data$Agriculture_delta <- Agriculture_2015 - Agriculture_1998
data$Arts_delta <- Arts_2015 - Arts_1998
data$Construction_delta <- Construction_2015 - Construction_1998
data$Education_delta <- Education_2015 - Education_1998
data$Health_Care_delta <- Health_Care_2015 - Health_Care_1998
data$Hospitality_delta <- Hospitality_2015 - Hospitality_1998
data$Management_delta <- Management_2015 - Management_1998
data$Manufacturing_delta <- Manufacturing_2015 - Manufacturing_1998
data$Mining_delta <- Mining_2015 - Mining_1998
data$Other_Services_delta <- Other_Services_2015 - Other_Services_1998
data$Retail_Trade_delta <- Retail_Trade_2015 - Retail_Trade_1998
data$Utilities_delta <- Utilities_2015 - Utilities_1998
data$Wholesale_Trade_delta <- Wholesale_Trade_2015 - Wholesale_Trade_1998

detach(data)

# Describing the delta columns
describe(data[28:40])


write.csv(data, file="/home/cysakamoto/Data/results/deltas_15_98", na = "", row.names=FALSE)
