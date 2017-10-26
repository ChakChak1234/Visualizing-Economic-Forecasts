library(dplyr)
library(plyr)

# Read in Data for Mexico
mexico_base <- read.csv("Mexico - reference scenario.csv", header=TRUE, sep=",")
mexico_prowomen <- read.csv("Mexico - prowomen.csv", header=TRUE, sep=",")

# Remove extra columns
mexico_base <- mexico_base[1:990,c(3,25,28,45:59,61:77)]
mexico_prowomen <- mexico_prowomen[1:990,c(3,25,28,45:76)]

# Merge data frames
Mexico_data <- do.call(rbind, Map(cbind, Scenario = c("mexico_base","mexico_prowomen"), 
                           mget(paste0("mexico", c("_base", "_prowomen")))))

# Reset row.names
rownames(Mexico_data) <- NULL

# Reshape dataframe from wide to long
Mexico_data_long <- Mexico_data %>% 
  gather(var_year, Value, starts_with("Income"), starts_with("Energy")) %>% 
  separate(var_year, c("Variable", "Year"), -5)

# Add Column for country, and reorganize data frame
Mexico_data_long["Country"] <- as.factor("Mexico")
Mexico_data_long <- Mexico_data_long[c('Variable', 'Country', 'Scenario', 'Year', 'Quantile', 'Value')]

# Convert from character to integer
Mexico_data_long$Year <- as.integer(Mexico_data_long$Year)

# Remove unncessary columns
Mexico_data_long <- Mexico_data_long[,-1]

# Write to csv
write.csv(x = Mexico_data_long, file = "Mexico.csv")
