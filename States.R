library(mongolite)
library(tidyverse)
library(rjson)
library(usmap)
library(ggplot2)

###
### You will need to import the customers_collection.json file and run the clean up section
### for customers_collection from Assignment.R before running the following lines 
### Run this code one section at a time by highlighting and running
###

# This code is used to count how many users are in each state with a certain tier

# We need this code here to calculate the numbers needed for the heatmap (which is the next section of code)

# Creating a new data frame for customers_collection
df_bronze <- as.data.frame(customers_collection)

# Search for "Bronze" in any column
has_bronze <- apply(df_bronze, 2, grepl, pattern = "Bronze")

# Extract the addresses of the rows that contain "Bronze"
addresses <- df_bronze[apply(has_bronze, 1, any), "address"]

# Extract the state codes from the addresses
# Using a regular expression to determine any two-character word in the address string (aka the state code)
regex <- "\\b[A-Z]{2}\\b"

# Using unlist we can convert the list of addresses into the vector state_codes
state <- unlist(str_extract_all(addresses, regex))

# Count the occurrences of each state code
bronze_state_counts <- table(state)

# Generate a bar plot of the bronze state code counts
# Use las = 2 to ensure every state appears on the x axis
barplot(bronze_state_counts, main = "Bronze State Code Counts", xlab = "State Code", ylab = "Count", las = 2)


# Now we do the same process for silver, gold, and platinum

# Silver
df_silver <- as.data.frame(customers_collection)
has_silver <- apply(df_silver, 2, grepl, pattern = "Silver")
addresses <- df_silver[apply(has_silver, 1, any), "address"]
regex <- "\\b[A-Z]{2}\\b"
state_codes <- unlist(str_extract_all(addresses, regex))
silver_state_counts <- table(state_codes)
barplot(silver_state_counts, main = "Silver State Code Counts", xlab = "State Code", ylab = "Count", las = 2)

# Gold
df_gold <- as.data.frame(customers_collection)
has_gold <- apply(df_gold, 2, grepl, pattern = "Gold")
addresses <- df_gold[apply(has_gold, 1, any), "address"]
regex <- "\\b[A-Z]{2}\\b"
state_codes <- unlist(str_extract_all(addresses, regex))
gold_state_counts <- table(state_codes)
barplot(gold_state_counts, main = "Gold State Code Counts", xlab = "State Code", ylab = "Count", las = 2)

# Platinum
df_platinum <- as.data.frame(customers_collection)
has_platinum <- apply(df_platinum, 2, grepl, pattern = "Platinum")
addresses <- df_platinum[apply(has_platinum, 1, any), "address"]
regex <- "\\b[A-Z]{2}\\b"
state_codes <- unlist(str_extract_all(addresses, regex))
platinum_state_counts <- table(state_codes)
barplot(platinum_state_counts, main = "Platinum State Code Counts", xlab = "State Code", ylab = "Count", las = 2)


### HEATMAPS ###

# I have only been able to get the heatmaps to work with manual data input which is why you see "state" and "values"
# being defined in each dataframe below


#For Bronze State Count Heat Map#

bronze_state_data <- data.frame(
  state = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"), 
  values = c(2,3,1,4,4,2,3,1,2,2,2,3,1,2,2,1,3,1,1,4,3,3,1,1,1,1,1,1,1,2,3,2,2,6,1,3,1,3,2,1,2,1,2,2,3,0,0,0,0,0), color = "brown")

plot_usmap(data = bronze_state_data, values = "values", color = "brown") + 
  scale_fill_continuous(
    low = "white", high = "brown", name = "Bronze state counts", label = scales::comma
  ) + theme(legend.position = "right")

#For Silver State Count Heat Map#

silver_state_data <- data.frame(
  state = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"),
  value = c(4,4,2,3,3,2,1,2,3,1,1,1,1,1,4,3,1,1,4,2,4,3,1,1,1,4,1,2,2,3,1,3,3,2,1,1,1,2,2,1,1,1,3,3,3,0,0,0,0,0), color = "grey")

plot_usmap(data = silver_state_data, values = "value", color = "grey") + 
  scale_fill_continuous(
    low = "white", high = "grey", name = "Silver state counts", label = scales::comma
  ) + theme(legend.position = "right")

#For Gold State Count Heat Map#

gold_state_data <- data.frame(
  state = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"),
  value = c(5,2,3,6,1,1,2,2,4,1,1,2,4,3,6,1,4,1,1,1,3,3,3,3,1,1,1,3,3,2,1,1,4,1,1,2,2,3,4,2,1,2,3,0,0,0,0,0,0,0), color = "gold")

plot_usmap(data = gold_state_data, values = "value", color = "gold") + 
  scale_fill_continuous(
    low = "white", high = "gold", name = "Platinum state counts", label = scales::comma
  ) + theme(legend.position = "right")

#For Platinum State Count Heat Map#

platinum_state_data <- data.frame(
  state = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"),
  value = c(3,3,1,3,1,1,1,2,2,1,4,1,1,1,1,3,4,2,1,5,2,2,4,2,2,3,6,2,1,1,3,1,2,4,1,1,2,2,1,1,3,2,5,2,1,3,0,0,0,0))

plot_usmap(data = platinum_state_data, values = "value", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Platinum state counts", label = scales::comma
  ) + theme(legend.position = "right")