install.packages("mongolite")
install.packages("tidyverse", dependencies = T)
install.packages("lubridate")
install.packages("ggplot2")
install.packages("stringr")


library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(mongolite)


connection_string = 'mongodb+srv://Lann0007:zw7Bi5ikPUbmwrqz@comp2031.x9dfsly.mongodb.net/?retryWrites=true&w=majority'



customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)

trips_collection = mongo(collection="trips", db="sample_training", url=connection_string)



account_types = customer_entries$aggregate('[{"$project":{"name": 1 ,"count": {"$sum": 1} }}]')

user_types = trips_collection$aggregate('[{"$group":{"_id":"$usertype", "Count": {"$sum":1}}}]')

df <- as.data.frame(account_types)
df

#removing unnecessary lines from the table (we're really only after the locations of each given account)
df[ , 2:3] <- str_split_fixed(df$address, ", ", 2)
df[ , 3:4] <- str_split_fixed(df$address, " ", 2)
addresses <- df[,3:4]

vec <- c()
# filter out rows with empty address entries
for (i in 1:nrow(addresses)){
  count = 0
  if(addresses[i, 1] == "" || nchar(addresses[i,1]) > 2){
      count = count + 1
  }
  if(count > 0){
    vec <- append(vec, i)
  }
}
#assign a new data frame the address values excluding the empty entries
another_df <- addresses[-vec, ]
another_df


#table transformation of just the addresses
counts <- table(another_df$address)
# resulting tallies of each state that the accounts reside from
barplot(counts, main="distribution of customers across different states",
        xlab = "States", las = 2)
