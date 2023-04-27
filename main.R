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



account_types = customer_entries$aggregate('[{"$project":{"name": 1 ,"address": 1 }}]')

user_types = trips_collection$aggregate('[{"$group":{"_id":"$usertype", "Count": {"$sum":1}}}]')

df <- as.data.frame(account_types)
df


df[ , 2:3] <- str_split_fixed(df$address, ", ", 2)
df[ , 3:4] <- str_split_fixed(df$address, " ", 2)
addresses <- df[,3:4]

vec <- c()

for (i in 1:nrow(addresses)){
  count = 0
  if(addresses[i, 1] == "" || nchar(addresses[i,1]) > 2){
      count = count + 1
  }
  if(count > 0){
    vec <- append(vec, i)
  }
}

another_df <- addresses[-vec, ]
another_df

counts <- table(another_df$address)

barplot(counts, main="distribution of customers across different states",
        xlab = "States", las = 2)