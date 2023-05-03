library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(mongolite)

connection_string = 'mongodb+srv://Lann0007:zw7Bi5ikPUbmwrqz@comp2031.x9dfsly.mongodb.net/?retryWrites=true&w=majority'



customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)


customer_entries <- customer_entries$aggregate('[{"$project":{"address": 1, "email": 1 }}]')

df_account <- as.data.frame(customer_entries)
df_account$address[1]

df_account$email <- gsub(".*@", "",df_account$email)

df_account

nrow(df_account$address)
for (i in 1:nrow(df_account)){
    df_account$address[i] <- str_extract(df_account$address[i], "\\b[A-Z]{2}\\b", group = NULL) 
}