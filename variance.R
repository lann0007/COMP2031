library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(mongolite)


connection_string = 'mongodb+srv://Lann0007:zw7Bi5ikPUbmwrqz@comp2031.x9dfsly.mongodb.net/?retryWrites=true&w=majority'



customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)
accounts_collection = mongo(collection="accounts", db="sample_analytics", url=connection_string)
transaction_entries = mongo(collection  ="transactions", db="sampkle_analytics", url=connection_string)

customer_entries <- customer_entries$aggregate('[{"$project": {"name": 1, "birthdate": 1}}]')

df_customer <- as.data.frame(customer_entries)
df_customer[2:3]

df_customer[c('year', 'month', 'day+time')] = str_split_fixed(df_customer$birthdate, '-',3)
df_customer[c('age')] = 2023 - strtoi(df_customer$year)
df_customer

max(df_customer$age, na.rm=TRUE) - min(df_customer$age, na.rm = TRUE)
boxplot(df_customer$age)

barplot(height = df_customer$age, name = df_customer$year)
