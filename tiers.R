library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(mongolite)


connection_string = 'mongodb+srv://Lann0007:zw7Bi5ikPUbmwrqz@comp2031.x9dfsly.mongodb.net/?retryWrites=true&w=majority'



customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)
accounts_collection = mongo(collection="accounts", db="sample_analytics", url=connection_string)
transaction_entries = mongo(collection  ="transactions", db="sampkle_analytics", url=connection_string)



account_types = customer_entries$aggregate('[{"$project":{"name": 1 ,"address": 1 }}]')
transaction_types = transaction_entries$aggregate('[{"$project": {"transaction_count": 1, "account_id": 1}}]')
user_types = accounts_collection$aggregate('[{"$group":{"_id":"$usertype", "Count": {"$sum":1}}}]')

df_account <- as.data.frame(account_types)
df_account

df_transaction <- as.data.frame(transaction_types)

df_user <- as.data.frame(user_types)