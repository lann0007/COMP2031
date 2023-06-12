library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(mongolite)
library(data.table)


connection_string = 'mongodb+srv://Lann0007:zw7Bi5ikPUbmwrqz@comp2031.x9dfsly.mongodb.net/?retryWrites=true&w=majority'



customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)
accounts_collection = mongo(collection="accounts", db="sample_analytics", url=connection_string)
transaction_entries = mongo(collection  ="transactions", db="sampkle_analytics", url=connection_string)



customer_entries <- customer_entries$aggregate('[{"$project":{"tier_and_details": 1 }}]')
transaction_types = transaction_entries$aggregate('[{"$project": {"transaction_count": 1, "account_id": 1}}]')
user_types = accounts_collection$aggregate('[{"$group":{"_id":"$usertype", "Count": {"$sum":1}}}]')

df_account <- as.data.frame(customer_entries)
df_account$tier_and_details
df_account %>% group_by(tier_and_details) %>% summarise(n())
df_account$tier_and_details[df_account$tier_and_details == "bronze"]
df_account$bronze

bronze <- df_account$tier_and_details[".tier",]
bronze %>% filter(bronze)
bronze$tier
df <- as.data.table(bronze, TRUE)

colnames(df)
class(df_account$tier_and_details)

df_transaction <- as.data.frame(transaction_types)

df_user <- as.data.frame(user_types)