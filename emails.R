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

df_account

df_table <- table(df_account$address, df_account$email)
df_table

df_account <- as.data.frame(df_table)
df_account

df_yahoo <-filter(df_account, df_account$Var2 == "yahoo.com")
df_gmail <- filter(df_account, df_account$Var2 == "gmail.com")
df_hotmail <- filter(df_account, df_account$Var2 == "hotmail.com")

df_yahoo <- subset(df_yahoo, select = -c(Var2))
df_gmail <- subset(df_gmail, select = -c(Var2))
df_hotmail <- subset(df_hotmail, select = -c(Var2))

barplot(height = df_yahoo$Freq)
barplot(height = df_gmail$Freq)
barplot(height = df_hotmail$Freq)


# df_account_grp <- df_account %>% group_by(df_account$Var1, df_account$Var2)
# df_account_grp

# ggplot(df_account_grp, aes(x=df_account$Freq)) + geom_bar(position = "dodge")
