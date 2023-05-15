library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(mongolite)
library(modelr)
library(data.table)

options(na.action =  na.warn)

connection_string = 'mongodb+srv://Lann0007:zw7Bi5ikPUbmwrqz@comp2031.x9dfsly.mongodb.net/?retryWrites=true&w=majority'

customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)
account_entries = mongo(collection = "accounts", db = "samplle_analytics", url = connection_string)
transaction_entries = mongo(collection = "transactions", db = "samplle_analytics", url = connection_string)

customer_entries <- customer_entries$aggregate('[{"$project":{"address": 1, "email": 1 }}]')
customer_ages <- customer_entries$aggregate('[{"$project": {"address": 1, "birthdate": 1}}]')
df_account <- as.data.frame(customer_entries)

df_account$email <- gsub(".*@", "",df_account$email)

nrow(df_account$address)
for (i in 1:nrow(df_account)){
    df_account$address[i] <- str_extract(df_account$address[i], "\\b[A-Z]{2}\\b", group = NULL) 
}

df_table <- table(df_account$address, df_account$email)

df_account <- as.data.frame(df_table)



df_yahoo <- filter(df_account, df_account$Var2 == "yahoo.com")

View(df_yahoo)

ggplot(df_yahoo, aes(Var1,Freq)) + geom_point()

df_yahoo_mod <- lm(df_yahoo$Freq ~ df_yahoo$Var1, data = df_yahoo)
coef(df_yahoo_mod)

grid <- df_yahoo %>% data_grid(Var1)

grid <- grid %>% add_predictions(df_yahoo_mod)

grid

ggplot(df_yahoo, aes(Var1)) + 
    geom_point(aes(y = Freq)) + 
    geom_point(data = grid, aes(y=pred), colour="red", size=4)
# whilst the above code does tecnically produce a model that 
# can predict some trends, the data used is terrible and 
# inefficient


# the next approach is:
# get timeline of customers based on when they became a member

customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)
customer_ages <- customer_entries$aggregate('[{"$project": {"address": 1, "birthdate": 1}}]')
df_ages <- as.data.frame(customer_ages)

for (i in 1:nrow(df_ages)){
    df_ages$address[i] <- str_extract(df_ages$address[i], "\\b[A-Z]{2}\\b", group = NULL) 
}

df_ages$address

rename(df_ages, age = birthdate)
df_ages$birthdate <- format(df_ages$birthdate, format="%Y")

df_total <- sum

# another useless example, moving on. realised I need to grab each accounts first transaction rather than
# peoples age lol

# pull the data using mongo
transaction_entries = mongo(collection = "transactions", db = "sample_analytics", url = connection_string)
transaction_dates <- transaction_entries$aggregate('[{"$project": {"bucket_start_date": 1, "bucket_end_date": 1}}]')

df_dates <- as.data.frame(transaction_dates)

#since we're comparing between yearly memberships, only part of relevance from the buckets is the year
df_dates$bucket_start_date <- format(df_dates$bucket_start_date, format="%Y")
df_dates$bucket_end_date <- format(df_dates$bucket_end_date, format="%Y")

#to integer for comparisons
df_dates$bucket_start_date <- strtoi(df_dates$bucket_start_date)
df_dates$bucket_end_date <- strtoi(df_dates$bucket_end_date)

#create new dataframe
Year <- c(1966:2016)
Count <- rep(0,51)

df_total <- data.frame(Year, Count)

#check for "active" members that have an account, total up the amount of active accounts each year
for (i in 1:51){
df_total[i,2] <- count(subset(df_dates, df_dates$bucket_start_date <= i+1965 & df_dates$bucket_end_date > i+1965))
}
df_total
