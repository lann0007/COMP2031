library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(mongolite)
library(modelr)

options(na.action =  na.warn)

connection_string = 'mongodb+srv://Lann0007:zw7Bi5ikPUbmwrqz@comp2031.x9dfsly.mongodb.net/?retryWrites=true&w=majority'

customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)

customer_entries <- customer_entries$aggregate('[{"$project":{"address": 1, "email": 1 }}]')

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

# get timeline of customers based on when they became a member