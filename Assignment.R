#install.packages("mongolite")
#install.packages("tidyverse")
#install.packages("rjson",  dependencies = T)

#The following libraries are used:
#Mongolite - database connection and export
#Tidyverse - data processing 
#Rjson - Used for importing JSON files back into R

library(mongolite)
library(tidyverse)
library(rjson)

#The connection string is defined, and three collections are created:

connection_string = 'mongodb+srv://mcqu0098:JagEM2aYWWwT3gQ@cluster0.dmyz9kd.mongodb.net/?retryWrites=true&w=majority'
accounts_collection = mongo(collection = "accounts", db = "sample_analytics", url = connection_string)
customers_collection = mongo(collection = "customers", db = "sample_analytics", url = connection_string)
transactions_collection = mongo(collection = "transactions", db = "sample_analytics", url = connection_string)

#Test the database connection - Only necessary at the start of each session

#accounts_collection$count()
#customers_collection$count()
#transactions_collection$count()

#View the available functions in mongolite - use this if you want to see what functions are available 
#View(accounts_collection)

#Export the collections as a JSON file for processing - This will save the database to a file on your local machine
accounts_collection$export(file("accounts_collection.json"))
customers_collection$export(file("customers_collection.json"))
transactions_collection$export(file("transactions_collection.json"))


# *** You can ignore the following lines up to line 44 : 

#Import database back to mongodb - Not currently used
#accounts_collection$import(file("accounts_collection.json"))

# Query for finding the buy transactions is:
# {"transactions.transaction_code":"buy"}
# and for sell transactions, it is:
# {"transactions.transaction_code":"sell"}

# ***

#View the collections to make sure they are not empty - this checks to see if you can connect to the database

#View(accounts_collection)

#View(customers_collection)

#View(transactions_collection)



# ***
#
# Before running the code in the next lines, click "Import Dataset" in the top right hand 
# corner of your screen and select "accounts_collection.json". Leave the settings
# on default and click import.
#
# ***

accounts_collection <- rename(accounts_collection, "id" = V1)
accounts_collection <- rename(accounts_collection, "account_id" = V2)
accounts_collection <- rename(accounts_collection, "limit" = V3)
accounts_collection <- rename(accounts_collection, "products" = V4)


# ***


#The following line can be used to combine two columns together
#For example, it could be used for joining together products + V4 + V5 + V6 + V7
#Which would combine all products into one column - It is not currently used.

#accounts_collection$products <- paste(accounts_collection$products, accounts_collection$V5)


# *** Data Cleaning 

#We can use the gsub function to remove 
#unwanted characters in each row of accounts_collection:

accounts_collection$products <- gsub(" \\] \\}", "", accounts_collection$products, fixed = FALSE)
accounts_collection$products <- gsub(" products : \\[ ", "", accounts_collection$products, fixed = FALSE)
accounts_collection$V5 <- gsub(" \\] \\}", "", accounts_collection$V5, fixed = FALSE)
accounts_collection$V6 <- gsub(" \\] \\}", "", accounts_collection$V6, fixed = FALSE)
accounts_collection$V7 <- gsub(" \\] \\}", "", accounts_collection$V7, fixed = FALSE)
accounts_collection$account_id <- gsub(" account_id : ", "", accounts_collection$account_id, fixed = FALSE)
accounts_collection$limit <- gsub("limit : ", "", accounts_collection$limit, fixed = FALSE)
accounts_collection$id <- gsub("\\{ \\_id : \\{ \\$oid : ", "", accounts_collection$id, fixed = FALSE)
accounts_collection$id <- gsub(" \\}", "", accounts_collection$id, fixed = FALSE)


#Create a new column to accommodate the "InvestmentStock ]" strings that have
# wrapped around to the id column. 
# So we will create a new column 
accounts_collection['V8'] <- NA

#To add our investment stock string to the previous row, we are going to use a for loop
# For each item in accounts_collection, if id = " InvestmentStock ]"
# then add "InvestmentStock" to the n-1 row in column 8.
# Only run this for loop once.

for (i in 1:nrow(accounts_collection)) {
  # Check if the first column contains the string " InvestmentStock ]"
  if (grepl(" InvestmentStock \\]", accounts_collection[i, 1])) {
    # Output "InvestmentStock" into the (i-1)th row of the data frame

    accounts_collection[i - 1, 8] <- "InvestmentStock"
  }
}

#Counting how many occurrences there are of each product in the products column
table(accounts_collection$products)

#Counting how many occurrences there are of each product in the other columns
#that contain products 
table(accounts_collection$V5)
table(accounts_collection$V6)
table(accounts_collection$V7)
table(accounts_collection$V8)

#From here we can work out most popular products, link to other columns etc...

# ***
#
# Before running the code in the next lines, click "Import Dataset" in the top right hand 
# corner of your screen and select "customers_collection.json". Leave the settings
# on default and click import.
#
# ***

#Renaming columns in customers_collection
customers_collection <- rename(customers_collection, "id" = V1)
customers_collection <- rename(customers_collection, "username" = V2)
customers_collection <- rename(customers_collection, "fullname" = V3)
customers_collection <- rename(customers_collection, "address" = V4)
customers_collection <- rename(customers_collection, "birthdate" = V5)
customers_collection <- rename(customers_collection, "email" = V6)
customers_collection <- rename(customers_collection, "accounts" = V7)


#Remove unwanted characters in customers_collection 

customers_collection$accounts <- gsub(" active : ", "", customers_collection$accounts, fixed = FALSE)
customers_collection$accounts <- gsub("accounts : \\[ ", "", customers_collection$accounts, fixed = FALSE)
customers_collection$email <- gsub(" email : ", "", customers_collection$email, fixed = FALSE)
customers_collection$birthdate <- gsub(" birthdate : \\{ \\$date : ", "", customers_collection$birthdate, fixed = FALSE)
customers_collection$birthdate <- gsub(" Z \\}", "", customers_collection$birthdate, fixed = FALSE)
customers_collection$address <- gsub(r"(\\n)", " ", customers_collection$address, fixed = FALSE)
customers_collection$address <- gsub(" address : ", "", customers_collection$address, fixed = FALSE)
customers_collection$fullname <- gsub(" name : ", "", customers_collection$fullname, fixed = FALSE)
customers_collection$username <- gsub(" username : ", "", customers_collection$username, fixed = FALSE)
customers_collection$id <- gsub("\\{ \\_id : \\{ \\$oid : ", "", customers_collection$id, fixed = FALSE)
customers_collection$id <- gsub(" \\}", "", customers_collection$id, fixed = FALSE)


#From here we can work out most popular products, link to other columns etc...

# ***
#
# Before running the code in the next 4 lines, click "Import Dataset" in the top right hand 
# corner of your screen and select "transactions_collection.json". Leave the settings
# on default and click import.
#
# ***

#Renaming columns in transactions_collection

transactions_collection <- rename(transactions_collection, "id" = V1)
transactions_collection <- rename(transactions_collection, "account_id" = V2)
transactions_collection <- rename(transactions_collection, "transaction_count" = V3)
transactions_collection <- rename(transactions_collection, "bucket_start_date" = V4)
transactions_collection <- rename(transactions_collection, "bucket_end_date" = V5)
transactions_collection <- rename(transactions_collection, "transactions" = V6)
transactions_collection <- rename(transactions_collection, "amount" = V7)
transactions_collection <- rename(transactions_collection, "transaction_code" = V8)
transactions_collection <- rename(transactions_collection, "symbol" = V9)
transactions_collection <- rename(transactions_collection, "price" = V10)
transactions_collection <- rename(transactions_collection, "total" = V11)
transactions_collection <- rename(transactions_collection, "date" = V12)

transactions_collection <- rename(transactions_collection, "amount_2" = V13)
transactions_collection <- rename(transactions_collection, "transaction_code_2" = V14)
transactions_collection <- rename(transactions_collection, "symbol_2" = V15)
transactions_collection <- rename(transactions_collection, "price_2" = V16)
transactions_collection <- rename(transactions_collection, "total_2" = V17)
transactions_collection <- rename(transactions_collection, "date_2" = V18)

transactions_collection <- rename(transactions_collection, "amount_3" = V19)
transactions_collection <- rename(transactions_collection, "transaction_code_3" = V20)
transactions_collection <- rename(transactions_collection, "symbol_3" = V21)
transactions_collection <- rename(transactions_collection, "price_3" = V22)
transactions_collection <- rename(transactions_collection, "total_3" = V23)
transactions_collection <- rename(transactions_collection, "date_3" = V24)

transactions_collection <- rename(transactions_collection, "amount_4" = V25)
transactions_collection <- rename(transactions_collection, "transaction_code_4" = V26)
transactions_collection <- rename(transactions_collection, "symbol_4" = V27)
transactions_collection <- rename(transactions_collection, "price_4" = V28)
transactions_collection <- rename(transactions_collection, "total_4" = V29)
transactions_collection <- rename(transactions_collection, "date_4" = V30)

transactions_collection <- rename(transactions_collection, "amount_5" = V31)
transactions_collection <- rename(transactions_collection, "transaction_code_5" = V32)
transactions_collection <- rename(transactions_collection, "symbol_5" = V33)
transactions_collection <- rename(transactions_collection, "price_5" = V34)
transactions_collection <- rename(transactions_collection, "total_5" = V35)
transactions_collection <- rename(transactions_collection, "date_5" = V36)

transactions_collection <- rename(transactions_collection, "amount_6" = V37)
transactions_collection <- rename(transactions_collection, "transaction_code_6" = V38)
transactions_collection <- rename(transactions_collection, "symbol_6" = V39)
transactions_collection <- rename(transactions_collection, "price_6" = V40)
transactions_collection <- rename(transactions_collection, "total_6" = V41)
transactions_collection <- rename(transactions_collection, "date_6" = V42)

transactions_collection <- rename(transactions_collection, "amount_7" = V43)
transactions_collection <- rename(transactions_collection, "transaction_code_7" = V44)
transactions_collection <- rename(transactions_collection, "symbol_7" = V45)
transactions_collection <- rename(transactions_collection, "price_7" = V46)
transactions_collection <- rename(transactions_collection, "total_7" = V47)
transactions_collection <- rename(transactions_collection, "date_7" = V48)

transactions_collection <- rename(transactions_collection, "amount_8" = V49)
transactions_collection <- rename(transactions_collection, "transaction_code_8" = V50)


#Remove unwanted characters in transactions_collection 

transactions_collection$id <- gsub("\\{ \\_id : \\{ \\$oid : ", "", transactions_collection$id, fixed = FALSE)
transactions_collection$id <- gsub(" \\}", "", transactions_collection$id, fixed = FALSE)
transactions_collection$account_id <- gsub("account_id : ", "", transactions_collection$account_id, fixed = FALSE)
transactions_collection$transaction_count <- gsub(" transaction_count : ", "", transactions_collection$transaction_count, fixed = FALSE)
transactions_collection$date <- gsub(" date : \\{ \\$date : ", "", transactions_collection$date, fixed = FALSE)
transactions_collection$date <- gsub(" Z \\}", "", transactions_collection$date, fixed = FALSE)
transactions_collection$bucket_start_date <- gsub(" bucket_start_date : \\{ \\$date : ", "", transactions_collection$bucket_start_date, fixed = FALSE)
transactions_collection$bucket_start_date <- gsub(" Z \\}", "", transactions_collection$bucket_start_date, fixed = FALSE)
transactions_collection$bucket_end_date <- gsub(" bucket_end_date : \\{ \\$date : ", "", transactions_collection$bucket_end_date, fixed = FALSE)
transactions_collection$bucket_end_date <- gsub(" Z \\}", "", transactions_collection$bucket_end_date, fixed = FALSE)

transactions_collection$transactions <- gsub(" transactions : \\[ \\{", "", transactions_collection$transactions, fixed = FALSE)
transactions_collection$transactions <- gsub(" Z \\}", "", transactions_collection$transactions, fixed = FALSE)
transactions_collection$amount <- gsub(" amount : ", "", transactions_collection$amount, fixed = FALSE)
transactions_collection$transaction_code <- gsub(" transaction_code : ", "", transactions_collection$transaction_code, fixed = FALSE)
transactions_collection$symbol <- gsub(" symbol : ", "", transactions_collection$symbol, fixed = FALSE)
transactions_collection$price <- gsub(" price : ", "", transactions_collection$price, fixed = FALSE)
transactions_collection$total <- gsub(" total : ", "", transactions_collection$total, fixed = FALSE)

transactions_collection$date_2 <- gsub(" date : \\{ \\$date : ", "", transactions_collection$date_2, fixed = FALSE)
transactions_collection$date_2 <- gsub(" Z \\}", "", transactions_collection$date_2, fixed = FALSE)
transactions_collection$amount_2 <- gsub(" amount : ", "", transactions_collection$amount_2, fixed = FALSE)
transactions_collection$transaction_code_2 <- gsub(" transaction_code : ", "", transactions_collection$transaction_code_2, fixed = FALSE)
transactions_collection$symbol_2 <- gsub(" symbol : ", "", transactions_collection$symbol_2, fixed = FALSE)
transactions_collection$price_2 <- gsub(" price : ", "", transactions_collection$price_2, fixed = FALSE)
transactions_collection$total_2 <- gsub(" total : ", "", transactions_collection$total_2, fixed = FALSE)

transactions_collection$date_3 <- gsub(" date : \\{ \\$date : ", "", transactions_collection$date_3, fixed = FALSE)
transactions_collection$date_3 <- gsub(" Z \\}", "", transactions_collection$date_3, fixed = FALSE)
transactions_collection$amount_3 <- gsub(" amount : ", "", transactions_collection$amount_3, fixed = FALSE)
transactions_collection$transaction_code_3 <- gsub(" transaction_code : ", "", transactions_collection$transaction_code_3, fixed = FALSE)
transactions_collection$symbol_3 <- gsub(" symbol : ", "", transactions_collection$symbol_3, fixed = FALSE)
transactions_collection$price_3 <- gsub(" price : ", "", transactions_collection$price_3, fixed = FALSE)
transactions_collection$total_3 <- gsub(" total : ", "", transactions_collection$total_3, fixed = FALSE)

transactions_collection$date_4 <- gsub(" date : \\{ \\$date : ", "", transactions_collection$date_4, fixed = FALSE)
transactions_collection$date_4 <- gsub(" Z \\}", "", transactions_collection$date_4, fixed = FALSE)
transactions_collection$amount_4 <- gsub(" amount : ", "", transactions_collection$amount_4, fixed = FALSE)
transactions_collection$transaction_code_4 <- gsub(" transaction_code : ", "", transactions_collection$transaction_code_4, fixed = FALSE)
transactions_collection$symbol_4 <- gsub(" symbol : ", "", transactions_collection$symbol_4, fixed = FALSE)
transactions_collection$price_4 <- gsub(" price : ", "", transactions_collection$price_4, fixed = FALSE)
transactions_collection$total_4 <- gsub(" total : ", "", transactions_collection$total_4, fixed = FALSE)

transactions_collection$date_5 <- gsub(" date : \\{ \\$date : ", "", transactions_collection$date_5, fixed = FALSE)
transactions_collection$date_5 <- gsub(" Z \\}", "", transactions_collection$date_5, fixed = FALSE)
transactions_collection$amount_5 <- gsub(" amount : ", "", transactions_collection$amount_5, fixed = FALSE)
transactions_collection$transaction_code_5 <- gsub(" transaction_code : ", "", transactions_collection$transaction_code_5, fixed = FALSE)
transactions_collection$symbol_5 <- gsub(" symbol : ", "", transactions_collection$symbol_5, fixed = FALSE)
transactions_collection$price_5 <- gsub(" price : ", "", transactions_collection$price_5, fixed = FALSE)
transactions_collection$total_5 <- gsub(" total : ", "", transactions_collection$total_5, fixed = FALSE)

transactions_collection$date_6 <- gsub(" date : \\{ \\$date : ", "", transactions_collection$date_6, fixed = FALSE)
transactions_collection$date_6 <- gsub(" Z \\}", "", transactions_collection$date_6, fixed = FALSE)
transactions_collection$amount_6 <- gsub(" amount : ", "", transactions_collection$amount_6, fixed = FALSE)
transactions_collection$transaction_code_6 <- gsub(" transaction_code : ", "", transactions_collection$transaction_code_6, fixed = FALSE)
transactions_collection$symbol_6 <- gsub(" symbol : ", "", transactions_collection$symbol_6, fixed = FALSE)
transactions_collection$price_6 <- gsub(" price : ", "", transactions_collection$price_6, fixed = FALSE)
transactions_collection$total_6 <- gsub(" total : ", "", transactions_collection$total_6, fixed = FALSE)

transactions_collection$date_7 <- gsub(" date : \\{ \\$date : ", "", transactions_collection$date_7, fixed = FALSE)
transactions_collection$date_7 <- gsub(" Z \\}", "", transactions_collection$date_7, fixed = FALSE)
transactions_collection$amount_7 <- gsub(" amount : ", "", transactions_collection$amount_7, fixed = FALSE)
transactions_collection$transaction_code_7 <- gsub(" transaction_code : ", "", transactions_collection$transaction_code_7, fixed = FALSE)
transactions_collection$symbol_7 <- gsub(" symbol : ", "", transactions_collection$symbol_7, fixed = FALSE)
transactions_collection$price_7 <- gsub(" price : ", "", transactions_collection$price_7, fixed = FALSE)
transactions_collection$total_7 <- gsub(" total : ", "", transactions_collection$total_7, fixed = FALSE)

transactions_collection$amount_8 <- gsub(" amount : ", "", transactions_collection$amount_8, fixed = FALSE)
transactions_collection$transaction_code_8 <- gsub(" transaction_code : ", "", transactions_collection$transaction_code_8, fixed = FALSE)

#Count how many occurrences are in each column 
#Example query:
table(transactions_collection$symbol)
