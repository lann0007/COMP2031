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
# Before running the code in the next 4 lines, click "Import Dataset" in the top right hand 
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

