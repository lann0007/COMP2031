#install.packages("mongolite")
#install.packages("tidyverse")

library(mongolite)
library(tidyverse)

connection_string = 'mongodb+srv://USER:PASSWORD@cluster0.dmyz9kd.mongodb.net/?retryWrites=true&w=majority'
accounts_collection = mongo(collection="accounts", db="sample_analytics", url=connection_string)



#Test connection 
#accounts_collection$count()
#accounts_collection$iterate()$one()

accounts_collection$count()

#View available commands in mongolite
#View(accounts_collection)

#dump database for analysis 
accounts_collection$export(file("dump.json"))

#import database back to mongodb 
accounts_collection$import(file("dump.json"))






