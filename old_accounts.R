install.packages("mongolite")
install.packages("tidyverse", dependencies = T)
install.packages("ggplot2")


library(tidyverse)
library(ggplot2)
library(mongolite)


connection_string = 'mongodb+srv://smul0002:2V9NiiU6spHC2Gkz@cluster0.tfzpkuq.mongodb.net/?retryWrites=true&w=majority'

transactions_collection = mongo(collection = "transactions", db = "sample_analytics", url = connection_string)



dt_collection = transactions_collection$aggregate('[{"$project": {"account_id":1,"bucket_end_date":1, "transactions": 1}}]')

df <- as.data.frame(dt_collection)

#grab the last dates from each accounts transaction table.
accounts_last <- c()
for (i in 1:nrow(df)){
  newframe = df[[4]][[i]]
  sorted = newframe[order(newframe$date, decreasing=TRUE),]
  accounts_last <- rbind(accounts_last,sorted[1,][1])
}



# I now realize this is completely pointless...
# dates <- c()
# for (i in 1:nrow(accounts_last)){
#   x = as.Date(accounts_last[i,],'%Y-%m-%d')
#   dates <- rbind(dates,as.data.frame(x))
# }

#account names and last transaction dates
last_transactions <- cbind(as.data.frame(df[,2]),accounts_last)

#test
accounts_last[1,] - accounts_last[2,]


#list accounts older than 100 days (dead) and younger (alive)
dead_accounts <- c()
alive_accounts <- c()
final_transaction = as.Date('2017-01-09 10:30:00')
for (i in 1:nrow(last_transactions)){
  diff_dates = difftime(final_transaction,last_transactions[i,2], units = "days")
  if (diff_dates > 100){
    dead_accounts <- rbind(dead_accounts, last_transactions[i,])
  }else{
    alive_accounts <- rbind(alive_accounts, last_transactions[i,])
  }
}




#junk test code

last_transactions[3,2] - accounts_last[2,]

diff_dates = difftime(final_transaction,last_transactions[1,2], units = "days")
diff_dates



