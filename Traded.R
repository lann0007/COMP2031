install.packages("mongolite")
install.packages("tidyverse", dependencies = T)
install.packages("ggplot2")


library(tidyverse)
library(ggplot2)
library(mongolite)


connection_string = 'mongodb+srv://smul0002:2V9NiiU6spHC2Gkz@cluster0.tfzpkuq.mongodb.net/?retryWrites=true&w=majority'

transactions_collection = mongo(collection = "transactions", db = "sample_analytics", url = connection_string)



transaction_types = transactions_collection$aggregate('[{"$project": {"transactions": 1}}]')

df <- as.data.frame(transaction_types)

#create a list of all transactions
list <- c()
for (i in 1:nrow(df)) {
  temp <- as.data.frame(df[[2]][[i]])
  
  list <- rbind(list,(temp[,2:4]))
}

#separate buy and sell
buy <- list[list$transaction_code == 'buy', ]
sell <- list[list$transaction_code == 'sell', ]


#Buy, Sell and All orders by symbol
table(buy$symbol)
table(sell$symbol)
table(list$symbol)


#sill working on this
times_bought <- buy %>% count(symbol)
times_sold <- buy %>% count(symbol)

ggplot(times_bought, aes(x=symbol, y=n))+
  geom_bar(stat = "identity")
