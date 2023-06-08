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


#Times Bought and sold
times_bought <- buy %>% count(symbol)
times_sold <- sell %>% count(symbol)

#Quantity of each symbol sold
Total_Quantity_Buy <- aggregate(amount ~ symbol, data=buy, sum)
Total_Quantity_Sell <- aggregate(amount ~ symbol, data=sell, sum)
sum(buy$amount)
sum(sell$amount)


#Bar Charts
ggplot(times_bought, aes(x=symbol, y=n))+
  geom_bar(stat = "identity")

ggplot(times_sold, aes(x=symbol, y=n))+
  geom_bar(stat = "identity")

#Point Plots
ggplot(times_bought, aes(x=symbol, y=n))+
  geom_point(stat = "identity")

ggplot(times_sold, aes(x=symbol, y=n))+
  geom_point(stat = "identity")


# Buy piechart
ggplot(times_bought, aes(x="", y=n, fill=symbol)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void() # remove background, grid, numeric labels

# Sell piechart
ggplot(times_bought, aes(x="", y=n, fill=symbol)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void() # remove background, grid, numeric labels

