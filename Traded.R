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
#add name
times_bought$type <- "buy"
times_bought <- times_bought[c(3, 1, 2)]
times_sold$type <- "sold"
times_sold <- times_sold[c(3, 1, 2)]

#combine times_bought and times_sold, then rename n to Count
transaction_count <- rbind(times_bought,times_sold)

colnames(transaction_count)[3] ="Count"


#create chart showing times bought and sold on same graph
ggplot(transaction_count, aes(symbol, Count, fill = type)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")




#total transactions
sum(transaction_count$Count)
sum(times_bought$n)
sum(times_sold$n)




#Quantity of each symbol sold
Total_Quantity_Buy <- aggregate(amount ~ symbol, data=buy, sum)
Total_Quantity_Sell <- aggregate(amount ~ symbol, data=sell, sum)
sum(buy$amount)
sum(sell$amount)

#Same as above but for quantity
Total_Quantity_Buy$type <- "bought_amount"
Total_Quantity_Buy <- Total_Quantity_Buy[c(3, 1, 2)]
Total_Quantity_Sell$type <- "sold_amount"
Total_Quantity_Sell <- Total_Quantity_Sell[c(3, 1, 2)]

Total_Quantity <- rbind(Total_Quantity_Buy,Total_Quantity_Sell)

#graph
ggplot(Total_Quantity, aes(symbol, amount, fill = type)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1")



#total Quantity
sum(Total_Quantity$amount)
sum(Total_Quantity_Buy$amount)
sum(Total_Quantity_Sell$amount)

test <- data.frame(
  Type=c("Total Quantity Bought", "Total Quantity Sold"),
  count=c(sum(Total_Quantity_Buy$amount),sum(Total_Quantity_Sell$amount))
)

rownames(test) <- c("219785213","220042344")

ggplot(test, aes(x=Type, y=count))+
  geom_point(stat = "identity")+
  ylim(219700000, 220050000)+
  geom_text(
    label=rownames(test), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T)




