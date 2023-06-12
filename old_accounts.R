install.packages("mongolite")
install.packages("tidyverse", dependencies = T)
install.packages("ggplot2")


library(tidyverse)
library(ggplot2)
library(mongolite)
library(dplyr)


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

#to clear pie and data for testing plots.
pie <- c()
data <- c()


#Pie chart:

pie <- rbind(count(alive_accounts), count(dead_accounts))

# Add Names
data <- data.frame(
  Accounts=c('Alive Accounts:','Dead Accounts:'),
  value=pie
)

# Compute the position of labels
data <- data %>% 
  arrange(desc(Accounts)) %>%
  mutate(prop = n / sum(data$n) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Piechart
ggplot(data, aes(x="", y=prop, fill=Accounts)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = Accounts), color = "white", size=4) +
  scale_fill_brewer(palette="Set1")



#Donut Chart:
# Compute percentages
data$fraction <- data$n / sum(data$n)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$Accounts, "\n", data$n)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Accounts)) +
  geom_rect() +
  geom_text( x=.6, aes(y=labelPosition, label=label, color=Accounts), size=7) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

