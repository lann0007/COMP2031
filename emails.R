library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(mongolite)
library(lessR)
library(usmap)
connection_string = 'mongodb+srv://Lann0007:zw7Bi5ikPUbmwrqz@comp2031.x9dfsly.mongodb.net/?retryWrites=true&w=majority'



customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)


customer_entries <- customer_entries$aggregate('[{"$project":{"address": 1, "email": 1 }}]')

df_account <- as.data.frame(customer_entries)
df_account$address[1]

df_account$email <- gsub(".*@", "",df_account$email)

df_account

nrow(df_account$address)
for (i in 1:nrow(df_account)){
    df_account$address[i] <- str_extract(df_account$address[i], "\\b[A-Z]{2}\\b", group = NULL) 
}

df_table <- table(df_account$address, df_account$email)

df_account <- as.data.frame(df_table)



df_yahoo <- filter(df_account, df_account$Var2 == "yahoo.com")
df_gmail <- filter(df_account, df_account$Var2 == "gmail.com")
df_hotmail <- filter(df_account, df_account$Var2 == "hotmail.com")

df_yahoo <- subset(df_yahoo, select = -c(Var2))
df_gmail <- subset(df_gmail, select = -c(Var2))
df_hotmail <- subset(df_hotmail, select = -c(Var2))

sim1_mod <- lm(x, data = df_yahoo)

numerical_data <- df_yahoo[, 2]

head(numerical_data)

data_normalized <- scale(numerical_data)
head(data_normalized)

corr_matrix <- cor(data_normalized)

ggcorrplot(corr_matrix)
df_yahoo

# data.pca <- princomp(corr_matrix)
# summary(data.pca)


################################
######### TRAINING
################################



# dataset <- df_yahoo

# validation_index <- createDataPartition(dataset$Freq, p=0.80, list=FALSE)
# # select 20% of the data for validation
# validation <- dataset[-validation_index,]
# # use the remaining 80% of data to training and testing the models
# dataset <- dataset[validation_index,]
# # dimension of the dataset
# dim(dataset)

# sapply(dataset, class)

# head(dataset)

# levels(dataset$Freq)

# percentage <- prop.table(table(dataset$Freq)) * 100
# cbind(freq=table(dataset$Freq), percentage=percentage)

# summary(dataset)

# x <- dataset[,1]
# y <- dataset[,2]

# par(mfrow=c(1,54))

# control <- trainControl(method = "cv", number=10)
# metric <- "Accuracy"

# set.seed(7)
# str(x)
# fit.lda <- train(Freq~., data = dataset, method = "rf", trControl = control)
# summary(fit.lda)
# plot(y)

# predictions <- predict(fit.lda, validation)

# head(predictions)
# head(validation$Freq)
# confusionMatrix(predictions, validation)
# sum(df_yahoo$Freq)
# representation of the same data in a pie chart. It doesn't really show much but it is however
# another representation that can be used
# Note: currently showing for df_yahoo, change would have to be specified in the line below


PieChart(df_yahoo$Freq, hole = 0, values = "%", data = df_yahoo$Freq, fill = 1:54, main = "")
pie = ggplot(df_yahoo, aes(x="", y=Freq, fill=Var1 )) + geom_bar(stat="identity", width=1)
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round((Freq/sum(Freq))*100), "%")), position = position_stack(vjust = 0.5))
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Hotmail users across America")
# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, color = "#666666"))
pie


##################
#### BARPLOTS
##################


# barplot(height = df_yahoo$Freq[order(df_yahoo$Freq)], ylim = c(0,10),name = df_yahoo$Var1, las =2) + 
# legend(x = "topright",
#        inset = c(-0.45, 0), # You will need to fine-tune the first
#                             # value depending on the windows size
#        legend = c(df_yahoo$Var1), 
#        lty = c(1, 2),
#        col = c(2, 3),
#        lwd = 2,
#        xpd = TRUE) 

# barplot(height = df_hotmail$Freq[order(df_hotmail$Freq)], ylim = c(0,10),name = df_hotmail$Var1, las =2) + 
# legend(x = "topright",
#        inset = c(-0.45, 0), # You will need to fine-tune the first
#                             # value depending on the windows size
#        legend = c(df_hotmail$Var1), 
#        lty = c(1, 2),
#        col = c(2, 3),
#        lwd = 2,
#        xpd = TRUE) 

# barplot(height = df_gmail$Freq[order(df_gmail$Freq)], ylim = c(0,10),name = df_gmail$Var1, las =2) + 
# legend(x = "topright",
#        inset = c(-0.45, 0), # You will need to fine-tune the first
#                             # value depending on the windows size
#        legend = c(df_gmail$Var1), 
#        lty = c(1, 2),
#        col = c(2, 3),
#        lwd = 2,
#        xpd = TRUE) 

ggplot(df_yahoo, aes(y = df_yahoo$Freq[order(df_yahoo$Freq)], x = df_yahoo$Var1, fill = Var1)) +
  geom_bar(stat = "identity") + 
  scale_fill_grey()

ggplot(df_yahoo, aes(y = df_gmail$Freq[order(df_gmail$Freq)], x = df_gmail$Var1, fill = Var1)) +
  geom_bar(stat = "identity") + 
  scale_fill_grey()
  
  ggplot(df_yahoo, aes(y = df_hotmail$Freq[order(df_hotmail$Freq)], x = df_hotmail$Var1, fill = Var1)) +
  geom_bar(stat = "identity") + 
  scale_fill_grey()

##################
#### HEATMAP ATTEMPT?
##################

#### YAHOO


state = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
values = c()


for(i in 1:length(state)){
    values[i] <- df_yahoo$Freq[df_yahoo$Var1 == state[i]]
}

df_yahoo_rnm <- data.frame(state,values)

plot_usmap(data = df_yahoo_rnm, values = "values" ) + 
  scale_fill_continuous(name = "Total email accounts (yahoo)", label = scales::comma) + 
  theme(legend.position = "right")


for(i in 1:length(state)){
    values[i] <- df_hotmail$Freq[df_hotmail$Var1 == state[i]]
}

df_hotmail_rnm <- data.frame(state,values)

plot_usmap(data = df_hotmail_rnm, values = "values" ) + 
  scale_fill_continuous(name = "Total email accounts (hotmail)", label = scales::comma) + 
  theme(legend.position = "right")


for(i in 1:length(state)){
    values[i] <- df_gmail$Freq[df_gmail$Var1 == state[i]]
}

df_gmail_rnm <- data.frame(state,values)

plot_usmap(data = df_gmail_rnm, values = "values" ) + 
  scale_fill_continuous(name = "Total email accounts (gmail)", label = scales::comma) + 
  theme(legend.position = "right")


barplot(height = df_gmail$Freq[order(df_gmail$Freq)], ylim = c(0,10), name = df_yahoo$Var1, las =2)
barplot(height = df_hotmail$Freq[order(df_hotmail$Freq)], ylim = c(0,10), name = df_yahoo$Var1, las =2)

df_hotmail$Freq[order(df_hotmail$Freq)]
df_gmail$Freq[order(df_gmail$Freq)]
df_yahoo$Freq[order(df_yahoo$Freq)]

df_hotmail$Var1[order(df_hotmail$Freq)]
df_gmail$Var1[order(df_gmail$Freq)]
df_yahoo$Var1[order(df_yahoo$Freq)]

df_account_grp <- df_account %>% group_by(df_account$Var1, df_account$Var2)
df_account_grp
sum(df_yahoo$Freq)
sum(df_gmail$Freq)
sum(df_hotmail$Freq)
ggplot(df_account_grp, aes(x=df_account$Freq)) + geom_bar(position = "dodge")
