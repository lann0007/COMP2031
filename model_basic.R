library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(mongolite)
library(modelr)
library(data.table)
library(Metrics)
library(factoextra)

library(cluster)

options(na.action =  na.warn)

connection_string = 'mongodb+srv://Lann0007:zw7Bi5ikPUbmwrqz@comp2031.x9dfsly.mongodb.net/?retryWrites=true&w=majority'

customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)
account_entries = mongo(collection = "accounts", db = "samplle_analytics", url = connection_string)
transaction_entries = mongo(collection = "transactions", db = "samplle_analytics", url = connection_string)

customer_entries <- customer_entries$aggregate('[{"$project":{"address": 1, "email": 1 }}]')
customer_ages <- customer_entries$aggregate('[{"$project": {"address": 1, "birthdate": 1}}]')
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
# whilst the above code does tecnically produce a model that 
# can predict some trends, the data used is terrible and 
# inefficient


# the next approach is:
# get timeline of customers based on when they became a member

customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)
customer_ages <- customer_entries$aggregate('[{"$project": {"address": 1, "birthdate": 1}}]')
df_ages <- as.data.frame(customer_ages)

for (i in 1:nrow(df_ages)){
    df_ages$address[i] <- str_extract(df_ages$address[i], "\\b[A-Z]{2}\\b", group = NULL) 
}

df_ages$address

rename(df_ages, age = birthdate)
df_ages$birthdate <- format(df_ages$birthdate, format="%Y")

df_total <- sum

# another useless example, moving on. realised I need to grab each accounts first transaction rather than
# peoples age lol

##################
##### MODEL 1 
#################


# pull the data using mongo
transaction_entries = mongo(collection = "transactions", db = "sample_analytics", url = connection_string)
transaction_dates <- transaction_entries$aggregate('[{"$project": {"bucket_start_date": 1, "bucket_end_date": 1}}]')

df_dates <- as.data.frame(transaction_dates)

#since we're comparing between yearly memberships, only part of relevance from the buckets is the year
df_dates$bucket_start_date <- format(df_dates$bucket_start_date, format="%Y")
df_dates$bucket_end_date <- format(df_dates$bucket_end_date, format="%Y")

#to integer for comparisons
df_dates$bucket_start_date <- strtoi(df_dates$bucket_start_date)
df_dates$bucket_end_date <- strtoi(df_dates$bucket_end_date)

#create new dataframe
Year <- c(1966:2016)
Count <- rep(0,51)

df_total <- data.frame(Year, Count)

#check for "active" members that have an account, total up the amount of active accounts each year
for (i in 1:51){
df_total[i,2] <- count(subset(df_dates, df_dates$bucket_start_date <= i+1965 & df_dates$bucket_end_date > i+1965))
}

dt = sample(nrow(df_total), nrow(df_total)*.8)
train<-df_total[dt,]
test<-df_total[-dt,]

#training and modelling of linear regression model glm
df_years_mod <- glm(Count ~ Year, data = train)
summary(glm(Count ~ Year, data = train))
coef(df_years_mod)

predicted_glim <- predict(df_years_mod, test, type = 'response')

grid <- df_total %>% data_grid(Year)
grid <- grid %>% add_predictions(df_years_mod)
grid

plot(test$Year,predicted_glim )

rmse(predicted_glim, test$Year)

#(df_total, aes(Year)) + 
 #   geom_point(aes(y = Count)) + 
  #  geom_point(data = grid, aes(y=pred), colour="red", size=4)

#training and modelling of linear model lm

df_years_mod_lin <- lm(Count ~ Year, data = train)
coef(df_years_mod)

predicted_lim <- predict(df_years_mod_lin, test, type = 'response')


grid <- df_total %>% data_grid(Year)
grid <- grid %>% add_predictions(df_years_mod)
grid

plot(test$Year,predicted_lim )

rmse(predicted_lim, test$Year)

# Measure performance through the calculating RMSE
sim1 <- df_total %>%
  add_predictions(df_years_mod)
View(sim1)
df_total

rmse(sim1$Count, sim1$pred)

##################
##### DATA WRANGLING FOR CLUSTERING
#################

customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)

customer_entries <- customer_entries$aggregate('[{"$project":{"address": 1, "email": 1, "birthdate": 1 }}]')

df_account <- as.data.frame(customer_entries)

df_account$email <- gsub(".*@", "",df_account$email)
df_account$birthdate <- format(as.Date(df_account$birthdate, format="%d-%m-%Y"),"%Y")
state = c("AA","AE","AL","AK","AP","AZ","AR","CA","CO","CT","DC","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
df_temp <- df_account
for (i in 1:nrow(df_account)){
    df_account$address[i] <- str_extract(df_account$address[i], "\\b[A-Z]{2}\\b", group = NULL)
    df_temp$address[i] <- df_account$address[i]
    df_account$address[i] <- as.numeric(which(df_account$address[i] == state)) 
}
df_temp
df_table <- table(df_account$address, df_account$email)

df_account <- as.data.frame(df_table)

df_account$Freq <- scale(df_account$Freq)
df_account <- na.omit(df_account)
df_account <- df_account %>% select(-Var2)
class(df_account$Freq)
 
df_account$Var1 <- as.numeric(unlist(df_account$Var1))
##################
##### K-Means Clustering
#################

#determine optimal amount of clusters
df_account <- scale(df_account)
fviz_nbclust(df_account, kmeans, method = "wss")

set.seed(2)
df_cluster <- kmeans(df_account[,2:1], center=3, nstart=50)

# df_cluster

# #required libraries for visualisation
# library(ggsignif)
# library(rstatix)

# df_account$Cluster <- df_cluster$cluster
# df_account$Cluster <- factor(df_account$Cluster)

# df_account_vis <- df_account
# for (i in 1:nrow(df_account)){
#     df_account$address[i] <- state[df_account$Var1[i]] 
# }


fviz_cluster(df_cluster, data = df_account)
# df_account
# ggplot(df_account_vis, aes(address,Freq)) + geom_point(aes(col=Cluster), size = 4)

##################
##### K-Medoid Clustering
#################
fviz_nbclust(df_account, pam, method = "wss")

PAM = pam(df_account, 4, metric = "euclidean", stand = FALSE)
gap_stat <- clusGap(as.matrix(df_account),
                    FUN = pam,
                    K.max = 10, #max clusters to consider
                    B = 50) #total bootstrapped iterations

fviz_gap_stat(gap_stat)

fviz_cluster(PAM, data = df_account)
class(PAM)
##################
##### Heirarchical Clustering
#################

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")


ac <- function(x) {
  agnes(df_account, method = x)$ac
}

sapply(m, ac)

clust <- agnes(df_account, method = "ward")

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 


gap_stat <- clusGap(df_account, FUN = hcut, nstart = 25, K.max = 10, B = 50)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)


d <- dist(df_account, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )
final_clust
#cut the dendrogram into 4 clusters
groups <- cutree(final_clust, k = 5)

final_data <- cbind(df_account, cluster = groups)
final_data                              

ggplot(final_data, aes(Year,Count)) + geom_point(aes(col=cluster), size = 4)
fviz_cluster(PAM, data = final_data)

#find number of observations in each cluster
table(groups)


# make a very similar model, but sort based on the different emails

##################
##### MODEL 2
#################

# // realised how much work this was gonna be so I've put it to the backburner for the moment

# transaction_entries = mongo(collection = "transactions", db = "sample_analytics", url = connection_string)
# transaction_dates <- transaction_entries$aggregate('[{"$project": {"bucket_start_date": 1, "bucket_end_date": 1}}]')

# customer_entries = mongo(collection= "customers", db = "sample_analytics", url = connection_string)
# customer_entries <- customer_entries$aggregate('[{"$project":{"address": 1}}]')

# transaction_dates$new_col <- customer_entries$address
# df_dates <- as.data.frame(transaction_dates)

# #since we're comparing between yearly memberships, only part of relevance from the buckets is the year
# df_dates$bucket_start_date <- format(df_dates$bucket_start_date, format="%Y")
# df_dates$bucket_end_date <- format(df_dates$bucket_end_date, format="%Y")

# #to integer for comparisons
# df_dates$bucket_start_date <- strtoi(df_dates$bucket_start_date)
# df_dates$bucket_end_date <- strtoi(df_dates$bucket_end_date)


# df_dates$email <- 

# #create new dataframe
# Year <- c(1966:2016)
# Count <- rep(0,51)

# df_total <- data.frame(Year, Count)

# #check for "active" members that have an account, total up the amount of active accounts each year
# for (i in 1:51){
# df_total[i,2] <- count(subset(df_dates, df_dates$bucket_start_date <= i+1965 & df_dates$bucket_end_date > i+1965))
# }


# df_total



# df_years_mod <- lm(df_total$Count ~ df_total$Year, data = df_total)
# coef(df_years_mod)

# grid <- df_total %>% data_grid(Year)
# grid <- grid %>% add_predictions(df_years_mod)
# grid

# ggplot(df_total, aes(Year)) + 
#     geom_point(aes(y = Count)) + 
#     geom_point(data = grid, aes(y=pred), colour="red", size=4)