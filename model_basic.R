library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(mongolite)
library(modelr)
library(data.table)
library(Metrics)
library(factoextra)
install.packages("rstatix")
install.packages("factoextra")
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
##### K-Means Clustering
#################

set.seed(2)
df_cluster <- kmeans(df_total[,1:2], center=4, nstart=50)

df_cluster
df_shortened
df_shortened <- df_total[,1:2]
#required libraries for visualisation
library(ggsignif)
library(rstatix)

fviz_cluster(df_cluster, data = scale(df_shortened))

df_total$Cluster <- df_cluster$cluster
df_total$Cluster <- factor(df_total$Cluster)

view(df_total)

ggplot(df_total, aes(Year,Count)) + geom_point(aes(col=Cluster), size = 4)

##################
##### K-Medoid Clustering
#################

PAM = pam(df_shortened, 4, metric = "euclidean", stand = FALSE)
fviz_nbclust(df_shortened, pam, method = "wss")
gap_stat <- clusGap(df_shortened,
                    FUN = pam,
                    K.max = 10, #max clusters to consider
                    B = 50) #total bootstrapped iterations

fviz_gap_stat(gap_stat)

fviz_cluster(PAM, data = df_shortened)

##################
##### Heirarchical Clustering
#################

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")


ac <- function(x) {
  agnes(df_shortened, method = x)$ac
}

sapply(m, ac)

clust <- agnes(df_shortened, method = "ward")

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 


gap_stat <- clusGap(df_shortened, FUN = hcut, nstart = 25, K.max = 10, B = 50)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)


d <- dist(df_shortened, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

#cut the dendrogram into 4 clusters
groups <- cutree(final_clust, k = 5)

final_data <- cbind(df_shortened, cluster = groups)
final_data                              

ggplot(final_data, aes(Year,Count)) + geom_point(aes(col=cluster), size = 4)


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