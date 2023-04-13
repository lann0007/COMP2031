install.packages("mongolite")
install.packages("tidyverse")
install.packages("lubridate") 
install.packages("ggplot2")



library(tidyverse)
library(lubridate)
library(ggplot2)
library(mongolite)

connection_string = 'mongodb+srv://Lann0007:zw7Bi5ikPUbmwrqz@comp2031.x9dfsly.mongodb.net/?retryWrites=true&w=majority'



db_accounts = mongo(collection= "accounts", db = "sample_analytics", url = connection_string)

trips_collection = mongo(collection="trips", db="sample_training", url=connection_string)



account_types = db_accounts$aggregate('[{"$group":{"_id":"$limit", "Count": {"$sum":1}}}]')

user_types = trips_collection$aggregate('[{"$group":{"_id":"$usertype", "Count": {"$sum":1}}}]')

df <- as.data.frame(account_types)
df

ggplot(df,aes(x=reorder(`_id`,Count),y=Count))+
  geom_bar(stat="identity",color='green',fill='transparent')+geom_text(aes(label = Count), color = "red") +coord_flip()+xlab("User Type")

#collection : "inspections"

inspections_collection = mongo(collection="inspections", db="sample_training", url=connection_string)

year_failures = inspections_collection$aggregate('[{"$addFields": {"format_year":{"$year":{"$toDate":"$date"}}}},
{"$match":{"result":"Fail"}},
{"$group":{"_id":"$format_year", "Failed": {"$sum":1}}}]')

df<-as.data.frame(year_failures)

ggplot(df,aes(x=reorder(`_id`,Failed),y=Failed))+
  geom_bar(stat="identity", width=0.4, color='skyblue',fill='skyblue')+
  geom_text(aes(label = Failed), color = "black") +coord_flip()+xlab("Year")

#collection : "companies"

companies_collection = mongo(collection="companies", db="sample_training", url=connection_string)

consulting_companies_year_wise = companies_collection$aggregate('[
{"$match":{"category_code":"consulting","founded_year":{"$gt":2003}}},
{"$group":{"_id":"$founded_year", "Count": {"$sum":1}}},
{"$sort":{"_id": 1}}
]')

df<-as.data.frame(consulting_companies_year_wise)

ggplot(df,aes(x=`_id`,y=Count))+
  geom_line(size=2,color="blue")+
  geom_point(size=4,color="red")+
  ylab("Number of consulting companies")+ggtitle("Year-wise (2004 onwards) companies founded in the category 'consulting'")+xlab("Year")

#Create maps

# Get the location array objects
fb_locs = companies_collection$aggregate('[{"$match":{"name":"Facebook"}},{"$unwind":{"path":"$offices"}}]')

# Get individual fields from each array object
loc_long <- fb_locs$offices$longitude
loc_lat <- fb_locs$offices$latitude
loc_city <- fb_locs$offices$city

# Plot the map
install.packages("maps")
library(maps)

map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(loc_long,loc_lat, col="red", pch=16)
text(loc_long, y = loc_lat, loc_city, pos = 4, col="red")

#collection: "grades"

grades_collection = mongo(collection="grades", db="sample_training", url=connection_string)

class_score_allstudents = grades_collection$aggregate('[{"$match":{"class_id":7}},{"$unwind":{"path": "$scores"}},{"$project":{"scores.score":1,"_id":0,"scores.type":1,"class_id":1}}]')


score_values <- class_score_allstudents$scores$score

median(score_values)

mean(score_values)


b<-boxplot(score_values,col="orange",main = "Overall score details of all students attending class with id '7'")

stripchart(score_values, method = "jitter", pch = 19, add = TRUE, col = "black", vertical=TRUE)

b$stats

# Get the scores array
student_score_exam = grades_collection$aggregate('[{"$unwind":{"path": "$scores"}},{"$match":{"class_id":7,"scores.type":"homework"}}]')

# Get the score values for 'exam' field for all the students
scores_of_allstudents <- student_score_exam$scores$score
hist(scores_of_allstudents,col="skyblue",border="black",xlab="Scores of all students of class id 7")

sort(scores_of_allstudents)

range_scores = range(scores_of_allstudents)

range_scores


#Import data to your MongoDB cluster

test <- mongo(collection="flowers-stats",db="flowers-database",url=connection_string)
test$drop()
