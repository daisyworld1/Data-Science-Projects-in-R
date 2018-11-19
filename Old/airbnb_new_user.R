library(tidyr)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(cluster)

#read in data
setwd("~/Documents/R/final project")
train_user <- read.csv(file="train_users_2.csv", header=TRUE, sep=",")

#categorize different ages

age_cat <- as.numeric(train_user$age)
age_cat[is.na(age_cat)] <- -1
age_cat <- ifelse(age_cat >1000, 2015 - age_cat, age_cat)
age_cat <- ifelse(age_cat >65 & age_cat < 1000,"above 65", age_cat)
age_cat <- ifelse(age_cat >55 & age_cat <= 65 ,"between 56 and 65", age_cat)
age_cat <- ifelse(age_cat >45 & age_cat <= 55 ,"between 46 and 55", age_cat)
age_cat <- ifelse(age_cat >35 & age_cat <= 45 ,"between 36 and 45", age_cat)
age_cat <- ifelse(age_cat >25 & age_cat <= 35 ,"between 26 and 35", age_cat)
age_cat <- ifelse(age_cat >18 & age_cat <= 25 ,"between 18 and 25", age_cat)
age_cat <- ifelse(age_cat > 0 & age_cat <= 18,"under 18", age_cat)
train_user <- cbind(train_user,age_cat)

train_user$age_cat <- as.character(train_user$age_cat)
train_user$age_cat <- ifelse(train_user$age_cat == 7, "under 18", train_user$age_cat)
train_user$age_cat <- ifelse(train_user$age_cat == -1, "-unknown-", train_user$age_cat)

#create a new variable gender_age
train_user$gender_age <- paste(train_user$gender, train_user$age_cat)
#categorize browser type
browser_type <- as.character(train_user$first_browser)

#view the top browser used
unique_browser <- train_user %>% 
  group_by(first_browser) %>% 
  tally()
arrange(unique_browser,desc(n))
sum(unique_browser$n)

browser_table <-  data_frame(
  first_browser = c("Chrome","Chrome Mobile", "Safari", "Safari Mobile", "Firefox", "IE"),
  browser_type = c("Chrome","Chrome","Safari","Safari","Firefox", "IE"))

train_user <- left_join(train_user, browser_table, by = "first_browser", copy = "browser_table")
train_user$browser_type[is.na(train_user$browser_type)] <- "others"

#view unique devise type
device_type <- train_user %>% 
  group_by(first_device_type) %>% 
  tally()
arrange(device_type,desc(n))
sum(device_type$n)

device_table <-  data_frame(
  first_device_type = c("Mac Desktop","Windows Desktop", "iPhone", "iPad" , "Android Phone", "Android Tablet ","Desktop (Other)","SmartPhone (Other)"),
  device_type = c("Desktop","Desktop","Portable","Portable","Portable", "Portable","Desktop","Portable"))

train_user <- left_join(train_user, device_table, by = "first_device_type", copy = "device_table")
train_user$device_type[is.na(train_user$device_type)] <- "others"


#bring in the name of each language code. "language_code.csv" came from the google search
language_code <- read.csv(file="language_code.csv",header=TRUE,sep=",")
train_user <- left_join(train_user, language_code, by = "language", copy = "language_code")
names(train_user)[names(train_user) == 'ï..language_full'] <- 'language_full'

#add primary_language to country_destination
destination_language <-  data_frame(
  country_destination = c("US", "FR", "IT", "GB", "ES", "CA", "DE", "NL", "AU","PT"),
  country_primary_language = c("English", "French", "Italian", "English", "Spanish", "English", "German", "Dutch", "English","Portuguese"))

train_user <- left_join(train_user, destination_language, by = "country_destination", copy = "destination_language")
train_user$country_primary_language [is.na(train_user$country_primary_language)] <- "others"
train_user$language_combo <- paste(train_user$language_full,"-",train_user$country_primary_language)
#calculate the time difference between date_account_created and date_first_booking
train_user$time_diff <- as.Date(train_user$date_first_booking,"%Y-%m-%d") - as.Date(train_user$date_account_created,"%Y-%m-%d")

write.csv2(train_user, file = "train_user_clean.csv")

##Statistical Analysis
train <- read_csv2("train_user_clean.csv")
glimpse(train)
colSums(is.na(train))

#subset age lower than 100 and larger than 17
train1 <- train %>% filter(age > 18 & age <= 65)
#subset country international locations only
train2 <- train1 %>% filter(country_destination != "US" & country_destination!= "NDF" & country_destination != "other")
#subset gender, removing unknown and other
train2_1 <- train2 %>% filter((gender == "FEMALE" | gender == "MALE") & (gender_age != "FEMALE below 18" & gender_age != "MALE below 18" & gender_age != "FEMALE over 65" & gender_age != "MALE over 65" ))
#subset time different from first active to first booking
train3 <- train %>% filter(time_diff >= 0)
#subset country destination != "NDF"
train4 <- train1 %>% filter(country_destination != "NDF")

summary(train1$age)
ggplot(train1, aes(x = age, fill = country_destination)) + geom_histogram(binwidth = 1, position = "stack") + scale_fill_brewer(palette="Paired")
ggplot(train2,aes(x = age, fill = country_destination)) + geom_histogram(binwidth = 1, position = "stack") + scale_fill_brewer(palette="Paired")
ggplot(train2,aes(x = age, fill = country_destination)) + geom_histogram(binwidth = 1, position = "fill") + scale_fill_brewer(palette="Paired")
ggplot(train2, aes(x = age_cat , fill = country_destination)) + geom_bar(position = "fill") + theme(axis.text.x = element_text(angle = 25)) + scale_fill_brewer(palette="Paired")
ggplot(train2_1, aes(x = gender, fill = country_destination)) + geom_bar() +  scale_fill_brewer(palette="Paired")
ggplot(train2_1, aes(x = gender_age, fill = country_destination)) + geom_bar(position = "fill") + theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Paired")
ggplot(train2_1 , aes(x = gender, y = age)) + geom_jitter(alpha = 0.2, width = 0.35 ) + facet_wrap(~country_destination)
ggplot(train2, aes(x = device_type, fill = country_destination)) + geom_bar(position = "fill") + scale_fill_brewer(palette="Paired")
ggplot(train2, aes(x = browser_type, fill = country_destination)) + geom_bar(position = "fill") + theme(axis.text.x = element_text(angle = 90)) + scale_fill_brewer(palette="Paired")
ggplot(train2, aes(x = signup_app, fill = country_destination)) + geom_bar(position = "fill") + scale_fill_brewer(palette="Paired")
ggplot(train2, aes(x = signup_app, fill = country_destination)) + geom_bar(position = "stack") + scale_fill_brewer(palette="Paired")
ggplot(train3, aes(x = time_diff, fill = country_destination)) + geom_bar(position = "stack") + coord_cartesian(xlim=c(0,30)) + scale_fill_brewer(palette="Paired")

language_combo1 <- train4 %>% 
  group_by(language_combo) %>% 
  tally()
arrange(language_combo1,desc(n))

language1 <- train4 %>% 
  group_by(language_full) %>% 
  tally()
arrange(language1,desc(n))

##machine learning
#I will make predicitons using three machine learning techniques: logistic regression, random forest, and clustering
#First let's explore logistic regression.

#Before make the predicton, we need to have a new dataset that's suitable for the logistic regression model.
#We will remove any observations with missing values and only retain the column that I think will impact the destination country

train_lr <- data.frame(as.numeric(train$age), as.factor(train$gender), as.numeric(train$time_diff) ,as.factor(train$language_full),train$country_destination)
colnames(train_lr) <- c("age", "gender", "time_diff", "language", "country_destination")
train_lr <- subset(train_lr, age > 18 & age <= 65)
train_lr <- na.omit(train_lr)
train_lr$booked <- as.numeric(ifelse(train_lr$country_destination=="NDF",1,0))
train_lr$booked_US <- as.numeric(ifelse(train_lr$country_destination=="US",1,0))

summary(train_lr)
str(train_lr)

#what is the relationship between gender and whether someone booked any reservations.

ctry.out <- glm(booked~age+gender,
                data=train_lr, family="binomial")
coef(summary(ctry.out))

ctry.out.tab <- coef(summary(ctry.out))
ctry.out.tab[, "Estimate"] <- exp(coef(ctry.out))
ctry.out.tab

predDat2 <- with(train_lr,
                 expand.grid(gender= c("-unknown-", "FEMALE", "MALE", "OTHER"),
                             age= mean(age, na.rm = TRUE)))
cbind(predDat2, predict(ctry.out, type = "response",
                        se.fit = TRUE, interval="confidence",
                        newdata = predDat2))

#what is the relationship between gender and whether someone booked reservations in the US.
ctry.out2 <- glm(booked_US~age+gender,
                 data=train_lr, family="binomial")
coef(summary(ctry.out2))

ctry.out.tab2 <- coef(summary(ctry.out2))
ctry.out.tab2[, "Estimate"] <- exp(coef(ctry.out2))
ctry.out.tab2

predDat3 <- with(train_lr,
                 expand.grid(gender= c("-unknown-", "FEMALE", "MALE", "OTHER"),
                             age= mean(age, na.rm = TRUE)))
cbind(predDat3, predict(ctry.out2, type = "response",
                        se.fit = TRUE, interval="confidence",
                        newdata = predDat2))

# This logistic regression tells us that when someone chose an gender, either Male or Female,
# they are 20% more likely to book a reservation regardless of country. 
# If someone selected other or -unknown- gender, they are at least 10% more likely to make a reservation in the US.
# This method has a lot of limitations in terms of prediciting country, we can only have one categorical variable,
# and the dependent variable must be a boolean variable; therefore, I will explore clustering and ramdon forest.

#Clustering
#Before starting the analysis, our dataset need to be structured in a way that can be used.
#I'll create a column for each age_cat, gender, language, country_desination

train5 <- subset(train, age >18 & age<=65 &  gender != "OTHER" & country_destination != 'US'& country_destination != 'NDF')
train_cl <- cbind.data.frame(train5$age_cat,train5$gender,train5$country_destination)
colnames(train_cl) <- c("age_cat", "gender",  "country_destination")
summary(train_cl)
train_cl <- tibble::rowid_to_column(train_cl, "ID")

train_cl <- train_cl %>% 
  mutate(TF = 1) %>% 
  distinct %>% 
  spread(age_cat,TF,fill = 0)

train_cl <- train_cl %>% 
  mutate(TF = 1) %>% 
  distinct %>% 
  spread(gender,TF,fill = 0)

train_cl <- train_cl %>% 
  mutate(TF = 1) %>% 
  distinct %>% 
  spread(country_destination,TF,fill = 0)

View(train_cl)

distances <- dist(train_cl, method = "euclidean")
clusterTrain <- hclust(distances, method="ward")
plot(clusterTrain)
clusterGroups <- cutree(clusterTrain, k = 8) 
tableCluster <- NULL
for (i in 2:19) {
   tableCluster <- rbind(tableCluster,tapply(train_cl[,i],clusterGroups, mean))
}
rnames <- as.vector(colnames(train_cl))
rownames(tableCluster) <- rnames[2:19]
tableCluster

#Decision Trees
#I'll use decision tree for our last machine learnng model

#using CART model
library(caTools)
library(rpart.plot)
set.seed(3000)
train6 <- cbind.data.frame(train$age_cat,train$gender,train$language_full, train$country_destination,train$device_type, train$browser_type)
colnames(train6) <- c("age_cat", "gender","language_full",  "country_destination", "device_type","browser_type" )
train6 <- na.omit(train6)
split <- sample.split(train6$country_destination, SplitRatio = 0.7)
testing <- subset(train6, split == FALSE)
training <- subset(train6, split == TRUE)
trainTree <- rpart(country_destination ~ gender + age_cat + language_full + device_type + browser_type, data=training, method="class", control=rpart.control(minbucket=1000))
prp(trainTree)
PredictCART <- predict(trainTree, newdata=testing, type="class")
table(testing$country_destination,PredictCART)

#Random Forest
library(randomForest)
training$country_destination <- as.factor(training$country_destination)
testing$country_destination <- as.factor(testing$country_destination)
trainForest <- randomForest(country_destination ~ gender + age_cat + language_full + device_type + browser_type, data=training, nodesize=25, ntree=200)
PredictForest <- predict(trainForest, newdata=testing)
table(testing$country_destination, PredictForest)
library(caret)
library(e1071)
fitControl <- trainControl(method="cv", number=10)
cartGrid <- expand.grid(.cp=(1:50)*0.01)
train(country_destination ~ gender + age_cat + language_full + device_type + browser_type, data=training, method="rpart", trControl=fitControl, tuneGrid=cartGrid)
trainTreeCV <- rpart(country_destination ~ gender + age_cat + language_full + device_type + browser_type, method="class", data=training, control=rpart.control(cp=0.18))
predictCV <- predict(trainTreeCV, newdata=testing, type="class")
table(testing$country_destination, predictCV)
