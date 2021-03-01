library(tidyverse)
library(readxl)
library(missForest)

setwd("/rds/general/project/hda_students_data/live/Group1")
dataframe <- readRDS("/rds/general/project/hda_students_data/live/Group1/data/merged_only00.rds")
colnames(dataframe) <- trimws(colnames(dataframe))


IntakesNum <- c("cooked vegetable in take","salad raw vege intake", "dried fruit intake", "fresh fruit intake", "bread intake",
                "cereal intake","tea intake","coffee intake per day","water intake")
IntakesCat <- c("beef intake","pork intake","lamb mutton intake","processed meat intake per day","cheese intake",
                "poultry intake","oily fish intake","non oily fish intake")
TypesCat <- c("never eat eggs dairy wheat sugar","spread type","milk type used","bread type","cereal type","coffee type",
              "salt added to food","variation in diet","smoking status")


for (var in IntakesNum){
  dataframe[var] <- as.numeric(ifelse(dataframe[var] == 'Less than one',"0.5",
                                      ifelse(dataframe[var] == 'Do not know',NA,
                                             ifelse(dataframe[var] == 'Prefer not to answer',NA,dataframe[[var]]))))}

for (var in IntakesCat){
  dataframe[var] <- as.numeric(ifelse(dataframe[var] == 'Never', "0",
                                      ifelse(dataframe[var] == 'Less than once a week',"0.5",
                                             ifelse(dataframe[var] == 'Once a week',"1",
                                                    ifelse(dataframe[var] == '2-4 times a week',"3",
                                                           ifelse(dataframe[var] == '5-6 times a week',"5.5",
                                                                  ifelse(dataframe[var] == 'Once or more daily',"7", NA)))))))}
for (var in TypesCat){
  dataframe[var] <- as.factor(dataframe[[var]])}





dataframe.imp <- missForest(xmis = dataframe[c(IntakesCat,IntakesNum)],xtrue = dataframe[c(IntakesCat,IntakesNum)], ntree = 100, variablewise = T)

for (var in c(IntakesCat,IntakesNum)){
  dataframe[[var]] <- ifelse(is.na(dataframe[[var]]),
                             dataframe.imp$ximp[[var]],
                             dataframe[[var]])}
#dataframe <- dataframe[complete.cases(dataframe[IntakesNum]),]
#dataframe <- dataframe[complete.cases(dataframe[IntakesCat]),]


data <- dataframe[,c(1:6)]
colnames(data) <- gsub(" ", "", colnames(data), fixed = TRUE)


data["age_of_recruitement"] <- dataframe[["age of recruitement"]]
data["ethnic_background"] <- dataframe[["ethnic baackground"]]
data["sex"] <- dataframe[["sex"]]
data["qualification"] <- dataframe[["qualification"]]
data["alcohol_intake_frequency"]<- dataframe[["alcohol intake frequency"]]
data["Smoking"] <- dataframe[["smoking status"]]

data["Vegetable_Intake"] <- dataframe[["cooked vegetable in take"]] + dataframe[["salad raw vege intake"]]
data["Fruit_Intake"] <- dataframe[["dried fruit intake"]] + dataframe[["fresh fruit intake"]]
data["OilyFish_Intake"] <- dataframe[["oily fish intake"]]
data["NonOilyFish_Intake"] <- dataframe[["non oily fish intake"]]
data["RedMeat_Intake"] <- dataframe[["beef intake"]] + dataframe[["pork intake"]] + dataframe[["lamb mutton intake"]] 
data["ProcessedMeat_Intake"] <- dataframe[["processed meat intake per day"]]
data["WhiteMeat_Intake"] <- dataframe[["poultry intake"]]
data["Bread_Intake"] <- dataframe[["bread intake"]]
data["Cheese_Intake"] <- dataframe[["cheese intake"]]
data["Cereal_Intake"] <- dataframe[["cereal intake"]]
data["Tea_Intake"] <- dataframe[["tea intake"]]
data["Coffee_Intake"] <- dataframe[["coffee intake per day"]]
data["Water_Intake"] <- dataframe[["water intake"]]
data["EDWS"] <- dataframe[["never eat eggs dairy wheat sugar"]]
data["Milk_Type"] <- dataframe[["milk type used"]]
data["Spread_Type"] <- dataframe[["spread type"]]
data["Bread_Type"] <- dataframe[["bread type"]]
data["Cereal_Type"] <- dataframe[["cereal type"]]
data["Coffee_Type"] <- dataframe[["coffee type"]]
data["SaltAdded"] <- dataframe[["salt added to food"]]
data["DietVariation"] <- dataframe[["variation in diet"]]

Names <- colnames(data[,13:ncol(data)])


colnames(data)

for (var in Names[1:13]){data[paste("Score",var)] <- ecdf(data[[var]])(data[[var]])}

data["Score SaltAdded" ] <- as.numeric(ifelse(data["SaltAdded"] == 'Never/rarely', "0.2",
ifelse(data["SaltAdded"] == 'Sometimes',"0.6",
       ifelse(data["SaltAdded"] == 'Usually',"0.8",
              ifelse(data["SaltAdded"] == 'Always',"1",
                     ifelse(data["SaltAdded"] == 'Prefer not to answer',"0.5",NA))))))
  
 
data["NutritionScore"]
data["NutritionScore"] <- (data[["Score Vegetable_Intake" ]]*10 + data[["Score Fruit_Intake" ]]*10 + (1-data[["Score Cereal_Intake"]])*5 
+ data[["Score Cheese_Intake"]]*5 + (1-data[["Score RedMeat_Intake"]])*5 + data[["Score WhiteMeat_Intake"]]*5 + data[["Score NonOilyFish_Intake"]]*5
+ (1-data[["Score OilyFish_Intake"]])*5 + (1-data[["Score Coffee_Intake"]])*5 + (1-data[["Score Tea_Intake"]])*5 
+ (1-data[["Score Bread_Intake"]])*10 + (1-data[["Score ProcessedMeat_Intake"]])*10 + (1-data[["Score SaltAdded" ]])*10)

hist(data$NutritionScore)
dev.copy(device = png,"results/nutrition_hist.png")
dev.off()
summary(data$NutritionScore)


model <- glm(LungCancer ~ age_of_recruitement + ethnic_background + sex + qualification + alcohol_intake_frequency + Smoking + NutritionScore, data = data , family = 'binomial')
summary(model)
