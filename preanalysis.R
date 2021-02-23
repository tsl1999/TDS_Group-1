library(tidyverse)
library(table1)
library(readxl)
library(pheatmap)



setwd("~/Desktop/TDS_Group-1")
dataframe <- readRDS("subset_cat_00.rds")
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





#dataframe <- dataframe[complete.cases(dataframe[IntakesNum]),]
#dataframe <- dataframe[complete.cases(dataframe[IntakesCat]),]

par(mar = c(8, 4.1, 4.1, 2.1))
data <- dataframe[,c(1:6)]
colnames(data) <- gsub(" ", "", colnames(data), fixed = TRUE)

data['BMI']<-dataframe[['BMI']]
data["age_of_recruitement"] <- dataframe[["age of recruitement"]]
data["ethnic_background"] <- dataframe[["ethnic baackground"]]
data["sex"] <- dataframe[["sex"]]
data["qualification"] <- dataframe[["qualification"]]
data["alcohol_intake_frequency"]<- dataframe[["alcohol intake frequency"]]
data["Smoking"] <- dataframe[["smoking status"]]

data["Vegetable_Intake"] <- dataframe[["cooked vegetable in take"]] + dataframe[["salad raw vege intake"]]
data["Fruit_Intake"] <- dataframe[["dried fruit intake"]] + dataframe[["fresh fruit intake"]]
data["Fish_Intake"] <- dataframe[["oily fish intake"]] + dataframe[["non oily fish intake"]]
data["RedMeat_Intake"] <- dataframe[["beef intake"]] + dataframe[["pork intake"]] + dataframe[["lamb mutton intake"]] 
data["ProcessedMeat_Intake"] <- dataframe[["processed meat intake per day"]]
data["WhiteMeat_Intake"]<- dataframe[["poultry intake"]]
data["EDWS"] <- dataframe[["never eat eggs dairy wheat sugar"]]
data["Bread_Intake"] <- dataframe[["bread intake"]]
data["Cheese_Intake"] <- dataframe[["cheese intake"]]
data["Cereal_Intake"] <- dataframe[["cereal intake"]]
data["Tea_Intake"] <- dataframe[["tea intake"]]
data["Coffee_Intake"] <- dataframe[["coffee intake per day"]]
data["Water_Intake"] <- dataframe[["water intake"]]
data["Milk_Type"] <- dataframe[["milk type used"]]
data["Spread_Type"] <- dataframe[["spread type"]]
data["Bread_Type"] <- dataframe[["bread type"]]
data["Cereal_Type"] <- dataframe[["cereal type"]]
data["Coffee_Type"] <- dataframe[["coffee type"]]
data["SaltAdded"] <- dataframe[["salt added to food"]]
data["DietVariation"] <- dataframe[["variation in diet"]]





# Define colums names
Names <- colnames(data[,14:ncol(data)])
data$LungCancer <- as.factor(data$LungCancer)

# Data characteristic plots

#Numeric variables
data.numeric <- na.omit(data[,7:33]) %>%
  as_data_frame() %>%
  select_if(is.numeric) %>%
  gather(key = "variable", value = "value")

ggplot(data.numeric, aes(value)) +
  geom_density() +
  facet_wrap(~variable,scales = "free")
dev.copy(device=pdf,'results/numeric_density.pdf')
dev.off()

#Categorical variables

data.factor<- data[Names] %>%
  select_if(is.factor) 

lapply(colnames(data.factor), function(var){barplot(table(data.factor[var]),main =var,las=2 )
  a=paste0("results/",var)
  dev.copy(device=pdf, paste0(a,'.pdf'))
   dev.off()})



# Univariate Regressions without smoking
pval=lapply(Names,
            function(var) {
              res.logist1 <- glm(LungCancer ~ age_of_recruitement + ethnic_background + sex + qualification + alcohol_intake_frequency+BMI + Smoking, data = (data %>% drop_na(var)), family = 'binomial')
              formula2    <- as.formula(paste("LungCancer ~ age_of_recruitement + ethnic_background + sex + qualification + alcohol_intake_frequency + Smoking +BMI+", var))
              res.logist2 <- glm(formula2, data = data, family = 'binomial')
              pval=c(anova(res.logist1,res.logist2,test="Chisq")$'Pr(>Chi)'[2])
              names(pval)='pval'
              return(pval)})

pval=data.frame((pval))
pval=data.frame(t(pval))
rownames(pval)=Names
a<-plot(-log(pval$pval),xaxt="n",xlab='',pch=16,ylab = '-ln(pvalue)',col=ifelse(pval$pval<=0.05/20,'black','pink'),ylim =c(0,-log(0.05/20)+1))
a+axis(1,labels=rownames(pval),at=c(1:20),las=2)
abline(h=-log(0.05/20))
dev.copy(device=pdf,'results/univariate_manhattenplot.pdf')
dev.off()




# Multivariate Regressions without smoking
formula    <- as.formula(paste("LungCancer ~", paste(Names, collapse=" + ")))
res.logist <- glm(formula, data = data, family = binomial)
print(summary(res.logist))

# Multivariate Regressions with smoking
formula    <- as.formula(paste("LungCancer ~ Smoking +", paste(Names, collapse=" + ")))
res.logist <- glm(formula, data = data, family = binomial)
print(summary(res.logist))

#Create Table 1
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))}
table1(as.formula(paste("~ Smoking + ", paste(colnames(data)[7:33], collapse=" + "),paste("|LungCancer"))), data=data, overall="Total",
       caption = 'Table 1. Patient Participant Demographic and Clinical Characteristics',
       render.continuous=my.render.cont, render.categorical=my.render.cat)


#Correlation Matrix
par(mar = c(5.1, 4.1, 4.1, 2.1))
a<-data[Names] %>%select_if(is.numeric)
a=data.frame(a)
a=drop_na(a)
mycor = cor(a,method = 'spearman')
pheatmap(mycor, cluster_rows = FALSE, cluster_cols = FALSE, border = NA,
         breaks = seq(-1, 1, length.out = 100))
dev.copy(png,'results/correlation_map.png')
dev.off()
