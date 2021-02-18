#column name standardisation
#keep 00

dataframe=`cate_merge_disease(final)`
dataframe=dataframe[,gsub('*.([0-9]+)*','\\1',colnames(dataframe))=='00']
dataframe=cbind(`cate_merge_disease(final)`[,1:6],dataframe)

column_name<-c()
for (i in 1:99){
  column_name[i]=gsub("[^a-zA-Z]", " ", colnames(dataframe)[i])
}

colnames(dataframe)=column_name

#some error column resorting
fresh_fruit_intake<-merged_no_withdraw$X1309.0.0
fresh_fruit_intake[fresh_fruit_intake==-10]<-"Less than one"
fresh_fruit_intake[fresh_fruit_intake==-3]<-"Prefer not to answer"
fresh_fruit_intake[fresh_fruit_intake==-1]<-"Do not know"
dataframe$`fresh fruit intake    `<-fresh_fruit_intake

coffee_intake<-merged_no_withdraw$X1498.0.0
coffee_intake[coffee_intake==-10]<-"Less than one"
coffee_intake[coffee_intake==-3]<-"Prefer not to answer"
coffee_intake[coffee_intake==-1]<-"Do not know"
dataframe$`coffee intake per day     `<-coffee_intake

water_intake<-merged_no_withdraw$X1528.0.0
water_intake[water_intake==-10]<-"Less than one"
water_intake[water_intake==-3]<-"Prefer not to answer"
water_intake[water_intake==-1]<-"Do not know"
dataframe$`water intake    `<-water_intake

number_household<-merged_no_withdraw$X709.0.0
number_household[number_household==-3]<-"Prefer not to answer"
number_household[number_household==-1]<-"Do not know"
dataframe$`Number in household    `<-number_household

cook_vege<-merged_no_withdraw$X1289.0.0
cook_vege[cook_vege==-10]<-"Less than one"
cook_vege[cook_vege==-3]<-"Prefer not to answer"
cook_vege[cook_vege==-1]<-"Do not know"
dataframe$`cooked vegetable in take    `<-cook_vege

salad<-merged_no_withdraw$X1299.0.0
salad[salad==-10]<-"Less than one"
salad[salad==-3]<-"Prefer not to answer"
salad[salad==-1]<-"Do not know"
dataframe$`salad raw vege intake    `<-salad

dried_fru<-merged_no_withdraw$X1319.0.0
dried_fru[dried_fru==-10]<-"Less than one"
dried_fru[dried_fru==-3]<-"Prefer not to answer"
dried_fru[dried_fru==-1]<-"Do not know"
dataframe$`dried fruit intake    `<-dried_fru

age_meat<-merged_no_withdraw$X3680.0.0
age_meat[age_meat==-3]<-"Prefer not to answer"
age_meat[age_meat==-1]<-"Do not know"
dataframe$`age when last ate meat    `<-age_meat

bread<-merged_no_withdraw$X1438.0.0
bread[bread==-10]<-"Less than one"
bread[bread==-3]<-"Prefer not to answer"
bread[bread==-1]<-"Do not know"
dataframe$`bread intake    `<-bread

cereal<-merged_no_withdraw$X1458.0.0
cereal[cereal==-10]<-"Less than one"
cereal[cereal==-3]<-"Prefer not to answer"
cereal[cereal==-1]<-"Do not know"
dataframe$`cereal intake    `<-cereal


tea<-merged_no_withdraw$X1488.0.0
tea[tea==-10]<-"Less than one"
tea[tea==-3]<-"Prefer not to answer"
tea[tea==-1]<-"Do not know"
dataframe$`tea intake    `<-tea

saveRDS(dataframe,"data/merged_only00.rds")
