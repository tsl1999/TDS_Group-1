colname_subset<-colnames(subset_cat_nutrition)
data_cat<-data.frame(data_dictionary[,c("Field ID","code ID","name")])
data_cat$Field.ID<-paste("X",data_cat$Field.ID,sep="")
rownames(data_cat)<-data_cat$Field.ID


column_name<-c()
for (i in 1:719){
column_name[i]=paste(data_cat[sub("\\..*", "", colnames(dataframe)[i]),3],
                     sub("^[^.]*.", "", colnames(dataframe)[i]))
}



na_true<-function(x){
  a=sum(is.na(x)==T)
  return(a)
}

table1<-apply(dataframe,MARGIN = 2,na_true)
dataframe=subset_cat
dataframe=dataframe[,sub("^[^ ]*", "", colnames(dataframe))==" 0.0"|colnames(dataframe)=="0.0"]
colnames(subset_cat)<-paste("X",data_cat$Field.ID,sep="")
sub("^[^ ]*", "", colnames(dataframe)[8])
dataframe=dataframe[,gsub('*.([0-9]+)*','\\1',colnames(dataframe))=='00']

saveRDS(dataframe,'Desktop/TDS_Group-1/data/subset00.rds')
