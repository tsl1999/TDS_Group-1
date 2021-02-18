#categorise, withdrawn removed 


#read data----------------------------------------------
path="/rds/general/project/hda_students_data/live/Group1"
setwd(path)

data_dictonary <- read_excel("data_dictonary.xlsx")
data_cat<-data.frame(data_dictionary[,c("Field ID","code ID")])
disease_outcomes <- readRDS("/rds/general/project/hda_students_data/live/Group1/disease_outcomes.rds")

#categorise-----------------------------------------------------
data_cat$Field.ID<-paste("X",data_cat$Field.ID,sep="")
rownames(data_cat)<-data_cat$Field.ID
mycoding=data.frame(read.csv("Codings_Showcase.csv"))


#categorise function--------------------------------------
dataframe<-merge_disease

  for(i in 1: length(colnames(dataframe))){
      if (is.na(data_cat[sub("\\..*", "", colnames(dataframe)[i]),2])==F) 
      {
        coding_id=data_cat[sub("\\..*", "", colnames(dataframe)[i]),2]
        mycoding=read.csv("Codings_Showcase.csv")
        mycoding_field=mycoding[which(mycoding[,1]==coding_id),]
        mycoding_field=mycoding_field[,-1]
        rownames(mycoding_field)=mycoding_field$Value
        dataframe[,i]<-as.character(mycoding_field[as.character(dataframe[,i]),"Meaning"])
      }else{
        dataframe[,i]=dataframe[,i]
      }
      
  }
data_cat<-data.frame(data_dictionary[,c("Field ID","code ID","name")])
data_cat$Field.ID<-paste("X",data_cat$Field.ID,sep="")
rownames(data_cat)<-data_cat$Field.ID


column_name<-c()
for (i in 1:540){
  column_name[i]=paste(data_cat[sub("\\..*", "", colnames(dataframe)[i]),3],
                       sub("^[^.]*.", "", colnames(dataframe)[i]))
}

colnames(dataframe)[7:540]<-column_name[7:540]

saveRDS(dataframe,"data/cate_merge_disease(final).rds")
