#categorise, remove withdrawn and subset


#read data----------------------------------------------
path="/rds/general/project/hda_students_data/live/Group1"
setwd(path)
merged<- readRDS("/rds/general/project/hda_students_data/live/Group1/merged.rds")
data_dictonary <- read_excel("data_dictonary.xlsx")
data_cat<-data.frame(data_dictonary[,c("Field ID","code ID")])
disease_outcomes <- readRDS("/rds/general/project/hda_students_data/live/Group1/disease_outcomes.rds")

#categorise-----------------------------------------------------
data_cat$Field.ID<-paste("X",data_cat$Field.ID,sep="")
rownames(data_cat)<-data_cat$Field.ID
mycoding=data.frame(read.csv("Codings_Showcase.csv"))


#categorise function--------------------------------------
  
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

dataframe<-merged_no_withdraw


#test---------------------------------------------------------------------
i=130

table(merged[,i])
