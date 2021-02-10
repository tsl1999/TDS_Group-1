#categorise, remove withdrawn and subset

#read data----------------------------------------------
path="/rds/general/project/hda_students_data/live/Group1"
setwd(path)
merged<- readRDS("/rds/general/project/hda_students_data/live/Group1/merged.rds")
data_dictonary <- read_excel("data_dictonary.xlsx")
data_cat<-data.frame(data_dictonary[,c("Field ID","code ID")])
disease_outcomes <- readRDS("/rds/general/project/hda_students_data/live/Group1/disease_outcomes.rds")

#remove withdrawn for risk factors--------------------------------------------------

withdrawn=as.character(read.csv("data/w19266_20200204.csv")[,1])
print(withdrawn)
merged_no_withdraw<-merged[!row.names(merged) %in% withdrawn,]
merged_2<- merged[-which(rownames(merged) %in% withdrawn), ]

#100 rows removed rather than 135

saveRDS(merged_no_withdraw,"data/merged_no_withdraw.rds")

#outcome of interest remove withdrawn---------------------------------------------
disease_outcome_withdrawn<-disease_outcomes[!row.names(disease_outcomes) %in% withdrawn,]
saveRDS(disease_outcome_withdrawn,"data/disease_outcome_no_withdrawn.rds")

#101 rows removed

#subset data-------------------------------------------------------
subset<-merged_no_withdraw[1:1000,]
saveRDS(subset,"TDS_Group-1/subset.rds")




#categorise-----------------------------------------------------
data_cat$Field.ID<-paste("X",data_cat$Field.ID,sep="")
rownames(data_cat)<-data_cat$Field.ID
mycoding=data.frame(read.csv("Codings_Showcase.csv"))


#categorise function--------------------------------------
catdata<-function(dataframe){
  
  for(i in 1: length(colnames(dataframe)))
  {
    if (colnames(dataframe)[i]=="eid"){colnames(dataframe)[i]<-"eid"
    }
    else {
      if (is.na(data_cat[sub("\\..*", "", colnames(dataframe)[i]),2])==F) 
      {
        coding_id=data_cat[sub("\\..*", "", colnames(dataframe)[i]),2]
        mycoding_field=mycoding[which(mycoding[,1]==coding_id),]
        mycoding_field=mycoding_field[,-1]
        rownames(mycoding_field)=mycoding_field$Value
        dataframe[,i]<-as.character(mycoding_field[as.character(dataframe[,i]),"Meaning"])
      }else{
        dataframe[,i]=dataframe[,i]
      }
      
      
    }
  }
}

catdata(merged_no_withdraw[1:60])
catdata(merged_no_withdraw[91:260])
catdata(merged_no_withdraw[360:565])
catdata(merged_no_withdraw[584:781])
catdata(merged_no_withdraw[796:881])
#test---------------------------------------------------------------------
i=130

table(merged[,i])
