#remove withdrawn for risk factors--------------------------------------------------
merged<- readRDS("/rds/general/project/hda_students_data/live/Group1/merged_new_var.rds")
withdrawn=as.character(read.csv("data/w19266_20200204.csv")[,1])
print(withdrawn)
merged_no_withdraw<-merged[!merged$eid %in% withdrawn,]
merged_2<- merged[-which(merged$eid %in% withdrawn), ]

#100 rows removed rather than 135

saveRDS(merged_no_withdraw,"data/merged_no_withdraw.rds")

#outcome of interest remove withdrawn---------------------------------------------
disease_outcome<-readRDS("/rds/general/project/hda_students_data/live/Group1/disease_outcomes.rds")
disease_outcome_withdrawn<-disease_outcomes[!row.names(disease_outcomes) %in% withdrawn,]
saveRDS(disease_outcome_withdrawn,"data/disease_outcome_no_withdrawn.rds")

#101 rows removed

#merge disease
merge_disease<-merge(disease_outcome_no_withdrawn,merged_no_withdraw,by="eid")

#subset data-------------------------------------------------------

saveRDS(merge_disease,"/rds/general/project/hda_students_data/live/Group1/merged_disease.rds")

subset<-dataframe[1:1000,]
saveRDS(subset,"TDS_Group-1/subset_new.rds")
