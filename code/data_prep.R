#path=/rds/general/project/hda_students_data/live/Group1
  
#group 1 codes
#project 3 Mandelian randomisation

#"ukb26390.csv" dataset
#it's the observation dataset, with eid on the first column 

#all other columns are for different are characteristics of the participants
# in the form
#"Field ID"."ID of data collection"."Array ID"

#The field IDs link to explicit descriptions of the variable

#The ID of data collection is an integer: 0 indicates that the information was collected at baseline,
# 1 is at a second time point, and 2 is a third time point. You will see that additional time points are
# available only for some variables, and of these, only a subset participants have data on all time points.

# The array ID is available for variables where multiple answers can be collected.
#The data collected is stored in multiple columns for the same variable.
#For example, for the data on qualifications collected at baseline
# we have 6138.0.0 containing one of the boxes that was ticked by the participant, and 6138.0.1 containing
# data from a second box ticked by the participant, etc. Participants who ticked only one degree will have 
# missing values for the array IDs > 0. 


#You can list the field IDs of variables you would like to look into
# as illustrated in the folder "example_extraction".
# Loading the data
library(data.table)
mydata=data.frame(fread("ukb26390.csv", nrows=5))
myfields=unname(unlist(read.table("extract.txt", header=FALSE)))

## Extracting the column ids 
column_id=grep("eid", colnames(mydata))
found_fieldids=NULL
for (k in 1:length(myfields)){
  mygrep=grep(paste0("X",myfields[k],"."), fixed=TRUE, colnames(mydata))
  if (length(mygrep)>0){
    found_fieldids=c(found_fieldids, myfields[k])
  }
  column_id=c(column_id, mygrep)
}

# Extracting required columns from dataset
extracted=data.frame(fread("/rds/general/project/hda_students_data/live/Group1/ukb26390.csv", select=column_id)) # Path to change!
saveRDS(extracted, "individual_covariates.rds")

extracted<-individual_covariates

df<-data.frame(colnames(extracted)[-1])
colnames(df)<-c("field ID")
colnames(individual_covariates)<-sub("X","",colnames(individual_covariates))
df$name<-c(NA)

colnames(ukb27725)<-sub("-",".",colnames(ukb27725))

merged<-merge(individual_covariates,ukb27725,by="eid",no.dups = FALSE)
saveRDS(merged,"merged_charac_biomarkers.rds")

ukb27725=data.frame(fread("/rds/general/project/hda_students_data/live/Group1/ukb27725.csv"))
colnames(ukb27725)<-sub("-",".",colnames(ukb27725))

merged<-merge(individual_covariates,ukb27725,by="eid",no.dups = FALSE)
saveRDS(merged,"merged_charac_biomarkers.rds")

setdiff(individual_covariates$eid,ukb27725$eid)
#[1] 5810686

individual_covariates[individual_covariates=="" ]<- NA

#qualification
coding_id="100305"
mycoding=read.csv("Codings_Showcase.csv")
print(head(mycoding))
mycoding_field=mycoding[which(mycoding[,1]==coding_id),]
mycoding_field=mycoding_field[,-1]
rownames(mycoding_field)=mycoding_field[,1]
print(mycoding_field)

# As it is in raw data:
print(merged_charac_biomarkers$'6138.0.0')

# Recoded categories:
merged_charac_biomarkers$'6138.0.0'<-as.character(mycoding_field[as.character(merged_charac_biomarkers$'6138.0.0'),"Meaning"])

table(merged_charac_biomarkers$'6138.0.0')








### Last but not least - Mandatory step: checking participant consent

# Over the years, some participants have withdrawn consent and asked that the data about them is 
# not used anymore in research. The eids of participants who have withdrawn have been listed in 
#  "w19266_20200204.csv". Please remember to remove them from your dataset during the data
# preparation stages. 

withdrawn=as.character(read.csv("w19266_20200204.csv")[,1])
print(withdrawn)
