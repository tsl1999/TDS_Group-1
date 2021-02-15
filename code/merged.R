#merge
library(data.table)

path="/rds/general/project/hda_students_data/live/Group1"
setwd(path)
biomarkers<-readRDS("/rds/general/project/hda_students_data/live/Group1/data/biomarkers.rds")
individual_cov<-readRDS("/rds/general/project/hda_students_data/live/Group1/data/individual_covariates_new_var.rds")

merged<-merge(individual_cov,biomarkers,by="eid",no.dups = FALSE)

saveRDS(merged,"merged_new_var.rds")

