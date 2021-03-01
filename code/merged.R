#merge
library(data.table)

path="/rds/general/project/hda_students_data/live/Group1"
setwd(path)
biomarkers<-readRDS("/rds/general/project/hda_students_data/live/Group1/data/biomarkers.rds")
individual_cov<-readRDS("/rds/general/project/hda_students_data/live/Group1/data/individual_covariates_new_var.rds")

merged<-merge(individual_cov,biomarkers,by="eid",no.dups = FALSE)

saveRDS(merged,"merged_new_var.rds")

merged<-merge(new_var_no_withdraw,merged_new,by="eid",no.dups = FALSE)
saveRDS(merged,"data/merged_new_var_acc.rds")


merged_new_var<-merge(merged_new_var_acc,merged_only00,by="eid",no.dups = FALSE)
saveRDS(merged_new_var,"data/merged_new_00.rds")

new_df <- merged_new_var %>%
  select(eid, `Lung Cancer`,`Colon Cancer`,`Stomach Cancer`,`Breast Cancer`,`Other Cancer`,accidents, everything())
saveRDS(new_df,"data/merged_new_00.rds")
