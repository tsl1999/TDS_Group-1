#merge
library(data.table)

path="/rds/general/project/hda_students_data/live/Group1"
setwd(path)
ukb27725=data.frame(fread("/rds/general/project/hda_students_data/live/Group1/ukb27725.csv"))
individual_cov<-readRDS("/rds/general/project/hda_students_data/live/Group1/individual_covariates.rds")

merged<-merge(individual_cov,ukb27725,by="eid",no.dups = FALSE)

isTRUE(merged$X40001.0.0.x==merged$X40001.0.0.y)
