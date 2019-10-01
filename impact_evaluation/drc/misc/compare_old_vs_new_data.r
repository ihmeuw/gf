# compare old and new data

# packages
rm(list=ls())
library(data.table)

# files
oldFile = "J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/archive/inputs_outputs_2019_06_24_20_05_53.rds"
newFile = "J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/inputs_outputs.rds"

# read data
old = readRDS(oldFile)
new = readRDS(newFile)

# compare
dim(old)
dim(new)
sapply(seq(ncol(old)), function(x) { 
	if (class(old[[x]])=='numeric') { 
		return(c(names(old)[x], mean(new[[x]] - old[[x]], na.rm=T), sum(new[[x]] - old[[x]], na.rm=T)))
	}
})

