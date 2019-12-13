### data validation analyses for perfor indicators
# create completeness rating for target and result value
dt$completeness_rating <- NA

dt$completeness_rating[which(   is.na(dt$target_value)  &  is.na(dt$any_result_value))] <- 1
dt$completeness_rating[which(   is.na(dt$target_value)  & !is.na(dt$any_result_value))] <- 2
dt$completeness_rating[which(  !is.na(dt$target_value)  &  is.na(dt$any_result_value))] <- 3
dt$completeness_rating[which(  !is.na(dt$target_value)  & !is.na(dt$any_result_value))] <- 4

# create factor variable and assign names
dt$completeness_rating <- factor(dt$completeness_rating)

levels(dt$completeness_rating) <-  c("No data", "Only Result", "Only Target", "Both available")

# calculate if the sources differ between the baseline value and the pr reported value
dt$sources_different <- NA
dt$sources_different[which(dt$baseline_source_code!=dt$pr_result_source_code)] <- 1
dt$sources_different[which(dt$baseline_source_code==dt$pr_result_source_code)] <- 0