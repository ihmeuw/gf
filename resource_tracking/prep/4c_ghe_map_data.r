# ----------------------------------------------
##### Load the mapping files  #####
# ----------------------------------------------

sicoin_data = strip_chars(cleaned_database, unwanted_array, remove_chars)

mapping_list = load_mapping_list("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx",
                                 include_rssh_by_disease = FALSE)

## before we get it ready for mapping, copy over so we have the correct punctuation for final mapping: 
final_mapping = copy(mapping_list)
final_mapping$disease = NULL
setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))
mapping_list$coefficient = 1
mapping_list$abbrev_intervention = NULL
mapping_list$abbrev_module= NULL

sicoin_init_mapping = merge(sicoin_data, gf_mapping_list, by=c("module", "intervention", "disease"), all.x=TRUE,allow.cartesian = TRUE)

##use this to check if any modules/interventions were dropped:
# dropped_sicoin= sicoin_init_mapping[is.na(sicoin_init_mapping$code)]

mapped_sicoin = merge(sicoin_init_mapping, final_mapping, by="code")
mapped_sicoin$budget = mapped_sicoin$budget*mapped_sicoin$coefficient
mapped_sicoin$expenditure = mapped_sicoin$expenditure*mapped_sicoin$coefficient
mapped_sicoin$disbursement = mapped_sicoin$disbursement*mapped_sicoin$coefficient
mapped_sicoin$country = "Guatemala"
mapped_sicoin$data_source = "sicoin"
mapped_sicoin$lang = "esp"
mapped_sicoin$year = year(mapped_sicoin$start_date)
mapped_sicoin$loc_name = "gtm"

mapped_sicoin$financing_source = ifelse(mapped_sicoin$financing_source == "donacions", "other_dah", as.character(mapped_sicoin$financing_source))
# data_check1 = sicoin_data[, sum(budget, na.rm = TRUE),by = c( "module","intervention","disease")]
# data_check2 =mapped_sicoin[, sum(budget, na.rm = TRUE),by = c("module", "intervention","disease")]

write.csv(mapped_sicoin, "J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
          ,row.names=FALSE, fileEncoding="latin1")