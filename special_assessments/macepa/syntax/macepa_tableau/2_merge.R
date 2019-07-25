# merge macepa and dhis2 data
# Francisco Rios Casas

# read in the two datatables
macepa <- readRDS("macepa_data.RDS")
dhis2 <- readRDS("dhis2_data.RDS")

# merge the two together even if some are missing
DT <- merge(macepa, dhis2, by = c("V1", "V2"), all = TRUE)
saveRDS(DT, "merged_all.RDS")

# only merge when there are no missing values
DT2 <- merge(macepa, dhis2, by = c("V1", "V2"), all = FALSE)
saveRDS(DT2, "merged.RDS")
