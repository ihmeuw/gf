test_anc1 <- before_MI[, .(dps, health_zone, date, ANC_1st)]

test_anc1$year <- year(test_anc1$date)
test_anc1$month <- month(test_anc1$date)
test_anc1 <- melt.data.table(test_anc1, id.vars=c("dps", "health_zone", "date", "year", "month"))
test_anc1 <- test_anc1[, .(dps, health_zone, variable, year, value, month)]
test_anc1 <- dcast.data.table(test_anc1, dps + health_zone + variable + year ~ month)
names(test_anc1) = c("dps", "health_zone", "variable", "year", "jan", "feb", "mar", "apr","may", "jun", "jul", "aug", "sept", "oct", "nov", "dec")
all_na_but_dec <- test_anc1[is.na(jan) & is.na(feb) & is.na(mar) & is.na(apr) & is.na(may) & is.na(jun)
                            & is.na(jul) & is.na(aug) & is.na(sept) & is.na(oct) & is.na(nov) & !is.na(dec), .(dps, health_zone, year), by="variable"]


test_all_vars <- melt.data.table(before_MI, id.vars=c("X", "province", "dps", "health_zone", "date", "id"))

test_all_vars$year <- year(test_all_vars$date)
test_all_vars$month <- month(test_all_vars$date)
test_all_vars <- test_all_vars[, .(dps, health_zone, variable, year, value, month)]
test_all_vars <- dcast.data.table(test_all_vars, dps + health_zone + variable + year ~ month)
names(test_all_vars) = c("dps", "health_zone", "variable", "year", "jan", "feb", "mar", "apr","may", "jun", "jul", "aug", "sept", "oct", "nov", "dec")
all_na_but_dec <- test_all_vars[is.na(jan) & is.na(feb) & is.na(mar) & is.na(apr) & is.na(may) & is.na(jun)
                            & is.na(jul) & is.na(aug) & is.na(sept) & is.na(oct) & is.na(nov) & !is.na(dec) & dec != 0,]
