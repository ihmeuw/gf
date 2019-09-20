# (1) graph showing how data quality in DHIS2 has become more complete over time 
# (e.g., more facilities reporting)
# (2) analysis showing how well data in DHIS2 compares to malaria program data 
# (gold standard proxy) - this could be done by comparing levels reported for one or 
# various key indicators to see the degree of concordance. 

# this indicator in 2018. -- % of confirmed cases that received a 
# first-line antimalarial treatment in public sector facilities).   

# explanation for the high percentage of confirmed cases that are treated, as reported 
# in the PUDR (96%) which uses unadjusted data from DHIS2.  The TERG is also interested 
# in overall DHIS2 data quality.  Here's what we're thinking:
  
  # 1.	First slide with a graph showing how data has become more complete in DHIS2 over time
    # (e.g., more facilities reporting), both nationally and by facility type.
  # 2.	Second slide with the report the total percentage of confirmed cases treated for 2018
    # (sum of confirmed cases treated/sum of confirmed cases) after data cleaning. 
    # Caitlin guesses that it's lower.
  # 3.	On the second slide, if IHME's data is reporting a lower percentage, then can we 
    # identify the source of the outlier(s) by showing both a visual example of the outlier 
    # and a description.
    # a.	From Caitlin: I think that the high percentage might be a result of outliers 
    # in the data (but not sure). Could you investigate this by looking at the 
      # (1) percentage after we do outlier removal (i.e. is it 96%?), 
      # (2) if it is different, were there any major outliers? They would need to be in 
        # the numerator (confirmed cases treated) to make such a high percentage. If so, 
        # Allison is suggesting visualizing the outliers that were removed either by showing
        # a national trend (with a spike where the outlier is), or, if it's not visible, 
        # an example of a facility-specific QR with an obvious errant value.
library(data.table)
library(ggplot2)
library(scales)

# read in data
dhis_before_qr = readRDS('J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outliers/base/base_to_screen.rds')
base_before_qr = readRDS('J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/base/base_services_prepped.rds')
dhis_after_qr = readRDS('J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/base/base_prepped_outliers_replaced.rds')

pnlp_after_qr = readRDS('J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/imputedData_run_0_001_aggVars_lagsLeads_condensed_hz_median.rds')
pnlp_before_qr = readRDS('J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/PNLP_dt_forMI_updated_6_10_19.rds')
# --------------------------
# get 2017 totals
# --------------------------
# PNLP
pnlp_after_qr[, year := year(date)]
dt = pnlp_after_qr[year == 2017,]
dt = dt[grepl(variable, pattern = 'mild', ignore.case = TRUE)]
dt[grepl(variable, pattern = 'treated', ignore.case = TRUE), var := 'conf_cases_treated']
dt[is.na(var), var := 'conf_cases']

dt[, sum(value), by = 'var']

dt2 = pnlp_before_qr[year == 2017,]
sum(dt2$newCasesMalariaMild_5andOlder, na.rm = TRUE) + sum(dt2$newCasesMalariaMild_pregnantWomen, na.rm = TRUE) + sum(dt2$newCasesMalariaMild_under5, na.rm = TRUE)
sum(dt2$mildMalariaTreated_5andOlder, na.rm = TRUE) + sum(dt2$mildMalariaTreated_pregnantWomen, na.rm = TRUE) + sum(dt2$mildMalariaTreated_under5, na.rm = TRUE)

# DHIS2
dhis_after_qr[, year := year(date)]
dhis_before_qr[, year := year(date)]

after = dhis_after_qr[ year == 2017, ]
after = after[grepl(element, pattern = 'simple', ignore.case = TRUE)]
after[grepl(element, pattern = 'trait', ignore.case = TRUE), var := 'conf_cases_treated']
after[is.na(var), var := 'conf_cases']

before = dhis_before_qr[ year == 2017, ]
before = before[grepl(element, pattern = 'simple', ignore.case = TRUE)]
before[grepl(element, pattern = 'trait', ignore.case = TRUE), var := 'conf_cases_treated']
before[is.na(var), var := 'conf_cases']

after[, sum(value), by = 'var']
before[, sum(value), by = 'var']
# --------------------------

outFile = 'J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/base/malaria_deep_dive_figures.pdf'

before = copy(dhis_before_qr)
after = copy(dhis_after_qr)

# fac_natl = fac_by_type[org_unit_type == 'facility', .(num_fac = length(unique(org_unit_id))), by = c('date')]
fac_by_type = after[org_unit_type == 'facility' & level != 'health_area' & date <= '2019-01-01', .(num_fac = length(unique(org_unit_id))), by = c('date', 'level')]
fac_natl = fac_by_type[, .(num_fac = sum(num_fac)), by = c('date')]

g1 = ggplot(fac_natl, aes(x = date, y = num_fac)) + 
  theme_bw() +
  geom_line() +
  geom_point() +
  theme(text = element_text(size = 14)) + 
  labs( title = 'Number of facilities reporting over time', x = 'Date (month - year)', 
        y = 'Number of facilities' ) +
  scale_y_continuous(label = comma)

fac_by_type[ level %in% c('health_post', 'health_center'), facet := 1]
fac_by_type[ level %in% c('general_reference_hospital', 'reference_health_center', 'hospital_center', 'medical_center'), facet := 2]
fac_by_type[is.na(facet), facet := 3]

g2 = ggplot(fac_by_type, aes(x = date, y = num_fac, color = level)) + 
  theme_bw() +
  geom_line() +
  geom_point() +
  theme(text = element_text(size = 14)) + 
  facet_wrap( ~ facet, scales = 'free') +
  theme(strip.text = element_blank(), legend.position = 'bottom') +
  labs( title = 'Number of facilities reporting over time by facility type', x = 'Date (month - year)', 
        y = 'Number of facilities', color = 'Facility Type:' )+
  scale_y_continuous(label = comma)

# check unique ids
nrow(before) == nrow(unique(before[,.(org_unit_id, element, category, date)]))
nrow(after) == nrow(unique(after[,.(org_unit_id, element, category, date)]))

# identify outliers
dt = merge(before, after, by = c('org_unit_id', 'element', 'category', 'date'))
dt[, outlier := value.x != value.y]
dt[outlier == TRUE, .N] # 7,047

# subset to confirmed cases
conf = dt[grepl(element, pattern = 'simple', ignore.case = TRUE)]
conf[outlier == TRUE, .N, by = element_eng.x]
conf[ grepl(pattern = 'trait', element), element_eng := 'Confirmed malaria cases treated']
conf[ is.na(element_eng), element_eng := 'Confirmed malaria cases']
setnames(conf, 'value.x', 'value_with_outliers')
setnames(conf, 'value.y', 'value_outliers_removed')

conf_natl = conf[, .(value_with_outliers = sum(value_with_outliers),
                     value_outliers_removed = sum(value_outliers_removed)), 
                 by = .(date, element_eng)]

conf_natl_melt = melt.data.table(conf_natl, id.vars = c('date', 'element_eng'))
conf_natl_melt[, value_in_mil := value/1000000]
conf_natl_melt[ variable == 'value_with_outliers', variable := 'With outliers']
conf_natl_melt[ variable == 'value_outliers_removed', variable := 'Outliers removed']

g3 = ggplot(conf_natl_melt, aes(x = date, y = value_in_mil, color = element_eng, linetype = variable)) + 
  theme_bw() +
  geom_line() +
  geom_point() +
  theme(text = element_text(size = 14)) + 
  labs( title = 'Confirmed cases treated and confirmed cases, before and after outlier removal', 
        x = 'Date (month - year)', 
        y = 'Number of cases (in millions)', 
        color = '', linetype = '') +
  theme(legend.position = 'bottom')

conf_natl_melt[ element_eng == 'Confirmed malaria cases', element_eng := 'conf_malaria']
conf_natl_melt[ element_eng == 'Confirmed malaria cases treated', element_eng := 'conf_malaria_treated']

conf_prop = dcast.data.table(conf_natl_melt, date + variable ~ element_eng)
conf_prop[, prop := conf_malaria_treated/conf_malaria]
conf_prop[, prop := prop * 100]

g4 = ggplot(conf_prop, aes(x = date, y = prop, color = variable)) + 
  theme_bw() +
  geom_line() +
  geom_point() +
  theme(text = element_text(size = 14)) + 
  labs( title = 'Percentage of confirmed cases treated over time', x = 'Date (month - year)', 
        y = '', color = '') +
  theme(legend.position = 'bottom')

conf_prop_2018 = conf_prop[year(date)== 2018,]
conf_prop_2018 = conf_prop_2018[, .(conf_malaria = sum(conf_malaria),
                                    conf_malaria_treated = sum(conf_malaria_treated)), 
                                by = .(variable)]
conf_prop_2018[ , prop := conf_malaria_treated/conf_malaria]

pdf(outFile, height = 9, width = 12)
print(g1)
print(g2)
print(g3)
print(g4)
dev.off()

