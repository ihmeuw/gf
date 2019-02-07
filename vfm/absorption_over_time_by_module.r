# make a figure of how absorption changes over time by module
# to show that grants shouldn't reprogram based on low absorption only

library(data.table)
library(ggplot2)
library(stringr)


data = fread("C:/local/GF_copy/resource_tracking/multi_country/mapping/prepped_gos_data.csv")
outFile = 'C:/local/GF_copy/vfm/visualizations/absorption_over_time_by_module.pdf'

# grant end
data[, grant_end:=as.numeric(str_sub(grant_period,-4,-1))]

# grant month
data[, start_date:=as.Date(start_date)]
data[, end_date:=as.Date(end_date)]
data[, date_midpoint:=((end_date-start_date)/2)+start_date]
data[, months_remaining:=as.numeric((as.Date(paste0(grant_end, '-12-30'))-date_midpoint)/30)]
data[, grant_month:=max(months_remaining)-months_remaining, by='grant_number']

data[, absorption:=expenditure/budget]
data[absorption>10 | absorption<0, absorption:=NA]
data = data[!is.na(absorption) & is.finite(absorption)]

# quick-removal of observations that are distracting to display (come back and improve)
data = data[!(code=='R8_3' & absorption>6)]
data = data[!(code=='R3' & country=='Uganda' & absorption>7.5)]
data = data[!(code=='R8' & country=='Uganda' & months_remaining>60 & absorption>3)]
data = data[!(code=='R2' & country=='Uganda' & months_remaining>50 & absorption>3)]
data = data[!(code=='R4' & country=='Uganda' & months_remaining>20 & absorption>2.5)]

pdf(outFile, height=5.5, width=9) 
ggplot(data[grepl('M',code)], aes(y=absorption*100, x=months_remaining)) + 
	geom_hline(aes(yintercept=100), color='red', linetype='dashed') + 
	geom_point() + 
	geom_smooth(se=FALSE) + 
	facet_wrap(~abbrev_module, scales='free_y') + 
	scale_x_reverse() +
	scale_y_continuous(limits = c(0, NA)) + 
	labs(title='Malaria Absorption Over Grant Phase', y='Absorption (%)', 
		x='Months Remaining in Grant (scale reversed)') + 
	theme_bw()

ggplot(data[grepl('R',code) & country=='Uganda'], aes(y=absorption*100, x=months_remaining)) + 
	geom_hline(aes(yintercept=100), color='red', linetype='dashed') + 
	geom_point() + 
	geom_smooth(se=FALSE) + 
	facet_wrap(~abbrev_module, scales='free_y') + 
	scale_x_reverse() +
	scale_y_continuous(limits = c(0, NA)) + 
	labs(title='RSSH Absorption Over Grant Phase', y='Absorption (%)', 
		x='Months Remaining in Grant (scale reversed)', subtitle='Uganda') + 
	theme_bw()

ggplot(data[grepl('R',code) & country=='Guatemala' & months_remaining<50], aes(y=absorption*100, x=months_remaining)) + 
	geom_hline(aes(yintercept=100), color='red', linetype='dashed') + 
	geom_point() + 
	geom_smooth(se=FALSE) + 
	facet_wrap(~abbrev_module, scales='free_y') + 
	scale_x_reverse() +
	scale_y_continuous(limits = c(0, NA)) + 
	labs(title='RSSH Absorption Over Grant Phase', y='Absorption (%)', 
		x='Months Remaining in Grant (scale reversed)', subtitle='Guatemala') + 
	theme_bw()

ggplot(data, aes(y=absorption*100, x=months_remaining)) + 
	geom_hline(aes(yintercept=100), color='red', linetype='dashed') + 
	geom_point() + 
	geom_smooth(se=FALSE) + 
	facet_wrap(~disease, scales='free') + 
	scale_x_reverse() +
	scale_y_continuous(limits = c(0, NA)) + 
	labs(title='Absorption Over Grant Phase', y='Absorption (%)', 
		x='Months Remaining in Grant (scale reversed)') + 
	theme_bw()
dev.off()
