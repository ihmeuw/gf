# ------------------------------------------------------------------
# Primary descriptive analysis PDF - August 2014 - December 2018
# Sourced by UVL descriptives

#------------------------------------
# export all summary descriptive figures as a pdf
pdf(paste0(dir, '/outputs/uvl_descriptives.pdf'), height=6, width=9)

# annual data reported for major variables
ggplot(uvl_year, aes(x=date, y=value, color=sex)) + 
  facet_wrap(~variable, scales='free_y') +
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="Year", y="Count", title="Monthly data reported, Uganda Viral Load Dashboard", color="Sex") +
  scale_color_manual(values=sex_colors)

# facilities reporting by month, year
ggplot(table_2, aes(x=date, y=facilities_report, color='red')) + 
  geom_point() +
  geom_line(alpha=0.6) +
  theme_bw() +
  labs(title="Number of facilities reporting", x="Date", y="Facilities reporting", caption="Source: Uganda Viral Load Dashboard") +
  scale_color_manual(values=single_red) +
  theme(legend.position='none')

# determine when scale up of reporting occurred
ggplot(table_2, aes(x=factor(month(date)), y=facilities_report, group=year, color=factor(year))) + 
  geom_point(size=2.5) + 
  geom_line(alpha=0.8) + 
  facet_wrap(~year) +
  theme_bw() +
  xlab("Month") + ylab("Facilities reporting") + 
  labs(title = "Total facilities reporting viral load test results by date (scale up)", 
       caption="Source: Uganda VL Dashboard", colour="Year") +
  scale_color_manual(values=graph_colors)

# facilities reporting and patients submitting samples
ggplot(uvl_year[variable=='Facilities Reporting' | variable=='Patients Submitting Samples'], aes(x=date, y=value, color=sex)) + 
  facet_wrap(~variable, scales='free_y') +
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="Year", y="Count", title="Monthly data reported, Uganda Viral Load Dashboard", color="Sex") +
  scale_color_manual(values=sex_colors)


# scale up in terms of three variables - used for presentation slides
ggplot(uvl_year[variable=='Facilities Reporting' | variable=='Patients Submitting Samples' | variable=="Valid Test Results"], aes(x=date, y=value, color=sex)) +
  facet_wrap(~variable, scales='free_y') +
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="Year", y="Count", title="Monthly data reported, Uganda Viral Load Dashboard", color="Sex") +
  scale_color_manual(values=sex_colors) +
  theme(plot.title=element_text(size=18),
        plot.subtitle=element_text(vjust=-4, size=18),
        plot.caption=element_text(vjust=6, size=14),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14),
        strip.text.x=element_text(size=14))


# percentage of missing sex data
ggplot(sex_tot, aes(x=date, y=percent)) + 
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="% of patients whose sex is unknown", y="Date", title="Reporting completeness: percentage of patients whose sex is unknown") 

# ---------------
# suppression ratio maps

# suppression ratio for all years 
ggplot(coordinates, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() + 
  labs(title="Percent virally suppressed by district, Uganda", subtitle=" August 2014 - February 2018",
       caption="Source: Uganda Viral Load Dashboard", fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# annual suppression ratios
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() +
  labs(title="Annual percent virally suppressed by district, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# annual patients received
n <- ratio_year[ ,sum(patients_received, na.rm=T)]

ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=patients_received)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors, trans='log', breaks=breaks, name="Patients received") + 
  theme_void() +
  labs(title="Number of patients submitting samples, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=paste('n=', n)) +
  theme(plot.title=element_text(vjust=-2), plot.subtitle=element_text(vjust=-2), plot.caption=element_text(vjust=6))   +
  coord_fixed() 

# ---------------
# suppression ratio by sex

# suppression ratios among females
ggplot(coordinates_female, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ladies) + 
  theme_void() +
  labs(title="Viral suppression ratios among females by district, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# suppression ratios among males
ggplot(coordinates_male, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=gents) + 
  theme_void() +
  labs(title="Viral suppression ratios among males by district, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


sex_ratio = uvl[ ,.(patients=sum(patients_received), ratio=100*sum(suppressed)/sum(valid_results)), by=.(date, sex)]
sex_ratio = melt(sex_ratio, id.vars=c('date', 'sex'))

sex_ratio$variable = factor(sex_ratio$variable, c('patients', 'ratio'), c('Patients submitting samples', 'Percent virally suppressed (%)'))

# patients submitting samples by sex
ggplot(sex_ratio[sex!='Unknown' & variable=='Patients submitting samples'], aes(x=date, y=value, color=sex, group=sex)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  labs(title='Patients submitting samples for VL testing by sex', x='Date', y='Count', color='Sex') +
  theme(plot.title=element_text(size=18))

# suppression ratio by sex
ggplot(sex_ratio[sex!='Unknown' & variable=='Percent virally suppressed (%)'], aes(x=date, y=value, color=sex, group=sex)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  labs(title='Percent virally suppressed by sex', x='Date', y='%', color='Sex') +
  theme(plot.title=element_text(size=18))


# stacked bar showing suppressed/not suppressed of valid test results by year 
ggplot(table_1, aes(x=date, y=valid_results, fill='Not Suppressed')) + 
  geom_bar(stat="identity") + 
  geom_bar(aes(y=suppressed, fill='Suppressed'), stat='identity') + 
  scale_fill_manual(name='', values=bar_colors) + theme_minimal() +
  labs(title = "Virally suppressed patients", x='Date', y="Total valid test results", caption="Source: Uganda VL Dashboard")


# ---------------
# annual variable comparisons

ggplot(uvl_1[year==2014], aes(y=value, x=factor(month(date)), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2014 Uganda Viral Load Dashboard data by sex", color="Sex") + theme_bw()

ggplot(uvl_1[year==2015], aes(y=value, x=factor(month(date)), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2015 Uganda Viral Load Dashboard data by sex", color="Sex") + theme_bw()

ggplot(uvl_1[year==2016], aes(y=value, x=factor(month(date)), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2016 Uganda Viral Load Dashboard data by sex", color="Sex") + theme_bw()

ggplot(uvl_1[year==2017], aes(y=value, x=factor(month(date)), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2017 Uganda Viral Load Dashboard data by sex", color="Sex") + theme_bw()

ggplot(uvl_1[year==2018], aes(y=value, x=factor(month(date)), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2017 Uganda Viral Load Dashboard data by sex", color="Sex") + theme_bw()

# ---------------

# patients received by month, year
ggplot(table_1, aes(x=factor(month), y=patients_received, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + 
  facet_wrap(~year) +
  theme_bw() +
  xlab("Month") + ylab("Patients submitting samples") + 
  labs(title = "Patients submitting samples by month, year", caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# samples received by month, year
ggplot(table_1, aes(x=factor(month(date)), y=samples_received, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + 
  facet_wrap(~year) +
  theme_bw() +
  xlab("Month") + ylab("Samples received") + 
  labs(title = "Samples received by month, year", caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# total DBS samples received by month, year
ggplot(table_1, aes(x=factor(month(date)), y=dbs_samples, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + theme_bw() +
  facet_wrap(~year) +
  xlab("Month") + ylab("DBS samples received") + 
  labs(title = "DBS samples received by month, year", caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# DBS ratio by month, year
ggplot(table_1, aes(x=factor(month(date)), y=dbs_ratio, col=factor(sex), group=sex)) + 
  geom_point(size=1.5, alpha=0.6) + geom_line(alpha=0.6) + theme_bw() + ylim(0,100) +
  xlab("Month") + ylab("Percent of total samples (%)") + 
  facet_wrap(~year) +
  labs(title = "Percentage of total samples that are DBS samples by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# total Plasma samples received by month, year
ggplot(table_1, aes(x=factor(month(date)), y=plasma_samples, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + theme_bw() +
  facet_wrap(~year) +
  xlab("Month") + ylab("Plasma samples received") + 
  labs(title = "Plasma samples received by month, year", caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# Plasma ratio by month, year
ggplot(table_1, aes(x=factor(month(date)), y=plasma_ratio, col=factor(sex), group=sex)) + 
  geom_point(size=1.5, alpha=0.6) + geom_line(alpha=0.6) + theme_bw() + ylim(0,100) +
  xlab("Month") + ylab("Percent of total samples (%)") + 
  facet_wrap(~year) +
  labs(title = "Percentage of total samples that are plasma samples by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# valid viral load test results by month, year
ggplot(table_1, aes(x=factor(month(date)), y=valid_results, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + theme_bw() +
  facet_wrap(~year) +
  xlab("Month") + ylab("Valid test results") + 
  labs(title = "Viral load test results by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# valid results as a percent of samples received
ggplot(table_1, aes(x=factor(month(date)), y=valid_samples_ratio, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + theme_bw() + 
  facet_wrap(~year) +
  xlab("Month") + ylab("Percent of total samples (%)") + 
  labs(title = "Valid test results as a percentage of total samples by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# total suppressed persons by month, year
ggplot(table_1, aes(x=factor(month(date)), y=suppressed, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + theme_bw() +
  facet_wrap(~year) +
  xlab("Month") + ylab("Virally suppressed patients") + 
  labs(title = "Total virally suppressed patients by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# suppression ratio by month, year
ggplot(table_1, aes(x=factor(month(date)), y=suppression_ratio, col=factor(sex), group=sex)) + 
  geom_point(size=1.5, alpha=0.5) + geom_line(alpha=0.7) + theme_bw() + ylim(0,100) +
  facet_wrap(~year) +
  xlab("Month") + ylab("Percent suppressed of valid test results (%)") + 
  labs(title = "Percent of patients that are virally suppressed by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# zoom in on suppression ratio by month, year (reduce size of y axis)
ggplot(table_1, aes(x=factor(month(date)), y=suppression_ratio, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + theme_bw() + 
  facet_wrap(~year) +
  xlab("Month") + ylab("Percent suppressed of valid test results (%)") + 
  labs(title = "Percent of patients that are virally suppressed by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# ----------------------------------------------
# MAPS

# -------------------
# facilities reporting results

# annual count of facilities reporting results
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=total_facilities)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors) + 
  theme_void() +
  labs(title="Number of facilities reporting on viral suppression, Uganda", subtitle=" n=2,040",
       caption="Source: Uganda Viral Load Dashboard", 
       fill="Facilities reporting") +
  theme(plot.title=element_text(vjust=-1.5), plot.subtitle=element_text(vjust=-1.5),
        plot.caption=element_text(vjust=6)) 

# annual percentage of facilities reporting results
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=facility_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() +
  labs(title="Percentage of all facilities that have ever reported viral load results, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% of facilities reporting") +
  theme(plot.title=element_text(vjust=-0.5), plot.caption=element_text(vjust=6)) 


# -------------------
# log-transformed counts

# annual samples received
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=samples_received)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors, trans="log", breaks=breaks, name="Samples received") + 
  theme_void() +
  labs(title="Number of samples received, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=" n=1,980,551") +
  theme(plot.title=element_text(vjust=-2), plot.subtitle=element_text(vjust=-2), plot.caption=element_text(vjust=6)) 

# dbs samples received
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=dbs_samples)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors, trans="log", breaks=breaks, name="DBS samples received") + 
  theme_void() +
  labs(title="Number of DBS samples received, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=" n=1,345,501") +
  theme(plot.title=element_text(vjust=-2), plot.subtitle=element_text(vjust=-2), plot.caption=element_text(vjust=6)) 


# annual ratio of all samples that are dbs samples
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=dbs_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() +
  labs(title="Percentage of samples received that are DBS samples, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% DBS") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# annual number of valid test results
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=valid_results)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors, trans="log", breaks=breaks, name="Test results") + 
  theme_void() +
  labs(title="Number of valid viral load test results, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle="  n=1,896,731", fill="Valid test results") +
  theme(plot.title=element_text(vjust=-2), plot.subtitle=element_text(vjust=-2), plot.caption=element_text(vjust=6))

# annual number of virally suppressed persons
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=as.numeric(suppressed))) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=sup_colors, trans="log", breaks=breaks, name="Suppressed") + 
  theme_void() +
  labs(title="Number of virally suppressed persons, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="Suppressed") +
  theme(plot.title=element_text(vjust=-2), plot.subtitle=element_text(vjust=-2), plot.caption=element_text(vjust=6)) 


dev.off()

# -----------------



# -----------------

# scale up PDF - use to determine when scale up occurred

# export as a pdf
pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/scale_up.pdf', height=6, width=9)

# facilities reporting

facilities_1 <- uvl[ ,.(facilities_report=length(unique(facility_id))), by=.(date, year) ]
uvl[ , .(length(unique(facility_id)))]
facilities_1[ , percent_report:=((facilities_report/2039)*100)]
facilities_1[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

# facilities reporting by month, year
ggplot(facilities_1, aes(x=date, y=facilities_report)) + 
  geom_point() +
  geom_line(alpha=0.6) +
  theme_bw() +
  labs(title="Number of facilities reporting", x="Date", y="Facilities reporting", caption="Source: Uganda Viral Load Dashboard")

# facilities reporting by month, year
ggplot(facilities_1, aes(x=date, y=percent_report)) + 
  geom_point() +
  geom_line(alpha=0.6) +
  theme_bw() +
  labs(title="Percentage of facilities reporting", x="Date", y="Percentage of facilities reporting", caption="Source: Uganda Viral Load Dashboard")

# years on separate graphs
ggplot(table_2, aes(x=factor(month), y=facilities_report, group=year, color=factor(year))) + 
  geom_point(size=2.5) + 
  geom_line(alpha=0.8) + 
  facet_wrap(~year) +
  theme_bw() +
  xlab("Month") + ylab("Facilities reporting") + 
  labs(title = "Total facilities reporting viral load test results by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Year") +
  scale_color_manual(values=graph_colors)


# detail of all data by month, year
ggplot(uvl_year, aes(x=date, y=value, color=sex)) + 
  facet_wrap(~variable, scales='free_y') +
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="Year", y="Count", title="Monthly data reported, Uganda Viral Load Dashboard", color="Sex") +
  scale_color_manual(values=sex_colors)


# facilities reporting by month, year
ggplot(table_2, aes(x=factor(month), y=facilities_report, col=factor(year), group=year)) + 
  geom_point(size=2.5) + geom_line(alpha=0.8) + theme_bw() +
  xlab("Month") + ylab("Facilities reporting") + 
  labs(title = "Total facilities reporting viral load test results by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Year") +
  scale_color_manual(values=graph_colors)


dev.off()


#-----------------------------

#--------------------
# TERG SLIDES

terg1 <- uvl[, .(patients_received=sum(patients_received)),
             by=.(sex, date)]

# create a table of the total number of facilities reporting in each district, all years
total_fac_year <- uvl[, .(total_facilities=as.numeric(length(unique(facility_id)))), by=.(date)]
terg <- merge(terg1, total_fac_year, by="date", all.x=TRUE)

# reshape long
terg <- melt(terg, id.vars=c("sex","date"))

# keep single values for facilities (females only)
terg <- terg[!(variable=="total_facilities" & (sex=="Male"| sex=="Unknown")) ]
terg <- terg[variable=="total_facilities", sex:="Facility"]

# label the variables for graph titles and put the graphs in an intuitive order
terg$variable <- factor(terg$variable, 
                        levels=c("patients_received", "total_facilities"), 
                        labels=c("Patients submitting samples for VL testing", "Facilities reporting VL tests performed"))

terg$sex <- factor(terg$sex, levels=c("Female", "Male", "Facility"),
                   labels=c("Females", "Males", "Facilities"))

# melt terg1 as an alternative
terg1 <- melt(terg1, id.vars=c("sex","date"))
# label the variables for graph titles and put the graphs in an intuitive order
terg1$variable <- factor(terg1$variable, 
                         levels=c("patients_received"), 
                         labels=c("Patients submitting samples for VL testing"))

terg1$sex <- factor(terg1$sex, levels=c("Female", "Male"),
                    labels=c("Females", "Males"))
total_fac_year[, sex:='Purple']

#--------------------
# subset coordinates_year to 2017 and 2018 only

coordinates_year <- coordinates_year[year==2017 | year==2018]

#--------------------

# export a PDF og the graphs needed for the TERG meeting

pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/uvl_terg.pdf', height=6, width=9)

#graph of facilities reporting and patients submitting samples for Vl testing
#annual data reported for major variables
ggplot(terg, aes(x=date, y=value, color=sex)) + 
  facet_wrap(~variable, scales='free_y') +
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="Date", y="Count", color="Sex") +
  scale_color_manual(values=sex_colors)

# alternative graphs
ggplot(terg1, aes(x=date, y=value, color=sex)) + 
  facet_wrap(~variable, scales='free_y') +
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="Date", y="Count", color="Sex") +
  scale_color_manual(values=sex_colors)

col <- c('#3f007d')

ggplot(total_fac_year, aes(x=date, y=total_facilities, color=sex)) + 
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(title="Total facilities reporting viral toad tests performed by month", x="Date", y="Count", color="Sex") + 
  scale_color_manual(values=col)


# annual suppression ratios
ggplot(coordinates_year[year==2018], aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() +
  labs(title="Viral suppression ratios by district, Uganda", fill="Percent virally suppressed") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


gents <- brewer.pal(6, 'Blues')

# breaks for log transformation legends
breaks <- c(1, 20, 400, 8100)


breaks <- c(1, 20000, 30000, 50000)

pdf(paste0(dir, 'mapss.pdf'), height=6, width=19)

# annual patients received
ggplot(coordinates_year[year==2018], aes(x=long, y=lat, group=group, fill=patients_received)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=gents, name="Patients", breaks=breaks ) + 
  theme_void() +
  labs(title="Patients submitting samples for viral load testing", caption="Source: Uganda Viral Load Dashboard") +
  theme(plot.title=element_text(vjust=-4, size=22), 
        plot.caption=element_text(vjust=6, size=14),
        legend.title = element_text(size=16), 
        legend.text = element_text(size=14)) 



dev.off()

#-----------------------

# info for describing the graph

x <- ratio_year[year==2017, .(suppression_ratio, patients_received), by=.(district, id)]
x <- x[order(suppression_ratio)]

#-----
# alternative graphs

level_ratio <- uvl[!is.na(level),.(suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                   by=.(date, level)]

# melt terg1 as an alternative
level1 <- melt(level_ratio, id.vars=c("date","level"))

# alternative graphs
ggplot(level1, aes(x=date, y=value, color=factor(level), group=level)) + 
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() 

labs(x="Date", y="Count", color="Sex") +
  scale_color_manual(values=sex_colors)


# label the variables for graph titles and put the graphs in an intuitive order
level1$variable <- factor(terg1$variable, 
                          levels=c("patients_received"), 
                          labels=c("Patients submitting samples for VL testing"))

terg1$sex <- factor(terg1$sex, levels=c("Female", "Male"),
                    labels=c("Females", "Males"))
total_fac_year[, sex:='Purple']

# slide for the board of directors meeting
pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/board_vl.pdf', height=6, width=9)

# suppression ratio for all years 
ggplot(coordinates_year[year==2018], aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() + 
  labs(title="Viral suppression ratios by district, Uganda", subtitle="January - April 2018",
       caption="Source: Uganda Viral Load Dashboard", fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-4, size=24), plot.subtitle=element_text(vjust=-4, size=16), 
        plot.caption=element_text(vjust=6, size=15), legend.title=element_text(size=16, hjust=0.5)) + coord_fixed()

dev.off()

graph <- uvl[ ,.(ratio=100*(sum(suppressed)/sum(valid_results))), by=.(date, sex, month, year)]

y_tho <-c('#756bb1', '#66c2a4', '#bd0026' )

pdf(paste0(dir, '/ratio.pdf'), height=6, width=9)

ggplot(graph, aes(x=date, y=ratio, color=sex)) +
  geom_point() +
  geom_line() +
  labs(x="Date", y="Percent virally supperessed (%)", color="Sex",
       caption='Source: Uganda Viral Load Dashboard') +
  scale_color_manual(values=y_tho) +
  theme_minimal() +
  theme(legend.title = element_text(size=16), 
        plot.caption=element_text(vjust=6, size=14),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=14)) 

dev.off()

#-----------------------------------





