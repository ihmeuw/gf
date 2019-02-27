#########################################333333333
# Case fatality analysis using prepped incidence and mortality data


rm(list=ls())
library(data.table)
library(ggplot2)


j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j') # have to declare the J drive on the cluster 
vr_dir <-  ('/ihme/geospatial/vr_prep/cod/outputs/gtm_collaborators/')
cohort_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
out_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')


wide_deaths <-fread(paste0(out_dir, 'wide_deaths_vr_cohort.csv'))
long_deaths <-fread(paste0(out_dir, 'long_deaths_vr_cohort.csv'))
inc <-fread(paste0(out_dir, 'gtm_tb_incidence.csv'))

inc[,V1:=NULL]
long_counts <- melt(inc, measure.vars=c("gbd_tb_hiv_case", "gbd_tb_case", "cohort_tb_hiv_notif", "cohort_tb_notif", "rev_nr", "rev_r"),
                    variable.name = "source", value.name = "count")
#Cleaning up the variables for plotting
long_counts[,cause:="."]
long_counts[(source=="gbd_tb_hiv_case"|source=="cohort_tb_hiv_notif"|source=="rev_r"),cause:="TB/HIV"]
long_counts[(source=="gbd_tb_case"|source=="cohort_tb_notif"|source=="rev_nr"), cause:="TB"]
long_counts[(source=="gbd_tb_hiv_case"|source=="gbd_tb_case"), data:="GBD 2016"]
long_counts[(source=="cohort_tb_hiv_notif"|source=="cohort_tb_notif"), data:="Cohort"]
long_counts[(source=="rev_r"|source=="rev_nr"), data:="Notifications"]

p1 <- ggplot(long_counts, aes(x = year, y= count, color=data)) + 
<<<<<<< HEAD
  geom_line(aes(linetype=cause, color=data)) +
  ggtitle("Incident TB and TB/HIV cases by source") +
  labs(x = "Year", y = "Cases") +
  theme_minimal()
p1
#Difference between cohort and notifications cases = denominator for CF
=======
  geom_line(size=1, aes(linetype=cause, color=data)) +
  ggtitle("Incident TB and TB/HIV cases by source") +
  labs(x = "Year", y = "Cases") +
  #theme(legend.title = element_text(face="bold", size=40))+
  theme_minimal(base_size = 20)
p1

#Do subtraction on wide dataset
#Difference between cohort and notifications cases = denominator for CF
inc[,diff_tbhiv:=rev_r-cohort_tb_hiv_notif]
inc[,diff_tb:=rev_nr-cohort_tb_notif]
inc[,miss_tbhiv:=gbd_tb_hiv_case-cohort_tb_hiv_notif]
inc[,miss_tb:=gbd_tb_case-cohort_tb_notif]

wide_deaths[,year:=year_id]
dt<-merge(inc, wide_deaths, by=c("year"))
dt[,cf_nottreated_tb_hiv:=tb_hiv_raw_vr/diff_tbhiv]
dt[,cf_missing_tb_hiv:=tb_hiv_raw_vr/miss_tbhiv]
>>>>>>> 000d22b4b6abbd9d72fe31d0711bdf364e7d9f23
#Difference between cohort and VR deaths = numerator for CF