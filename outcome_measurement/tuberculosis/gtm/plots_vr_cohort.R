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
  geom_line(aes(linetype=cause, color=data)) +
  ggtitle("Incident TB and TB/HIV cases by source") +
  labs(x = "Year", y = "Cases") +
  theme_minimal()
p1
#Difference between cohort and notifications cases = denominator for CF
#Difference between cohort and VR deaths = numerator for CF