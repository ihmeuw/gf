dt = data.table(read_excel(paste0(dir, "_gf_files_gos/master_file_list.xlsx")))
nrow(dt)
dt[, start_date_programmatic:=as.Date(as.numeric(start_date_programmatic), origin="1899-12-30")]
dt[, end_date_programmatic:=as.Date(as.numeric(end_date_programmatic), origin="1899-12-30")]

metadata = fread(paste0(dir, "_gf_files_gos/metadata/grant_agreement_implementation_periods_dataset_201963.csv"))
correct_periods = metadata[GeographicAreaCode_ISO3%in%c('COD', 'GTM', 'SEN', 'UGA'), .(GrantAgreementNumber, ImplementationPeriodStartDate, ImplementationPeriodEndDate)]
names(correct_periods) = c('grant', 'grant_period_start', 'grant_period_end')

#Format dates correctly 
correct_periods[, grant_period_start:=tstrsplit(grant_period_start, " ", keep=1)][, grant_period_start:=as.Date(grant_period_start, format="%m/%d/%Y")]
correct_periods[, grant_period_end:=tstrsplit(grant_period_end, " ", keep=1)][, grant_period_end:=as.Date(grant_period_end, format="%m/%d/%Y")]

#Extract grant period 
correct_periods[, grant_period:=paste0(year(grant_period_start), "-", year(grant_period_end))]
correct_periods[, correct_grant_period:=grant_period]

correct_periods[, ip_start_month:=month(grant_period_start)]
correct_periods[, ip_end_month:=month(grant_period_end)]
correct_periods[, ip_start_year:=year(grant_period_start)]
correct_periods[, ip_end_year:=year(grant_period_end)]

#Merge files together and compare
dt1 = merge(dt, correct_periods, all.x=T, by=c('grant', 'grant_period'))
nrow(dt1)

#Using these new correct months, figure out what the correct PUDR semesters are. 
pudr_semesters = correct_periods[, .(grant, grant_period, grant_period_start, grant_period_end)] #Only will care about the PUDRs from 2015 on. 
melt = melt(pudr_semesters, id.vars=c('grant', 'grant_period'), value.var='date')
melt = melt[order(grant, grant_period, variable)]
melt[, concat:=paste0(grant, grant_period)]

pudr_sequence = c('1-A', '1-B', '2-A', '2-B', '3-A', '3-B', '4-A', '4-B', '5-A', '5-B', '6-A', '6-B', '7-A', '7-B', '8-A', '8-B', '9-A', '9-B', '10-A', '10-B')

correct_pudr_sem = data.table()
for (g in unique(melt$concat)){
  dt = melt[concat==g]
  stopifnot(nrow(dt)==2)
  start = as.Date(dt[variable=="grant_period_start", value])
  end = as.Date(dt[variable=="grant_period_end", value])
  frame = data.table(grant=dt$grant, grant_period=dt$grant_period, 
                     value=seq(start, end, by='6 months'))
  frame[, pudr_semester:=pudr_sequence[1:nrow(frame)]]
  if (nrow(frame)==2){
    frame$pudr_semester[2] = "1-A"
  }
  
  correct_pudr_sem = rbind(correct_pudr_sem, frame)
}

pudrs = dt1[data_source%in%c('pudr') & !is.na(start_date_programmatic), .(grant, grant_period, file_name, start_date_programmatic, end_date_programmatic, pudr_semester_programmatic)]
setnames(pudrs, 'pudr_semester_programmatic', 'hand_coded_semester')
setnames(correct_pudr_sem, 'value', 'start_date_programmatic')

pudrs = merge(pudrs, correct_pudr_sem, by=c('grant', 'grant_period', 'start_date_programmatic'), all.x=T)

#Check. 
pudrs[, hand_code_start:=substr(hand_coded_semester, 1, 3)] # If you have a semester like "1-AB", that's ok, just check that it matches 1-A. AKA the start dates are correct. 
error = pudrs[hand_code_start!=pudr_semester]
if (nrow(error)!=0){
  print(error) 
  stop("Some PUDR semesters were entered incorrectly. Review variable 'pudr_semester_programmatic'.")
}