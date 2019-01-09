# Audrey Batzel
# Code I used in developing more finalized versions of code
# Keeping it here just in case I need to refer back to it


# ----------------------------------------------  
# ----------------------------------------------  
# # all interventions
makeGraph2 <- function(hz){
  g2 <- ggplot(dt[health_zone==hz], aes(date, value, color = intervention_spec, ymin=0))
  
  g2 + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y", labeller = as_labeller(intervention_names))
}

# "ArtLum", "SP", "ASAQ", "ITN"
makeGraphTreatments <- function(hz){
  gTreatments <- ggplot(data= subset(dt[health_zone==hz & (intervention==treatments)]), aes(date, value, color = intervention_spec, ymin=0))
  
  gTreatments + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y", labeller = as_labeller(intervention_names))
}

# "ANC", "RDT", "smearTest", "VAR"
makeGraphTests <- function(hz){
  gTests <- ggplot(data= subset(dt[health_zone==hz & (intervention==tests)]), aes(date, value, color = intervention_spec, ymin=0))
  
  gTests + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y", labeller = as_labeller(intervention_names))
}

# "healthFacilities", "reports"
makeGraphCompleteness <- function(hz){
  gCompleteness <- ggplot(data= subset(dt[health_zone==hz & (intervention==completenessMeasures)]), aes(date, value, color = intervention_spec, ymin=0))
  
  gCompleteness + geom_point() + geom_line() + theme_bw() + ggtitle(hz) + facet_wrap("intervention", scales="free_y", labeller = as_labeller(intervention_names))
}
# ----------------------------------------------
# ----------------------------------------------         
# antenatal care visits by health zone
makeGraphANC <- function(hz){
  gANC <- ggplot(data= subset(dt[health_zone==hz & (intervention==("ANC_1st")| intervention==("ANC_2nd") | intervention==("ANC_3rd") | intervention==("ANC_4th")) ]), aes(date, value, color= intervention, ymin=0))
  
  gANC + geom_point() + geom_line() + theme_bw() + ggtitle(hz)
}

# ----------------------------------------------
# ----------------------------------------------