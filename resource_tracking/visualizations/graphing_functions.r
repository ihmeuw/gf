#-----------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: General budget/absorption graphing functions
# DATE: Last updated August 2019 
#-----------------------------------------------------

#----------------------
# To do 
#Want to bring in abbreviated module labels, 
# Pass a specific intervention to limit to 
# Split by intervention 
# show stacked budget/expenditure instead of absorption%
# Show projected absorption
#-----------------------

#Function that can provide the absorption landscape of a given country, grant period, and disease.  
# Options: 
# countryName - the country to subset to: options are 'cod', 'gtm', 'sen', or 'uga'. 
# diseaseName - the disease to subset to: options are 'hiv', 'tb', 'malaria', 'hiv/tb', or 'rssh'. 
# grantPeriod - the grant period in the absorption data to subset to. 
# byModule - show one bar for each module? Incompatible with bySemester.
# bySemester - show one bar for each semester? Incompatible with byModule. 
# byGrant - facet wrap by grant? 
# grantName - subset to one particular grant? Pass the name of the grant desired. 
# yScaleMax - What's the cutoff point for the y-axis labels (absorption as a %?)
# barColor - specify a color palette or bar color. 
# trimAbsorption - should absorption be cut off at 150%? 
#limitModules - pass a character vector of the Global Fund modules to limit the graph to. 

absorption_by_loc_disease = function(countryName, diseaseName, grantPeriod, bySemester=FALSE, byModule=FALSE, byGrant=FALSE, 
                                grantName=NULL, yScaleMax=160, barColor="royalblue", barLabels = FALSE, 
                                trimAbsorption=FALSE, limitModules=NULL){
  require(data.table) 
  require(ggplot2) 
  
  #Validation checks
  stopifnot(countryName%in%c('cod', 'gtm', 'sen', 'uga'))
  stopifnot(diseaseName%in%c('hiv', 'tb', 'malaria', 'hiv/tb', 'rssh'))
  if (bySemester==byModule) stop("You must specify a formatting option - either bySemester or byModule, but not both.")
  
  #Read in data 
  dir = paste0("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/", countryName, "/prepped_data/")
  dt = readRDS(paste0(dir, "absorption_", countryName, ".rds"))
  
  for (x in unique(grantPeriod)){
    if (!x%in%dt$grant_period) stop("grantPeriod incorrectly specified.") #Keep debugging this EMILY 
  } 
  
  # Limit data if options are specified. 
  dt = dt[grant_disease==diseaseName] #making editorial decision to limit by grant disease, not intervention-level disease - 
                                  # will want to seek feedback on this decision EL 8/28/19 
  dt = dt[grant_period%in%grantPeriod] #A list may be specified. 
  
  if (!is.null(grantName)){
    dt = dt[grant==grantName]
  }
  if (!is.null(limitModules)){
    dt = dt[gf_module%in%limitModules]
  }
  
  #Formatting 
  if (countryName == "cod") countryLabel = "DRC"
  if (countryName == "gtm") countryLabel = "Guatemala"
  if (countryName == "sen") countryLabel = "Senegal"
  if (countryName == "uga") countryLabel = "Uganda"
  
  if (diseaseName == "hiv") diseaseLabel = "HIV"
  if (diseaseName =="tb") diseaseLabel = "tuberculosis"
  if (diseaseName == "malaria") diseaseLabel = "malaria"
  if (diseaseName == "hiv/tb") diseaseLabel = "HIV/TB"
  if (diseaseName == "rssh") diseaseLabel = "RSSH"
  
  #------------------------------------------------------------
  # BY SEMESTER GRAPH 
  # -----------------------------------------------------------
  if (bySemester) {
    collapse = dt[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant_period', 'semester')]
    collapse[, absorption:=round((expenditure/budget)*100, 2)] #Editorial decision to round here; can be revisited but this seems to be the preference. 
 
    #Trim absorption if specified, and flag values greater than yScale limits. 
    if (trimAbsorption) collapse[absorption>150, absorption:=150]
    if (max(collapse$absorption)>yScaleMax) stop(paste0("Increase yScaleMax value - absorption values will be cut off. Max absorption is ", max(collapse$absorption)))
    
    # Base plot 
    p = ggplot(collapse, aes(x=semester, y=absorption)) + 
      geom_bar(stat="identity", fill=barColor) + 
      theme_bw(base_size=16) + 
      scale_y_continuous(limits=c(0, yScaleMax)) + 
      labs(title=paste0("Absorption for ", diseaseLabel, " in ", countryLabel, " in ", grantPeriod), x="Semester", y="Absorption")
    
    #Options
    if (length(grantPeriod)!=1){
      p = p + facet_wrap(~grant_period)
    }
  } 
  
  #------------------------------------------------------------
  # BY MODULE GRAPH
  # -----------------------------------------------------------
  
  if (byModule) {
    if (length(grantPeriod)>1) stop("byModule option only available for one grant period at once.")
    
    collapse = dt[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant_period', 'semester', 'gf_module')]
    collapse[, absorption:=round((expenditure/budget)*100, 2)] #Editorial decision to round here; can be revisited but this seems to be the preference. 
    
    #Trim absorption if specified, and flag values greater than yScale limits. 
    if (trimAbsorption) collapse[absorption>150, absorption:=150]
    if (max(collapse$absorption, na.rm=T)>yScaleMax) stop(paste0("Increase yScaleMax value - absorption values will be cut off. Max absorption is ", max(collapse$absorption)))
    
    # Base plot 
    p = ggplot(collapse, aes(x=gf_module, y=absorption)) + 
      geom_bar(stat="identity", fill=barColor) + 
      theme_bw(base_size=16) + 
      scale_y_continuous(limits=c(0, yScaleMax)) + 
      facet_wrap(~semester) + 
      coord_flip() + 
      labs(title=paste0("Absorption for ", diseaseLabel, " in ", countryLabel, " in ", grantPeriod), x="Semester", y="Absorption")
    
  } 
  
  # Options 
  if (barLabels) {
    p = p + geom_text(aes(label=paste0(absorption, "%")), vjust=0, size=5, nudge_y=3)  
  } 
  if (trimAbsorption){
    p = p + labs(caption="*Absorption capped at 150%")
  } 
  if (!is.null(grantName)){
    p = p + labs(title=paste0("Absorption for ", diseaseLabel, " in ", countryLabel, " in ", grantPeriod, "\nfor ", grantName))
  }
  if (!is.null(limitModules)){
    p = p + labs(title=paste0("Absorption for ", diseaseLabel, " in ", countryLabel, " in ", grantPeriod, "\nfor ", limitModules))
  }
  
  return(p) 
}

# Helper function to get modules available. 
get_modules = function() { 
  require(data.table) 
  dt = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
  return(unique(dt[, .(disease, module_eng)][order(disease)]))
}

# Helper function to return the PUDR data availability for a given grant and grant period. 
pudr_completeness = function(countryName, grantName, grantPeriod) { 
  require(data.table) 
  stopifnot(countryName%in%c('cod', 'gtm', 'sen', 'uga'))
  stopifnot(length(grantName==1) & length(grantPeriod)==1)
  
  dir = paste0("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/", countryName, "/prepped_data/")
  dt = readRDS(paste0(dir, "absorption_", countryName, ".rds"))
  
  dt = dt[grant==grantName & grant_period==grantPeriod]
  print(paste0("The following PUDR semesters are available for ", grantName, " ", grantPeriod, ":"))
  
  return(unique(dt[, .(semester, start_date=as.Date(start_date), end_date=as.Date(end_date))]))
  
}