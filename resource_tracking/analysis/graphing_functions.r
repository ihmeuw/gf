#-----------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: General budget/absorption graphing functions
# DATE: Last updated August 2019 
#-----------------------------------------------------

#----------------------
# To do 

# Show projected absorption
#-----------------------

#Function that can provide the absorption landscape of a given country, grant period, and disease.  
# Options: 
# countryName - the country to subset to: options are 'cod', 'gtm', 'sen', or 'uga'. 
# diseaseName - the disease to subset to: options are 'hiv', 'tb', 'malaria', 'hiv/tb', or 'rssh'. 
# grantPeriod - the grant period in the absorption data to subset to. 
# xVar - what should the x-axis be? (Will show as y-axis with coord_flip) 
# facetVar - should you facet wrap by any variables? 
# grantName - subset to one particular grant? Pass the name of the grant desired. 
# yScaleMax - What's the cutoff point for the y-axis labels (absorption as a %?)
# trimAbsorption - should absorption be cut off at 150%? 
# angleText - should y-axis labels be angled at 30 degrees? 
# altTitle, altSubtitle, altCaption - pass strings as alternate options to the "labs" argument in ggplot

budget_exp_bar = function(countryName, diseaseName, grantPeriod=NULL, xVar=c('abbrev_mod_eng'), facetVar=NULL,
                                     grantName=NULL, yScaleMax=160, baseSize=16, barLabels = TRUE, 
                                     trimAbsorption=FALSE, angleText=FALSE,
                                     altTitle=NULL, altSubtitle=NULL, altCaption=NULL){
  require(data.table) 
  require(ggplot2) 
  require(scales)
  options(scipen=100)
  
  #Validation checks
  stopifnot(countryName%in%c('cod', 'gtm', 'sen', 'uga'))
  stopifnot(diseaseName%in%c('hiv', 'tb', 'malaria', 'hiv/tb', 'rssh'))
  if (is.null(grantPeriod)) print("WARNING: Grant period is null. This will pool data from all grant periods. Subset your data first.")
  stopifnot(length(xVar)==1)
  stopifnot(length(facetVar)<=1)

  #Read in data 
  dir = paste0("C:/Users/elineb/Box Sync/Global Fund Files/", toupper(countryName), "/prepped_data/")
  dt = readRDS(paste0(dir, "absorption_", countryName, ".rds"))
  
  #Merge on abbreviated module names. 
  all_interventions = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
  all_interventions = unique(all_interventions[, .(code, abbrev_mod_eng, abbrev_int_eng)])
  dt = merge(dt, all_interventions, by='code')
  stopifnot(nrow(dt[is.na(abbrev_mod_eng) | is.na(abbrev_int_eng)])==0)
  
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
  
  if (xVar=="abbrev_mod_eng") xLabel="Module"
  if (xVar=="abbrev_int_eng") xLabel="Intervention"
  if (xVar=="grant") xLabel="Grant"
  if (xVar=="grant_period") xLabel="Grant Period"
  
  #Set these options so they can be dynamically filled. 
  baseTitle = NULL
  baseSubtitle = NULL
  baseCaption = NULL
  
  #------------------------------------------------------------
  # Collapse data, and set by variables. 
  # -----------------------------------------------------------
  collapseVars = c(xVar, facetVar)
  
  plot_data = dt[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=collapseVars]
  plot_data[, absorption:=round((expenditure/budget)*100, 1)]
  
  #Trim absorption if specified, and flag values greater than yScale limits. 
  if (trimAbsorption) plot_data[absorption>150, absorption:=150]
  if (max(plot_data$absorption, na.rm=T)>yScaleMax) stop(paste0("Increase yScaleMax value - absorption values will be cut off. Max absorption is ", max(plot_data$absorption)))
  
  #Add labels 
  plot_data[, barLabel:=paste0(dollar(expenditure), " (", absorption, ")%")]
  
  # Melt data 
  plot_data = melt(plot_data, id.vars=c(collapseVars, 'absorption', 'barLabel'))
  plot_data[variable=="budget", barLabel:=""]
  plot_data[variable=="budget", variable:="Budget"]
  plot_data[variable=="expenditure", variable:="Expenditure"]
  
  #Base plot
  p = ggplot(plot_data, aes(x=get(xVar), y=value, fill=variable, label=barLabel)) + 
    geom_bar(stat="identity", position="identity") + 
    theme_bw(base_size=baseSize) + 
    coord_flip() + 
    scale_y_continuous(labels = scales::dollar) + 
    labs(x=xLabel, y="", fill="")
  
  if (!is.null(facetVar)) {
    p = p + facet_wrap(~get(facetVar)) 
  } 
  
  #------------------------------------------  
  # Options 
  #------------------------------------------  
  if (barLabels) {
    p = p + geom_text(hjust=0)  
    if (angleText){
      p = p + theme(axis.text.x=element_text(angle=30, vjust=0.5))
    }
  } 
  if (trimAbsorption){
    baseCaption = paste0(baseCaption, "*Absorption capped at 150%\n")
  } 
  if (!is.null(grantName)){
    baseTitle = paste0(baseTitle, "for ", grantName, "\n")
  }
  if (!is.null(limitModules)){
    baseTitle=paste0(baseTitle, " ", limitModules, "\n")
  }
  if (!is.null(poolSemester)){
    baseCaption = paste0(baseCaption, "*Semesters have been pooled across grant period\n")
  }
  
  #Add all modified labels at once
  #Remove the last "\n" from each label 
  if (!is.null(baseTitle)) baseTitle = substr(baseTitle, 1, nchar(baseTitle)-1)
  if (!is.null(baseCaption)) baseCaption = substr(baseCaption, 1, nchar(baseCaption)-1)
  if (!is.null(baseSubtitle)) baseSubtitle = substr(baseSubtitle, 1, nchar(baseSubtitle)-1)
  
  #If specific arguments are used, replace with them. 
  if (!is.null(altTitle)) baseTitle = altTitle 
  if (!is.null(altCaption)) baseCaption = altCaption
  if (!is.null(altSubtitle)) baseSubtitle = altSubtitle
  
  #Add these formatting options. 
  p = p+labs(title=baseTitle, subtitle=baseSubtitle, caption=baseCaption)
  
  return(p) 
}

#Return a graph of the funding landscape for the disease in the country over the time period using Financing Global Health actuals. 
# Options: 
# countryName: Country name. options are 'cod', 'gtm', 'sen', or 'uga'. 
# diseaseName: Disease name. Options are 'hiv', 'tb', or 'malaria'. 
# startYear: What date would you like to start data at? 
# endYear: What date would you like to end data at? 
# 

funding_landscape = function(graphType, countryName, diseaseName, startYear, endYear, includeGHE=FALSE, altCaption=NULL, altTitle=NULL, altSubtitle=NULL){
  require(data.table) 
  require(ggplot2)
  
  #Validation checks
  stopifnot(graphType%in%c('proportion', 'ribbon'))
  stopifnot(countryName%in%c('cod', 'gtm', 'sen', 'uga'))
  stopifnot(diseaseName%in%c('hiv', 'tb', 'malaria'))
  stopifnot(is.numeric(startYear) & is.numeric(endYear))
  
  #Read in data 
  dt = readRDS("J:/Project/Evaluation/GF/resource_tracking/_odah/prepped_data/other_dah_actuals_all.rds")
  
  #Subset down to the options specified
  dt = dt[loc_name==toupper(countryName) & year>=startYear & year<=endYear & disease==diseaseName]
  
  #Collapse data
  collapse = dt[, .(disbursement=sum(disbursement)), by = .(channel_agg, year)] #Just do general function for now, although we probably will want an option to split by module! EL 8/29/19 
  
  #Add on GHE if option is specified 
  if (includeGHE){
    ghe = readRDS("J:/Project/Evaluation/GF/resource_tracking/_ghe/combined_prepped_data/all_ghe.rds")
    ghe = ghe[loc_name==countryName & disease==diseaseName & year>=startYear & year<=endYear, .(disbursement=sum(disbursement, na.rm=TRUE)), by='year']
    ghe[, channel_agg:="Government Health Expenditure"]
    collapse = rbind(collapse, ghe, use.names=T)
  }
  
  #Formatting 
  if (countryName == "cod") countryLabel = "DRC"
  if (countryName == "gtm") countryLabel = "Guatemala"
  if (countryName == "sen") countryLabel = "Senegal"
  if (countryName == "uga") countryLabel = "Uganda"
  
  if (diseaseName == "hiv") diseaseLabel = "HIV"
  if (diseaseName =="tb") diseaseLabel = "tuberculosis"
  if (diseaseName == "malaria") diseaseLabel = "malaria"
  
  if (is.null(altTitle)){
    altTitle = paste0("Funding landscape in ", countryLabel, " for ", diseaseLabel, ", ", startYear, "-", endYear)
  }
  
  #Wrap text for expecially long labels
  collapse[channel_agg == "UN agencies, The World Bank and other regional development banks", 
            channel_agg:= "UN agencies, The World Bank \nand other regional development banks"]
  
  #Order plot so global fund is on the bottom. 
  if (includeGHE) { 
    collapse[, channel_agg:=factor(channel_agg, levels=c("Government Health Expenditure", "Multilateral organizations (GAVI, CEPI)", "NGOs and foundations", "Other bilateral assistance",                                                      
                                                         "U.S. bilateral assistance", "UN agencies, The World Bank \nand other regional development banks",
                                                         "The Global Fund"))]
  
  } else {
      collapse[, channel_agg:=factor(channel_agg, levels=c("Multilateral organizations (GAVI, CEPI)", "NGOs and foundations", "Other bilateral assistance",                                                      
                                                 "U.S. bilateral assistance", "UN agencies, The World Bank \nand other regional development banks",
                                                  "The Global Fund"))]
  } 
  
  #Generate plot 
  if (graphType=='ribbon'){
    funding_landscape = ggplot(data = collapse, aes(x = year, y = disbursement, fill = channel_agg)) + 
      geom_ribbon(aes(ymin = 0, ymax = disbursement), position = "stack") + 
      theme_bw(base_size = 18) + theme(legend.title = element_blank())+
      scale_y_continuous(labels = scales::dollar) +
      scale_fill_brewer(palette = "RdYlBu") +
      labs(x = "Year", y = "Disbursement", title = altTitle, subtitle=altSubtitle, caption=altCaption)
  } else if (graphType=='proportion'){
    funding_landscape = ggplot(data = collapse, aes(x = year, y = disbursement, fill = channel_agg)) + 
      geom_bar(stat="identity", position="fill") + 
      theme_bw(base_size = 18) + theme(legend.title = element_blank())+
      scale_y_continuous(labels=scales::percent) +
      scale_fill_brewer(palette = "RdYlBu") +
      labs(x = "Year", y = "Percentage of annual disbursement", title = altTitle, subtitle=altSubtitle, caption=altCaption)
  }
  
  return(funding_landscape)
  
}

# Helper function to get modules available. 
get_modules = function() { 
  require(data.table) 
  dt = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
  return(unique(dt[, .(disease, module_eng)][order(disease)]))
}

# Helper function to get interventions available. 
get_interventions = function(diseaseName) { 
  require(data.table) 
  dt = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
  dt = dt[disease==diseaseName]
  return(unique(dt[, .(disease, module_eng, intervention_eng)][order(disease, module_eng)]))
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

#----------------------------------------------------
# Formatting and color palettes! 
#----------------------------------------------------
ihme_divergent = c('purple3', 'limegreen', 'plum2', 'springgreen4', 'orchid', 'lavenderblush', 'gray40', 'darkolivegreen1')
ihme_purples = c('mistyrose', 'plum1', 'plum3', 'mediumorchid1', 'mediumorchid3', 'darkorchid2', 'darkorchid4', 'purple4')
ihme_greens = c('palegreen', 'seagreen1', 'olivedrab3', 'palegreen3', 'mediumseagreen', 'olivedrab4', 'seagreen4', 'green4')

#Once color palettes are standardized, set option to review when desired. 
view_color_palettes = function() { 
  require(RColorBrewer)
  require(plotrix) 
  
  par(ask=TRUE)
  for (pal in c("ihme_divergent", "ihme_purples", "ihme_greens")){
    sliceValues = rep(10, length(get(pal)))
    pie3D(sliceValues, explode=0, theta=1.2, col=get(pal), main=paste0(pal, ": ", length(get(pal)), " options"))
  } 
  par(ask=FALSE)
} 
