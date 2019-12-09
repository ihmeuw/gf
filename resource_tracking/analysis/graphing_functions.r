#-----------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: General budget/absorption graphing functions
# DATE: Last updated August 2019 
#-----------------------------------------------------

#----------------------
# To do 

# Show projected absorption
#-----------------------

# Returns cumulative absorption for the 2018-2020 grant period based on a set of subset conditions. 
#byVars - variables to collapse data by. 
#countrySubset - country to subset to. 
# grantSubset - grant to subset to. 
# diseaseSubset - disease to subset to. 
# moduleSubset - Global Fund module to subset to. 
# currency - what currency would you like to return the data in? Options are USD or EUR. 
# repoRoot - If your repository isn't stored in your documents folder, provide a file path to the 'gf' folder. 

# Function to calculate cumulative absorption data 
get_cumulative_absorption= function(byVars, countrySubset=NULL, grantSubset=NULL, 
                                    diseaseSubset=NULL, moduleSubset=NULL, currency=NULL, repoRoot=NULL, 
                                    dollarFormat=FALSE){
  require(data.table) 
  
  #Validate arguments 
  if (!is.null(countrySubset)) stopifnot(countrySubset%in%c('COD', 'SEN', 'UGA'))
  if (!is.null(currency)) stopifnot(currency%in%c('USD', 'EUR'))
  if (!is.null(diseaseSubset)) stopifnot(diseaseSubset%in%c('hiv', 'tb', 'hiv/tb', 'rssh', 'malaria'))
  
  user = Sys.info()[[7]]
  box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/")
  if(is.null(repoRoot)) repoRoot = paste0("C:/Users/", user, "/Documents/gf/")
  
  #Read in absorption data 
  cod = readRDS(paste0(box, "COD/prepped_data/absorption_cod.rds"))
  sen = readRDS(paste0(box, "SEN/prepped_data/absorption_sen.rds"))
  uga = readRDS(paste0(box, "UGA/prepped_data/absorption_uga.rds"))
  
  all_absorption = rbindlist(list(cod, sen, uga))
  all_absorption[, loc_name:=toupper(loc_name)]
  
  # Validate a few more columns using this data set 
  if (!is.null(grantSubset)) stopifnot(grantSubset%in%unique(all_absorption$grant))
  if (!is.null(moduleSubset)) stopifnot(moduleSubset%in%unique(all_absorption$gf_module))
  
  #Fix disease names 
  all_absorption[, grant_disease:=toupper(grant_disease)]
  all_absorption[grant_disease=="MALARIA", grant_disease:="Malaria"]
  
  #Add abbreviated module names. 
  all_mods = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds") #THIS SHOULD BE CHANGED BACK 
  setnames(all_mods, c('module_eng', 'intervention_eng', 'abbrev_mod_eng', 'abbrev_int_eng'), c('gf_module', 'gf_intervention', 'abbrev_mod', 'abbrev_int'))
  all_mods = unique(all_mods[, .(gf_module, gf_intervention, disease, abbrev_mod, abbrev_int)])
  all_absorption = merge(all_absorption, all_mods, by=c('gf_module', 'gf_intervention', 'disease'), allow.cartesian=TRUE)
  
  #make sure this merge worked correctly. 
  stopifnot(nrow(all_absorption[is.na(abbrev_int)])==0)
  
  #Add in PR type 
  governmental = c('COD-M-MOH', 'GTM-M-MSPAS', 'UGA-M-MoFPED', 'UGA-T-MoFPED', 'COD-H-MOH', 'UGA-H-MoFPED', 'COD-T-MOH', 
                   'GTM-T-MSPAS', 'SEN-Z-MOH', 'SEN-S-MOH', 'UGA-S-MoFPED', 'SEN-M-PNLP', 'GUA-311-G06-H', 'SEN-H-CNLS', 'SNG-T-PNT')
  civil_society = c('COD-M-SANRU', 'UGA-M-TASO', 'UGA-C-TASO', 'COD-H-CORDAID', 'COD-C-CORDAID', 'GTM-H-HIVOS', 'GTM-H-INCAP', 'COD-H-SANRU', 'COD-T-CARITAS', 'COD-M-PSI', 'GUA-311-G05-H', 'SEN-H-ANCS', 'SNG-T-PLAN', 'SEN-M-IntraH', 'UGA-S-TASO') 
  all_absorption[grant%in%governmental, pr_type:="Governmental"]
  all_absorption[grant%in%civil_society, pr_type:="Civil Society"]
  all_absorption[is.na(pr_type), pr_type:="Unknown"]
  stopifnot(nrow(all_absorption[pr_type=="Unknown"])==0)
  
  # Limit dataset if desired. 
  if (!is.null(countrySubset)){
    all_absorption = all_absorption[loc_name%in%countrySubset]
  }
  if (!is.null(grantSubset)){
    all_absorption = all_absorption[grant%in%grantSubset]
  }
  if (!is.null(diseaseSubset)){
    all_absorption = all_absorption[disease%in%diseaseSubset]
  }
  if (!is.null(moduleSubset)){
    all_absorption = all_absorption[gf_module%in%moduleSubset]
  }
  if (!is.null(currency)){
    if (currency=="EUR"){
      source(paste0(repoRoot, "resource_tracking/prep/_common/shared_functions.r"))
      all_absorption[, year:=2018]
      all_absorption = convert_currency(all_absorption, yearVar="year", convertFrom="USD", convertTo="EUR", 
                                            finVars=c('cumulative_budget', 'cumulative_expenditure'))
    }
  }
  
  #Make sure you can do the collapse correctly. 
  if (!any(byVars%in%names(all_absorption))){
    print("Some byVars are not in absorption data.")
    print("Available variables to collapse by are: ")
    print(sort(names(all_absorption)[!names(all_absorption)%in%c('budget', 'expenditure', 'cumulative_budget', 'cumulative_expenditure', 'absorption')]))
    stop()  
  } 
  
  #Flag the different ways to pull cumulative expenditure. 
  # Was it entered in the PUDRs, or do we need to sum old PUDRs? 
  check_exp = all_absorption[grant_period=="2018-2020" & semester=="Semester 3", .(budget = sum(cumulative_budget, na.rm=T), 
                                                                                   expenditure=sum(cumulative_expenditure, na.rm=T)), by='grant']
  check_exp = check_exp[expenditure==0, method:='calculated']
  check_exp = check_exp[expenditure!=0, method:='reported_in_pudr']
  
  check_exp = check_exp[, .(grant, method)]
  all_absorption = merge(all_absorption, check_exp, by='grant')
  
  # Now, run these two methods separately. 
  # First, reported in PUDR. 
  cumulative_absorption1 = all_absorption[grant_period=="2018-2020" & semester=="Semester 3" & method=="reported_in_pudr",
                                         .(cumulative_budget = sum(cumulative_budget, na.rm=T), 
                                             cumulative_expenditure=sum(cumulative_expenditure, na.rm=T)), 
                                         by=c(byVars, 'method')]
  # Then, calculate using previous PUDRs. 
  
  # In cases where cumulative expenditure isn't reported, calculate it from the previous PUDRs. 
  calculate = all_absorption[grant_period=="2018-2020" & method=="calculated"]
  cumulative_absorption2 = data.table()
  if (nrow(calculate)!=0){
    for (g in unique(calculate$grant)){
      subset = calculate[grant==g]
      semesters = unique(all_absorption[grant==g & grant_period=="2018-2020", semester])
      if ('Semester 1-2'%in%semesters){ #These are the only two types of reporting we've seen - a full-year PUDR in 2018 or a S2 2018 PUDR. 
        subset = subset[semester%in%c('Semester 1-2', 'Semester 3'), .(cumulative_budget = sum(budget, na.rm=T), 
                                                                       cumulative_expenditure=sum(expenditure, na.rm=T)), by=c(byVars, 'method')]
      } else if ('Semester 2'%in%semesters){
        subset = subset[semester%in%c('Semester 1', 'Semester 2', 'Semester 3'), .(cumulative_budget = sum(budget, na.rm=T), 
                                                                                   cumulative_expenditure=sum(expenditure, na.rm=T)), by=c(byVars, 'method')]      }
      cumulative_absorption2 = rbind(cumulative_absorption2, subset, fill=T)
    } 
    #Collapse the grant-level out after summing by semester. 
    cumulative_absorption2 = cumulative_absorption2[, .(cumulative_budget = sum(cumulative_budget, na.rm=T), 
                                                        cumulative_expenditure=sum(cumulative_expenditure, na.rm=T)), by=c(byVars, 'method')]
  } 
   
  # ***How can you verify that you've captured the whole time series? 
  
  # Bind the data together 
  if ('cumulative_absorption1'%in%ls() & 'cumulative_absorption2'%in%ls()){
    cumulative_absorption = rbind(cumulative_absorption1, cumulative_absorption2)
  } else if ('cumulative_absorption1'%in%ls()){
    cumulative_absorption = cumulative_absorption1
  } else {
    cumulative_absorption = cumulative_absorption2
  }
  
  # Finally, collapse out the calculation method. 
  cumulative_absorption = cumulative_absorption[, .(budget = sum(cumulative_budget, na.rm=T), 
                                                    expenditure=sum(cumulative_expenditure, na.rm=T)), by=c(byVars)]
  cumulative_absorption[, absorption:=round((expenditure/budget)*100, 1)]
  
  if (dollarFormat){
    cumulative_absorption[, budget:=dollar(budget)]
    cumulative_absorption[, expenditure:=dollar(expenditure)]
  }
    
  # In order to make this function work more nicely with budget_exp_bar graphing function, renaming columns to just 'budget'/'expenditure'
  return(cumulative_absorption) 
} 

#Function that can provide the absorption landscape of a given country, grant period, and disease.  
# Options: 
# dt - the data you'd like to graph.
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

budget_exp_bar = function(dt, xVar=c('abbrev_mod'), facetVar=NULL,
                                     yScaleMax=NULL, baseSize=16, barLabels = TRUE, 
                                     trimAbsorption=FALSE, angleText=FALSE,
                                     altTitle=NULL, altSubtitle=NULL, altCaption=NULL, xLabel=""){
  require(data.table) 
  require(ggplot2) 
  require(scales)
  options(scipen=100)
  
  #Validation checks
  stopifnot(length(xVar)==1)
  stopifnot(length(facetVar)<=1)
  
  #Set these options so they can be dynamically filled. 
  baseTitle = NULL
  baseSubtitle = NULL
  baseCaption = NULL
  
  #------------------------------------------------------------
  # Collapse data, and set by variables. 
  # -----------------------------------------------------------
  collapseVars = c(xVar, facetVar)
  stopifnot(c(collapseVars, 'budget', 'expenditure')%in%names(dt))
  
  plot_data = dt[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=collapseVars]
  plot_data[, absorption:=(expenditure/budget)*100]
  
  # Trim absorption if specified. 
  if (trimAbsorption) plot_data[absorption>150.0, absorption:=150.0]
  plot_data[, absorption:=round(absorption)]
  plot_data[, barLabel:=paste0(dollar(expenditure), " (", absorption, "%)")]
  
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
  
  if (!is.null(yScaleMax)) {
    p = p + scale_y_continuous(labels=scales::dollar, limits=c(0, yScaleMax))
  }
  
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
# graphType - one of either 'proportion' or 'ribbon'. 
# countryName: Country name. options are 'cod', 'gtm', 'sen', or 'uga'. 
# diseaseName: Disease name. Options are 'hiv', 'tb', or 'malaria'. 
# startYear: What date would you like to start data at? 
# endYear: What date would you like to end data at? 
# 

funding_landscape = function(graphType, countryName, diseaseName, startYear, endYear, includeGHE=FALSE, altCaption=NULL, altTitle=NULL, altSubtitle=NULL, 
                             labelBars=FALSE){
  require(data.table) 
  require(ggplot2)
  require(scales)
  
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
    funding_landscape = ggplot(data = collapse, aes(x = year, y = disbursement, fill = channel_agg, label=dollar(disbursement))) + 
      geom_ribbon(aes(ymin = 0, ymax = disbursement), position = "stack") + 
      theme_bw(base_size = 18) + theme(legend.title = element_blank())+
      scale_y_continuous(labels = scales::dollar) +
      scale_fill_brewer(palette = "RdYlBu") +
      labs(x = "Year", y = "Disbursement", title = altTitle, subtitle=altSubtitle, caption=altCaption)
  } else if (graphType=='proportion'){
    funding_landscape = ggplot(data = collapse, aes(x = year, y = disbursement, fill = channel_agg, label=dollar(disbursement))) + 
      geom_bar(stat="identity", position="fill") + 
      theme_bw(base_size = 18) + theme(legend.title = element_blank())+
      scale_y_continuous(labels=scales::percent) +
      scale_fill_brewer(palette = "RdYlBu") +
      labs(x = "Year", y = "Percentage of annual disbursement", title = altTitle, subtitle=altSubtitle, caption=altCaption)
  }
  if (labelBars==TRUE){
    funding_landscape = funding_landscape + 
      geom_text(position=position_fill(vjust = 0.5))
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
