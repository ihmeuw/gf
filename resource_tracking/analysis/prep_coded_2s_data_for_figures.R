# -----------------------------------------------
# Audrey Batzel and Francisco Rios Casas
# 4/8/2021

# Prep the coded 2S data into usable format
# Make figures showing 
  # Changes in support vs. strengthening allocations from approved budgets in NFM2 to approved budgets in NFM3 
    # ^^^ Kath said this is the focus
  # Changes in support vs. strengthening allocations from NFM3 FR to NFM3 approved
  # ^Both by country overall, and by country/module

# OTHER TO DO:
# - verify budget values in the 2S data are up to date? 
# -----------------------------------------------

# -----------------------------------------------
# set up
# -----------------------------------------------
rm(list = ls())
library(data.table)
library(ggplot2)
library(readxl)
library(stringr)

#Box filepaths - these should be used to source raw files, and to save final prepped files. 
user=as.character(Sys.info()[7])
box = paste0("C:/Users/",user,"/Box Sync/Global Fund Files/")

inFile = paste0(box, '2s_data/2S Analysis Template.xlsx')
# -----------------------------------------------

# -----------------------------------------------
# read in data and prep one sheet at a time
# -----------------------------------------------
countries_to_run = c('UGA', 'GTM')

# sheets = c('2020(GA)', ' 2017', ' 2020') # this might not work without more specific if else statement...
#### FRC: I turned this part into an if statement and moved it into the loop below since it looks like sometimes countries share 
#### sheet names but not always

prepped_dt = data.table()

# loop through countries and sheets  to prep

for(country in countries_to_run) {
  
  if (country=='UGA') {
    sheets = c('2017(GA)', '2020(FR)', '2020(GA)')
  } else if (country=='GTM') {
    sheets = c(' 2017', ' 2020', '2020(GA)')
  }
  prepped_dt_country = data.table()
  
  for(sheet in sheets){
    inSheet = paste0(country, sheet)
    
    # read in data
    dt = as.data.table(read_xlsx(inFile, sheet = inSheet))
    
    # prep columns for r usable format
    colnames(dt) = unlist(dt[1,])
    colnames(dt) = tolower(colnames(dt))
    colnames(dt) = gsub(pattern = ' ', replacement = '_', x = colnames(dt))
    dt = dt[-c(1),]
    
    if(inSheet %in% c('UGA2017(GA)', 'UGA2020(FR)', 'GTM 2017', 'GTM 2020')){
      final_coding_col = 'sensitivity_2'
    } else if(inSheet %in% c('UGA2020(GA)', 'GTM2020(GA)')) {
      final_coding_col = 'finaldesignation'
    }
    
    # remove extra row for guatemala (contains summed total of data)--might not apply elsewhere
    if (inSheet%in%c("GTM2020(GA)", "GTM 2020")){
      dt = dt[!(is.na(activity_description))]
    }
    
    # keep just relevant columns and final coding designation
    if (!"country"%in%colnames(dt)) {
      dt[, loc_name := country]
    } else if ("country"%in%colnames(dt)) {
      dt[, country:=NULL]
      dt[, loc_name := country]
      }
    
    dt[, cycle := ifelse(grepl('2017', inSheet), 'NFM2', 'NFM3') ]
    if (country == "UGA"){
    dt[, version := ifelse(grepl('FR', inSheet), 'funding_request', 'approved_budget') ]
    } else {
      if (inSheet=="GTM 2020"){
        dt[, version := 'funding_request']
      }else {
        dt[, version := 'approved_budget']
      }
    }
        
      # }  
      # dt[, version := ifelse(grepl('GA', inSheet), 'approved_budget', 'funding_request') ]}
    
    # not sure if this will apply to other countries but need to make the NFM3 award match other data
    if (sheet == '2020(GA)'){
      # finaldesignation applies to just the newly coded activities, so we will use the previous
      # coding (final_designation), for all activities that were already coded but are in the award budget
      if (country=='UGA') {
        dt = dt[is.na(finaldesignation), finaldesignation := final_designation]
      }else if (country=='GTM'){
        dt = dt[is.na(finaldesignation), finaldesignation := final_designation_fr]
      }
      setnames(dt, 'gf_module', 'module')
      setnames(dt, 'gf_intervention', 'intervention')
      setnames(dt, 'cost_category', 'cost_input')
      # use file name to create grant variable - we want this to be able to compare NFM2 and NFM3
      dt[, grant := lapply(file_name, function(x){paste(unlist(str_split(x, '_'))[c(1,2,3)], collapse = '_')})]
      dt[, grant := unlist(grant)]
    }
    
    dt = dt[, c('loc_name', 'grant', 'cycle', 'grant_period', 'version', 'module', 'intervention', 'activity_description', 'cost_input', 'budget', final_coding_col), with = FALSE]
    setnames(dt, final_coding_col, 'coding_2s')
    dt[, grant := gsub(pattern = '-', '_', grant)]
    
    # save each prepped data sheet to prepped_dt 
    if(nrow(prepped_dt_country) == 0){
      prepped_dt_country = dt
    } else {
      prepped_dt_country = rbindlist(list(prepped_dt_country, dt), use.names = TRUE, fill = TRUE)
    }
  }
  # save country data individually
  saveRDS(prepped_dt_country[loc_name==country], paste0(box, '2s_data/prepped_2s_data_', country, '.rds'))
  write.csv(prepped_dt_country[loc_name==country], paste0(box, '2s_data/prepped_2s_data_', country, '.csv'), row.names = FALSE)
  if(nrow(prepped_dt) == 0){
    prepped_dt = prepped_dt_country
  } else {
    prepped_dt = rbindlist(list(prepped_dt, prepped_dt_country), use.names = TRUE, fill = TRUE)
  }
}
prepped_dt[, budget := as.numeric(budget)]
prepped_dt = prepped_dt[budget != 0, ]

# save full data
saveRDS(prepped_dt, paste0(box, '2s_data/prepped_2s_data_all_countries.rds'))
write.csv(prepped_dt, paste0(box, '2s_data/prepped_2s_data_all_countries.csv'), row.names = FALSE)

# check/compare budget values to tableau to ensure accuracy
check = prepped_dt[loc_name == 'UGA' & cycle == 'NFM3', .(budget = sum(budget)), by=.(module, intervention, cycle, version)]
check = dcast.data.table(check, module + intervention + cycle ~ version, value.var = 'budget')
# -----------------------------------------------

# -----------------------------------------------
#################################################
# -----------------------------------------------

# -----------------------------------------------
# additional data prep for figs
# -----------------------------------------------
# few changes to data for figures...
prepped_dt[, coding_2s := as.factor(coding_2s)]
prepped_dt[, version := as.factor(version)]
prepped_dt[, version := ordered(version, c('approved_budget', 'funding_request'))]
prepped_dt[, cycle := as.factor(cycle)]
prepped_dt[, cycle := ordered(cycle, c('NFM3', 'NFM2'))]

# GTM module names are still in Spanish
prepped_dt[module == 'Sistema de información de gestión de la salud y el seguimiento y la evaluación', plot_module := "HMIS and M&E"]
prepped_dt[module == 'Prestación de servicios integrados y la mejora de la calidad', plot_module := "Int. service del\nand QE"]
prepped_dt[module == 'Sistemas de gestión de la cadena de adquisiciones y suministros', plot_module := "P&SCM \nsyst"]
prepped_dt[module == 'Respuestas y los sistemas comunitarios', plot_module := "CSS"]

prepped_dt[module == 'Health management information systems and M&E', plot_module := 'HMIS and M&E']
prepped_dt[module == 'Health management information system and monitoring and evaluation', plot_module := 'HMIS and M&E']
prepped_dt[module == 'Health products management systems', plot_module := 'Health products\nmngmt syst']
prepped_dt[module == 'Integrated service delivery and quality improvement', plot_module := 'Int. service del\nand QE']
prepped_dt[module == 'Community responses and systems', plot_module := 'CSS']
prepped_dt[module == 'Community systems strengthening', plot_module := 'CSS']
prepped_dt[module == 'Procurement and supply chain management systems', plot_module := 'P&SCM \nsyst']
prepped_dt[module == 'Human resources for health, including community health workers', plot_module := 'HRH, incl. CHWs']
prepped_dt[module == 'Laboratory systems', plot_module := 'Lab\nsystems']
prepped_dt[module == 'Financial management systems', plot_module := 'Fin mngmt\nsyst']
prepped_dt[module == 'National health strategies', plot_module := 'Natl health\nstrats']
prepped_dt[module == 'Health sector governance and planning', plot_module := 'Health Gov \n& planning']
# -----------------------------------------------

# -----------------------------------------------
# set colors to apply to all figures
# -----------------------------------------------
colors = c('#D55E00','#56B4E9') #'#C378A2'
names(colors) = levels(prepped_dt$coding_2s)
# -----------------------------------------------

# -----------------------------------------------
# sum across different variables for figs
# -----------------------------------------------
# Sum to compare NFM2 Award to NFM3 Award overall, by grant, by grant/module, by module
overall_comp = prepped_dt[version == 'approved_budget', .(budget = sum(budget)), by = .(loc_name, cycle, coding_2s)]
graph_grant = prepped_dt[version == 'approved_budget', .(budget = sum(budget)), by = .(loc_name, grant, cycle, coding_2s)]
graph_grant_module = prepped_dt[version == 'approved_budget', .(budget = sum(budget)), by = .(loc_name, grant, cycle, module, plot_module, coding_2s)]
graph_module = prepped_dt[version == 'approved_budget', .(budget = sum(budget)), by = .(loc_name, cycle, module, plot_module, coding_2s)]

# Sum to compare NFM3 FR to NFM3 Award overall, by module, (by intervention?)
overall_comp_nfm3 = prepped_dt[cycle == 'NFM3', .(budget = sum(budget)), by = .(loc_name, version, coding_2s)]
graph_module_nfm3 = prepped_dt[cycle == 'NFM3', .(budget = sum(budget)), by = .(loc_name, version, module, plot_module, coding_2s)]
# -----------------------------------------------

# -----------------------------------------------
# loop through countries and make/save main figures
# -----------------------------------------------
for (country in unique(prepped_dt$loc_name)){
  list_of_plots = NULL
  # figure outFiles:
  outFile_box = paste0(box, '2s_data/figures/2S_figures_', country, '.pdf')
  outFile_j = paste0('J:/Project/Evaluation/GF/resource_tracking/visualizations2021/2S_figures/2S_figures_', country, '.pdf')

  # Overall, NFM3 FR to NRM3 GA (side by side bars)
  # list_of_plots[[x]] = ggplot(overall_comp_nfm3[loc_name == country], aes(x = version, y = budget/1000000, fill = coding_2s)) + 
  #   geom_bar(stat = 'identity', position=position_dodge()) +
  #   labs(fill = '2S Coding', x = 'Budget Version', y = 'Budget (millions USD)', title = '2S funding, comparing NFM3 funding request to NFM3 award') +
  #   geom_text(aes(x = version, y = budget/1000000, label = paste0('US$',round(budget/1000000, 1), 'million'), group = coding_2s), 
  #             hjust = -0.05, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE)  +
  #   scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 15) +
  #   theme(legend.position="bottom")
  
  # Overall, NFM3 FR to NFM3 GA (stacked bars)
  list_of_plots[[1]] = ggplot(overall_comp_nfm3[loc_name == country], aes(x = version, y = budget/1000000, fill = coding_2s)) + 
    geom_bar(stat = 'identity', position='stack') +
    labs(fill = '2S Coding', x = 'Budget Version', y = 'Budget (millions USD)', title = '2S funding, comparing NFM3 funding request to NFM3 award') +
    geom_text(aes(x = version, y = budget/1000000, label = paste0('US$',round(budget/1000000, 1), 'million'), group = coding_2s), 
              position = position_stack(vjust = .5), inherit.aes = TRUE)  +
    scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 15) +
    theme(legend.position="bottom")
  
  # Overall by Module, NFM3 FR to NFM3 GA
  list_of_plots[[2]] = ggplot(graph_module_nfm3[loc_name == country], aes(x = version, y = budget/1000000, fill = coding_2s)) + 
    geom_bar(stat = 'identity', position=position_dodge()) +
    facet_grid(rows = 'plot_module', scales = 'free_y') +
    labs(fill = '2S Coding', x = 'Budget Version', y = 'Budget (millions USD)', title = '2S funding by module, comparing NFM3 funding request to NFM3 award') +
    geom_text(aes(x = version, y = budget/1000000, label = paste0('US$',round(budget/1000000, 1), 'million'), group = coding_2s), 
              hjust = -0.05, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE)  +
    scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 15) +
    theme(legend.position="bottom")
  
  # Overall, NFM2 to NFM3 (Side by side bars)
  # list_of_plots[[x]] = ggplot(overall_comp[loc_name == country], aes(x = cycle, y = budget/1000000, fill = coding_2s)) + 
  #   geom_bar(stat = 'identity', position=position_dodge()) +
  #   labs(fill = '2S Coding', x = 'Grant Cycle', y = 'Budget (millions USD)', title = '2S funding, comparing NFM2 award to NFM3 award') +
  #   geom_text(aes(x = cycle, y = budget/1000000, label = paste0('US$',round(budget/1000000, 1), 'million'), group = coding_2s), 
  #             hjust = -0.05, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE)  +
  #   scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 15) +
  #   theme(legend.position="bottom")
  
  # Overall, NFM2 to NFM3 (Stacked bars)
  list_of_plots[[3]] = ggplot(overall_comp[loc_name == country], aes(x = cycle, y = budget/1000000, fill = coding_2s)) + 
    geom_bar(stat = 'identity', position='stack') +
    labs(fill = '2S Coding', x = 'Grant Cycle', y = 'Budget (millions USD)', title = '2S funding, comparing NFM2 award to NFM3 award') +
    geom_text(aes(x = cycle, y = budget/1000000, label = paste0('US$',round(budget/1000000, 1), 'million'), group = coding_2s), 
              position = position_stack(vjust = .5), inherit.aes = TRUE)  +
    scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 15) +
    theme(legend.position="bottom")
  
  # Overall by module, NFM2 to NFM3
  list_of_plots[[4]] = ggplot(graph_module[loc_name == country], aes(x = cycle, y = budget/1000000, fill = coding_2s)) + 
    geom_bar(stat = 'identity', position='stack') +
    labs(fill = '2S Coding', x = 'Grant Cycle', y = 'Budget (millions USD)', title = '2S funding by module, comparing NFM2 award to NFM3 award across all grants') +
    facet_grid(rows = 'plot_module', scales = 'free_y') +
    #geom_text(aes(x = cycle, y = budget/1000000, label = paste0('US$',round(budget/1000000, 1), 'million'), group = coding_2s), 
    #hjust = -0.05, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE)  +
    scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 15) +
    theme(legend.position="bottom")
  
  # By grant, NFM2 to NFM3
  list_of_plots[[5]] = ggplot(graph_grant[loc_name == country], aes(x = cycle, y = budget/1000000, fill = coding_2s)) + 
    geom_bar(stat = 'identity', position=position_dodge()) +
    labs(fill = '2S Coding', x = 'Grant Cycle', y = 'Budget (millions USD)', title = '2S funding by grant, comparing NFM2 award to NFM3 award') +
    facet_grid(rows = 'grant', scales = 'free_y') +
    geom_text(aes(x = cycle, y = budget/1000000, label = paste0('US$',round(budget/1000000, 1), 'million'), group = coding_2s), 
              hjust = -0.05, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE)  +
    scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 15) +
    theme(legend.position="bottom")

  # save figures
  pdf(outFile_box, height = 12, width = 15)
  for(i in seq(length(list_of_plots))) {
    print(list_of_plots[[i]])
  }
  dev.off()
  
  pdf(outFile_j, height = 12, width = 15)
  for(i in seq(length(list_of_plots))) {
    print(list_of_plots[[i]])
  }
  dev.off()
}
# -----------------------------------------------

# -----------------------------------------------
# loop through countries and make/save figures by grant and module, just NFM2GA vs NFM3GA because 'grant' not in FR
# -----------------------------------------------
for (country in unique(prepped_dt$loc_name)){
  list_of_plots = NULL
  # figure outFiles:
  outFile_box = paste0(box, '2s_data/figures/2S_figures_', country, '_byGrantModule_NFM2GA_NFM3GA.pdf')
  outFile_j = paste0('J:/Project/Evaluation/GF/resource_tracking/visualizations2021/2S_figures/2S_figures_', country, '_byGrantModule_NFM2GA_NFM3GA.pdf')
  
  i = 1
  # By grant and module, NFM2 to NFM3
  for(g in unique(graph_grant_module[loc_name == country, grant])){
    list_of_plots[[i]] = ggplot(graph_grant_module[loc_name == country & grant == g, ], aes(x = cycle, y = budget/1000000, fill = coding_2s)) + 
      geom_bar(stat = 'identity', position='stack') +
      labs(fill = '2S Coding', x = 'Grant Cycle', y = 'Budget (millions USD)', title = paste0('2S funding by module for ', g,', comparing NFM2 award to NFM3 award')) +
      facet_grid(rows = 'plot_module', scales = 'free_y') +
      #geom_text(aes(x = cycle, y = budget/1000000, label = paste0('US$',round(budget/1000000, 1), 'million'), group = coding_2s), 
      #hjust = -0.05, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE)  +
      scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 15) +
      theme(legend.position="bottom")
    i = i+1
  }
  # save figures
  pdf(outFile_box, height = 12, width = 15)
  for(i in seq(length(list_of_plots))) {
    print(list_of_plots[[i]])
  }
  dev.off()
  
  pdf(outFile_j, height = 12, width = 15)
  for(i in seq(length(list_of_plots))) {
    print(list_of_plots[[i]])
  }
  dev.off()
} 
# -----------------------------------------------

# -----------------------------------------------
# loop through countries and make/save figures by intervention (both NFM2GA vs NFM3GA and NFM3FR vs. NFM3GA)
# -----------------------------------------------
graph_intervention_bothCycles = prepped_dt[, .(budget = sum(budget)), by = .(loc_name, version, cycle, module, plot_module, intervention, coding_2s)]

graph_intervention_bothCycles[, plot_intervention := intervention]
graph_intervention_bothCycles[intervention == 'Social mobilization, building community linkages, and coordination', plot_intervention := 'Soc. mob., building community\n linkages, coordination']
graph_intervention_bothCycles[intervention == 'Social mobilization, building community linkages, collaboration and coordination', plot_intervention := 'Soc. mob., building community\n linkages, coordination']
graph_intervention_bothCycles[intervention == 'Community-led advocacy and research', plot_intervention := 'Community-led \nadvocacy']
graph_intervention_bothCycles[intervention == 'Community-led advocacy', plot_intervention := 'Community-led \nadvocacy']
graph_intervention_bothCycles[intervention == 'Institutional capacity building, planning and leadership development', plot_intervention := 'Instl cap. building, planning\nand leadership dvlpt']
graph_intervention_bothCycles[intervention == 'Community-based monitoring', plot_intervention := 'Community-based \nmonitoring']
graph_intervention_bothCycles[intervention == 'Other health information systems and monitoring and evaluation intervention(s)', plot_intervention := 'Other HMIS and\nM&E interventions']

for (country in c('UGA')){ 
  list_of_plots = NULL  
  i = 1
  # figure outFiles:
  outFile_box = paste0(box, '2s_data/figures/2S_figures_', country, '_byIntervention.pdf')
  outFile_j = paste0('J:/Project/Evaluation/GF/resource_tracking/visualizations2021/2S_figures/2S_figures_', country, '_byIntervention.pdf')

  for(mod in c('HMIS and M&E', 'CSS')){
    #NFM2GA to NFM3GA, by intervention
    list_of_plots[[i]] = ggplot(graph_intervention_bothCycles[loc_name == country & plot_module == mod & version == 'approved_budget', ], aes(x = cycle, y = budget/1000000, fill = coding_2s)) + 
      geom_bar(stat = 'identity', position=position_dodge()) +
      labs(fill = '2S Coding', x = 'Grant Cycle', y = 'Budget (millions USD)', title = paste0('2S funding by intervention for ', mod,', comparing NFM2 award to NFM3 award')) +
      facet_grid(rows = 'plot_intervention', scales = 'free_y') +
      geom_text(aes(x = cycle, y = budget/1000000, label = paste0('US$',round(budget/1000000, 2), 'million'), group = coding_2s), 
                hjust = -0.05, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE)  +
      scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 15) +
      theme(legend.position="bottom")
   
    i = i+1
    
    #NFM3FR to NFM3GA, by intervention
    list_of_plots[[i]] = ggplot(graph_intervention_bothCycles[loc_name == country & plot_module == mod & cycle == 'NFM3', ], aes(x = version, y = budget/1000000, fill = coding_2s)) + 
      geom_bar(stat = 'identity', position=position_dodge()) +
      labs(fill = '2S Coding', x = 'Grant Cycle', y = 'Budget (millions USD)', title = paste0('2S funding by intervention for ', mod,', comparing NFM3 FR to NFM3 award')) +
      facet_grid(rows = 'plot_intervention', scales = 'free_y') +
      geom_text(aes(x = version, y = budget/1000000, label = paste0('US$',round(budget/1000000, 2), 'million'), group = coding_2s), 
                hjust = -0.05, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE)  +
      scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 15) +
      theme(legend.position="bottom")
    
    i = i+1
  }
  
  # save figures
  pdf(outFile_box, height = 12, width = 15)
  for(i in seq(length(list_of_plots))) {
    print(list_of_plots[[i]])
  }
  dev.off()
  
  pdf(outFile_j, height = 12, width = 15)
  for(i in seq(length(list_of_plots))) {
    print(list_of_plots[[i]])
  }
  dev.off()
}
# -----------------------------------------------
