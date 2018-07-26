# ----------------------------------------------
# Irena Chen
#
# 1/11/2017
# ### Run these functions for the resource_tracking_analysis_graph.R script:  

# ---------------------------------------------
##function to create indicator between current, upcoming, and rejected budgets: 
data_sources_facet <- function(year, data_source){
  if(year < 2018){
    x <- "Past/Active"
  } else if (data_source=="rej_fpm"){
    x <- "In Iteration"
  } else if (grepl("init_fpm", data_source)){
    x <- "Initial"
  }else {
    x <- "Upcoming"
  }
  return(x)
}


# ---------------------------------------------
##make disease names look nicer on graphs: 
disease_names_for_plots <- function(gos_data){
  
  graphData <- copy(gos_data)
  graphData[disease=='hiv', disease:='HIV/AIDS']
  graphData[disease=='malaria', disease:='Malaria']
  graphData[disease=='tb', disease:='Tuberculosis']
  graphData[disease=='hss', disease:='RSSH']
  graphData$disease <- factor(graphData$disease, levels=c("HIV/AIDS", "Malaria", "Tuberculosis", "RSSH"))
  return(graphData)
}
# ---------------------------------------------
##make data source names nicer for graphs
data_source_names_for_plots <- function(gos_data){
  
  graphData <- copy(gos_data)
  graphData[data_source=='sicoin',data_source:='SICOIN']
  graphData[data_source=='fpm',data_source:='Final Official Budgets']
  graphData[data_source=='init_fpm',data_source:='Initial Official Budgets']
  graphData[data_source=='init2_fpm',data_source:='2nd Iter. Official Budgets']
  graphData[data_source=='gos_expend',data_source:='GOS Expenditures']
  graphData[data_source=='gos_budget',data_source:='GOS Budgets']
  graphData[data_source=='fgh',data_source:='Financing Global Health']
  graphData[data_source=='pudr',data_source:='Progress Update Disb. Request']
  graphData$data_source<- factor(graphData$data_source, levels=c("SICOIN", "Progress Update Disb. Request",
                                                          "Financing Global Health","Initial FPM Budgets","2nd Iter. FPM Budgets",
                                                          "Final FPM Budgets", "GOS Budgets", "GOS Expenditures"))
  return(graphData)
}


# ---------------------------------------------
##colors to map the GF Modules: 
# ---------------------------------------------
primColors <- c('#254CCB',
                '#3E6BD1', '#588BD8',
                '#71aade', '#8BCAe5', ##blues
               '#e3c8f4','#a23be5'
               , '#2d0254', '#8700ff', 
               '#450184', ##purples
               '#bf9700', #goldenrod
                '#00cdcd',
                '#0f5b5b',#teals
               '#e8946d', '#af3607', ##terracotta
                '#db0645','#e787a3',
                '#f4c7d4', ##blush/milennial pink
                '#17bb5d', '#008000',
                '#73ed02','#76b503',##greens 
                '#88efde', '#9acebd',
               '#294f3f', "#660000", #pinks 
                '#b20000',
               '#7a786e', '#d1d0ca') ##red 

names(primColors) <- c('Prevention programs for MSM'
                       , 'Prevention programs for CSW & clients'
                       ,'Prevention programs for IJU',##blues
                       'Prevention programs for transgender',
                       "Prevention programs for prisoners",
                       'Prevention programs for general pop.'
                       ,'Prevention programs for other KVP',
                       'Prevention programs for youth/adol.', 
                       'PMTCT',
                       'Specific prev. interventions',
                       'Human rights barriers',  ##teals
                       'Case management', 
                       'Vector control', 
                        'Treatment, care & support', 'HIV Testing Services'
                       ,'TB/HIV',
                       'Care & prevention'
                       , 'MDR-TB'
                       ,'PSM'
                       ,'Info systems & M&E',
                       'HR & health workers'
                       , 'Service delivery'
                       ,'Financial systems'
                       , 'Nat. health strategies',
                       'Community systems'
                       , "Program mgmt"
                       ,'Performance Based Financing', 'Unidentified'
                       ) 

# ---------------------------------------------
##set the colors for interventions 
indColors <- c('#a6cee3',
               '#1f78b4',
               '#b2df8a',
               '#33a02c',
               '#fb9a99',
               '#c0c0c0',
               '#fdbf6f',
               '#ff7f00',
               '#cab2d6',
               '#6a3d9a'
               )
# ---------------------------------------------

get_summary_level <- function(gf_module, gf_intervention){
  x <- gf_intervention
  if(is.na(gf_module)){
    x <- "No Data"
  } else if(is.na(gf_intervention)){
    x <- x
  }
  else if(gf_module==gf_intervention){
    x <- "Summary Level Only"
  } else {
    x <- x
  }
  return(x)
}


