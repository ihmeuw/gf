# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code written by Irena Chen
# PURPOSE: Prep commonly-formatted coverage indicator sheet from 
#   PU/DRs across countries. 
# DATE: Last updated June 2019. 
# ----------------------------------------------

prep_coverage_1B_disagg =  function(dir, inFile, sheet_name, language) {
  
  #TROUBLESHOOTING HELP
  # #Uncomment variables below and run line-by-line.
  # folder = "budgets"
  # folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
  # if (file_list$file_iteration[i]=="initial"){
  #   version = "iterations"
  # } else if (file_list$file_iteration[i]=="revision"){
  #   version= "revisions"
  # } else {
  #   version = ""
  # }
  # grant_period = file_list$grant_period[i]
  # 
  # dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", grant_period, "/", folder, "/")
  # if (version != ""){
  #   dir = paste0(dir, version, "/")
  # }
  # inFile = file_list$file_name[i]
  # sheet_name = file_list$sheet_coverage_1b_disagg[i]
  # language = file_list$language_programmatic[i]

  # Sanity check: Is this sheet name one you've checked before? 
  verified_sheet_names <- c('Disaggregation_1B', 'Ventilation_1B')
  if (!sheet_name%in%verified_sheet_names){
    print(sheet_name)
    stop("This sheet name has not been run with this function before - Are you sure you want this function? Add sheet name to verified list within function to proceed.")
  }
  
  # Load/prep data
  gf_data <-data.table(read.xlsx(paste0(dir,inFile), sheet=sheet_name, detectDates=TRUE))
  
  #------------------------------------------------------
  # 1. Select columns, and fix names 
  #------------------------------------------------------
  module_col = grep("Module|Módulo", gf_data)
  extra_module_col = grep("HIVAIDS_Module", gf_data)
  if (length(extra_module_col)>0){
    if (verbose){
      print("Extra name rows are being dropped.")
      print(gf_data[[extra_module_col]])
    }
    gf_data[[extra_module_col]]<-NULL
    module_col = module_col[module_col!=extra_module_col]
  }
  stopifnot(length(module_col)==1)
  name_row = grep("Module|Módulo", gf_data[[module_col]])
  extra_name_row = grep("Module Name", gf_data[[module_col]])
  if (length(extra_name_row)>0){
    if (verbose){
      print("Extra name rows are being dropped.")
      print(gf_data[extra_name_row])
    }
    gf_data = gf_data[-extra_name_row, ]
    name_row = name_row[name_row!=extra_name_row]
  }
  stopifnot(length(name_row)==1)
  
  names = gf_data[name_row, ]
  names = tolower(names)
  names = gsub("\\.", "_", names)
  
  #Drop out the comments column, and record ID column. 
  # If the module column is #3, drop the first two rows. 
  comment_col = grep("comment|comentario", names) 
  record_id_col = grep("record id", tolower(gf_data))
  stopifnot(length(record_id_col)==1 | is.na(record_id_col)) #Just don't drop more than one column here. 
  gf_data = gf_data[, !c(comment_col, record_id_col), with=FALSE] 
  if (module_col==3){
    gf_data = gf_data[, 3:ncol(gf_data)] #Drop the first two columns in this case, they're unnecessary. 
  }
  
  #Reset names 
  names = gf_data[name_row, ]
  names = tolower(names)
  names = gsub("\\.", "_", names)
  
  #Drop out column for country
  country_col = grep("country", names)
  if (length(country_col)>0){
    gf_data = gf_data[, !c(country_col), with=F]
  }
  #------------------------------------------------------
  # 2. Reset names after subset above. 
  #------------------------------------------------------
  
  module_col = grep("Module|Módulo", gf_data)
  stopifnot(length(module_col)==1)
  name_row = grep("Module|Módulo", gf_data[[module_col]])
  stopifnot(length(name_row)==1)
  
  names = gf_data[name_row, ]
  names = tolower(names)
  names = gsub("\\.", "_", names)
  
  names(gf_data) = names
  
  #Sometimes, there is a row right before the names row that says where the LFA and Global Fund verified sections begin, respectively. 
  #EMILY KEEP DEBUGGING THIS!! 
  #pre_name_row = name_row-1
  # lfa_start_col = grep()
 
   #Drop everything before the name row, because it isn't needed 
  gf_data = gf_data[(name_row+1):nrow(gf_data)] #Go ahead and drop out the name row here too because you've already captured it
  sub_names = as.character(gf_data[1, ])
  
  #------------------------------------------------------
  # 3. Rename columns 
  #------------------------------------------------------
  
  #Remove diacritical marks from names to make grepping easier
  names = fix_diacritics(names)
  names = gsub("\\n", "", names)
  
  if (language == "fr"){
    reference_col = grep("reference", names)
    # target_col = grep("cible", names)
    result_col = grep("resultats", names)
    lfa_result_col = grep("verified result", names)
    gf_result_col = grep("global fund validated result", names) 
  } else if (language == "eng"){
    reference_col = grep("baseline", names) 
    # target_col = grep("target", names)
    result_col = grep("result", names) 
    lfa_result_col = grep("verified result", names)
    gf_result_col = grep("global fund validated result|validated result", names)
  } else if (language=="esp"){
    reference_col = grep("linea de base|base de referencia", names) 
    # target_col = grep("meta", names)
    result_col = grep("resultados", names) 
    lfa_result_col = grep("verified result", names)
    gf_result_col = grep("global fund validated result", names)
  }
  
  if (length(result_col)>1){ #The word 'result' appears several times for English files, and you just want the first column here. 
    result_col = result_col[1]
  }
  
  #Validate that you grabbed exactly 5 columns. 
  flagged_col_list = c(reference_col, result_col, lfa_result_col, gf_result_col)
  stopifnot(length(flagged_col_list)==4)
  
  #------------------------------------------------------------
  # DYNAMICALLY RE-ASSIGN NAMES (DUE TO MULTIPLE FILE FORMATS)
  #------------------------------------------------------------
  #1. Tag the names that you currently have. 
  #2. Match them from a list of previously tagged names. 
  #3. Build up a list of correctly named vectors in the order in which it appears. 
  # 4. Reset names 
  
  #---------------------------------------------
  # MAIN NAMES 
  #---------------------------------------------
  
  #Acceptable raw column names - will be matched to corrected names below. 
  module_names = c('module', "modulo")
  standard_ind_names = c('standard coverage indicator', 'indicateurs', 'coverage indicator', 'indicateurs standard', "indicadores")
  subcat_names = c('ventilation', 'disaggregation', 'desglose')
  category_names = c('categorie', 'categoria')
  geography_names = c('geographic area', 'country', 'geographie', 'geografia')
  cumulative_target_names = c('targets cumulative?', "cibles cumulatives ?")
  reverse_ind_names = c("reverse indicator?")
  
  baseline_names = c('baseline (if applicable)', "reference", "base de referencia")
  result_names = c('result', 'resultats', 'results', 'resultados')
  lfa_result_names = c('verified result', 'verified results')
  gf_result_names = c('validated result', "global fund validated result", 'validated results')
  
  #Correct these matched names. 
  names[which(names%in%module_names)] = "module"
  names[which(names%in%standard_ind_names)] = "indicator"
  names[which(names%in%category_names)] = "category"
  names[which(names%in%subcat_names)] = "sub-category"
  names[which(names%in%geography_names)] = "geography"
  names[which(names%in%cumulative_target_names)] = "cumulative_target"
  names[which(names%in%reverse_ind_names)] = "reverse_indicator"
  
  names[which(names%in%baseline_names)] = "baseline"
  names[which(names%in%result_names)] = "pr_result"
  names[which(names%in%lfa_result_names)] = "lfa_result"
  names[which(names%in%gf_result_names)] = "gf_result"
  
  
  #Where 'achievement ratio' exists in the names vector, move to the sub-names vector 
  achievement_ratio_names = c('achievement ratio', "taux d'accomplissement", "achivement ratio(final one is calculated by gos)", "achivement ratio")
  ach_ratio_indices = which(names%in%achievement_ratio_names)
  stopifnot(is.na(unique(sub_names[ach_ratio_indices])))
  sub_names[ach_ratio_indices] = "achievement_ratio"
  names[ach_ratio_indices] = NA
  
  #Where 'verification method' exists in the names vector, move to the sub-names vector 
  verification_method_names = c('verification method', "data validation checks on pr data", "data validation checks on lfa data", "data validation checks on gf data")
  ver_method_indices = which(names%in%verification_method_names)
  stopifnot(is.na(unique(sub_names[ver_method_indices])))
  sub_names[ver_method_indices] = "verification_method"
  names[ver_method_indices] = NA
  
  #Where 'source' exists in the names vector, move to the sub-names vector 
  data_source_names = c('source', 'fuente')
  source_indices = which(names%in%data_source_names)
  stopifnot(is.na(unique(sub_names[source_indices])))
  sub_names[source_indices] = "source"
  names[source_indices] = NA
  
  #Make sure you've tagged all names correctly so far. 
  if (verbose){
    print("These are the variable names that haven't been correctly tagged.")
    print(names[!names%in%c("module", "category", "sub-category", "indicator", "custom_coverage_indicator", "geography", 
                              "cumulative_target", "reverse_indicator", "baseline", "target", "pr_result", "lfa_result", "gf_result", NA)])
  }
  stopifnot(names%in%c("module", "category", "sub-category", "indicator", "custom_coverage_indicator", "geography", 
                       "cumulative_target", "reverse_indicator", "baseline", "target", "pr_result", "lfa_result", "gf_result") | is.na(names))
  
  #----------------------------------
  # SUB-NAMES 
  #----------------------------------
  num_names = c("N#")
  denom_names = c("D#")
  proportion_names = c("%")
  year_names = c("Year", "Année", "Año")
  verification_source_names = c("Source", "source", "Fuente")
  
  sub_names[which(sub_names%in%num_names)] = "n"
  sub_names[which(sub_names%in%denom_names)] = "d"
  sub_names[which(sub_names%in%proportion_names)] = "%"
  sub_names[which(sub_names%in%year_names)] = "year"
  sub_names[which(sub_names%in%verification_source_names)] = "source"
  
  #Certain column names are okay to change to NA here. 
  na_names = c("If sub-national, please specify under the \"Comments\" Column", "Si infranationale, veuillez préciser dans la colonne des commentaires")
  sub_names[which(sub_names%in%na_names)] = NA
  
  if (verbose){
    print("These are the sub-names that haven't been correctly tagged.")
    print(sub_names[!sub_names%in%c('n', 'd', '%', 'year', 'source', 'achievement_ratio', 'verification_method', NA)])
  }
  stopifnot(sub_names%in%c('n', 'd', '%', 'year', 'source', 'achievement_ratio', 'verification_method') | is.na(sub_names))
  
  #------------------------------------------
  # REASSIGN NAMES USING CORRECTED VECTORS
  
  #First, extend each of the 'flag' column names to cover the whole span. 
  names[reference_col:(result_col-1)] = "baseline"
  names[result_col:(lfa_result_col-1)] = "pr_result"
  names[lfa_result_col:(gf_result_col-1)] = "lfa_result"
  names[gf_result_col:length(names)] = "gf_result"
  stopifnot(!is.na(names))
  
  #Second, append names and subnames. 
  stopifnot(length(names)==length(sub_names))
  final_names = names
  for (i in 1:length(sub_names)){
    if (!is.na(sub_names[i])){
      final_names[i] = paste0(names[i], "_", sub_names[i])
    }
  }
  
  #Make sure your name vector still matches the length of the data! 
  stopifnot(length(final_names)==ncol(gf_data))
  names(gf_data) = final_names
  #------------------------------------------------------
  # 2. Drop out empty rows 
  #------------------------------------------------------
  
  #Drop out rows that have NAs, and drop the sub names column. 
  gf_data = gf_data[-c(1)]
  gf_data = gf_data[!(is.na(module) & is.na(indicator)), ] 
  
  return(gf_data)
}


