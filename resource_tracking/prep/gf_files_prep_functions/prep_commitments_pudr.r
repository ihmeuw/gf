# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code written by Irena Chen
# PURPOSE: Prep commonly-formatted PU/DRs across countries. 
# DATE: Last updated January 2019. 

# For this function to work properly, a file must have a section labeled "Modular approach" with columns for module, intervention, budget, and expenditure. 
# Returns a file called budget dataset with columns for module, intervention, budget, and expenditure. 
# ----------------------------------------------

#Sheet names that don't work so far: "LFA EFR_7", "LFA_Annex-SR Financials", "LFA_Total PR Cash Outflow_3", "LFA_Total PR Cash Outflow_3A"

# start function
prep_commitments_pudr =  function(dir, inFile) {
  
  #TROUBLESHOOTING HELP
  #Uncomment variables below and run line-by-line. 
  # Set up file path 
  # folder = "budgets"
  # folder = ifelse (file_list_subset$data_source[i] == "pudr", "pudrs", folder)
  # if (file_list_subset$file_iteration[i]=="initial"){
  #   version = "iterations"
  # } else if (file_list_subset$file_iteration[i]=="revision"){
  #   version= "revisions"
  # } else {
  #   version = ""
  # }
  # grant_period = file_list_subset$grant_period[i]
  # 
  # file_dir = paste0(master_file_dir, file_list_subset$grant_status[i], "/", file_list_subset$grant[i], "/", grant_period, "/", folder, "/")
  # if (version != ""){
  #   file_dir = paste0(file_dir, version, "/")
  # }
  # dir = file_dir
  # inFile = file_list_subset$file_name[i]
  # sheet_name = file_list_subset$sheet_financial[i]
  # start_date = file_list_subset$start_date_financial[i]
  # period = file_list_subset$period[i]
  # disease = file_list_subset$disease[i]
  # grant = file_list_subset$grant[i]
  # recipient = file_list_subset$primary_recipient
  # source = file_list_subset$data_source[i]
  # qtr_number = file_list_subset$qtr_number[i]

  # -----------------------------------------------------------------------------
  # Test the inputs to make sure that they are the correct type
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # -----------------------------------------------------------------------------
  # Files and directories
  
  # Load/prep data
  gf_data <-data.table(read.xlsx(paste0(dir,inFile), sheet="Commitments_Obligations", detectDates=TRUE))
  
  # There are two tables that you need to extract separately, "commitments" and "obligations". 

  #General function for grants.
  #-------------------------------------
  # 1. Divide into tables (subset rows)
  #-------------------------------------
  module_column = grep("Module|Módulos", gf_data)
  if (inFile=="SEN-H-ANCS_PUDR (Juil-Dec18) LFA, 15Mar18.xlsx") module_column = module_column[1] # This file mentions 'module' in the comments column. 
  stopifnot(length(module_column)==1)
  start_rows = grep("Module|Módulos", gf_data[[module_column]])
  stopifnot(length(start_rows)==2)
  
  total_column = grep("Total", gf_data)
  if (inFile%in%c("PUDR 31122018 revised 13052019.xlsx", 
                  "Final LFA reviewed UGA-M-TASO PUDR 2 Oct 2018.xlsx", 
                  "LFA Reviewed UGA-C-TASO PE 31Dec18.xlsx", 
                  "LFA verified UGA-C-TASO PU PE 30 June 2019 12 Sept 19.xlsx", 
                  "LFA verified UGA-M-TASO PU PE 30 June 2019 12 Sept 19.xlsx", 
                  "UGA-M-TASO PUDR Jul-Dec18.xlsx")){
    total_column = total_column[1] #These files mention "Total" in a comments column. 
  }  
  stopifnot(length(total_column)==1)
  end_rows = grep("Total", gf_data[[total_column]])
  stopifnot(length(end_rows)==2)
  stopifnot(end_rows[1]<start_rows[2]) #Make sure your tables don't overlap! 
  
  #Subset data 
  commitments = gf_data[start_rows[1]:end_rows[1]]
  obligations = gf_data[start_rows[2]:end_rows[2]]
 
  #-------------------------------------
  # 2. Rename and subset columns 
  #-------------------------------------
  module_names = c('Module ', "Módulos")
  intervention_names = c('Intervention', "Intervenciones")
  activity_names = c("Description de l'activité", "Activity Description", "Descripcion de la actividad")
  category_names = c("Entrées de coûts ", "Cost input", "Entrada de costos")
  budget_names = c("Montant en monnaie de la subvention", "Amount in Grant Currency", "Monto en moneda de la Subvencion")
  delivery_date_names = c('Date de livraison', "Delivery date", "Expected Delivery date", "Fecha de entrega")
  scheduled_date_names = c('Date de paiement prévue', 'Expected Payment date ', "Fecha de pago prevista")
  effective_payment_date_names = c('Date de paiement effective', "Effective Payment date ", "Fecha de pago efectiva")
  lfa_adjustments_names = c('LFA Adjustments') 
  lfa_budget_names = c('As verified by LFA')
  ct_adjustments_names = c('CT Adjustments (incl. External Audit adjustments)')
  gf_budget_names = c("The Global Fund Validated Figures")
  
  commitments_names = as.character(commitments[1])
  obligations_names = as.character(obligations[1])

  commitments_names[commitments_names%in%module_names] = "module"
  commitments_names[commitments_names%in%intervention_names] = "intervention"
  commitments_names[commitments_names%in%activity_names] = "activity_description"
  commitments_names[commitments_names%in%category_names] = "cost_category"
  commitments_names[commitments_names%in%budget_names] = "budget"
  commitments_names[commitments_names%in%delivery_date_names] = "delivery_date"
  commitments_names[commitments_names%in%scheduled_date_names] = "expected_payment_date"
  commitments_names[commitments_names%in%effective_payment_date_names] = "effective_payment_date"
  commitments_names[commitments_names%in%lfa_adjustments_names] = "lfa_adjustment"
  commitments_names[commitments_names%in%lfa_budget_names] = "lfa_budget"
  commitments_names[commitments_names%in%ct_adjustments_names] = "ct_adjustment"
  commitments_names[commitments_names%in%gf_budget_names] = "gf_budget"
  
  obligations_names[obligations_names%in%module_names] = "module"
  obligations_names[obligations_names%in%intervention_names] = "intervention"
  obligations_names[obligations_names%in%activity_names] = "activity_description"
  obligations_names[obligations_names%in%category_names] = "cost_category"
  obligations_names[obligations_names%in%budget_names] = "budget"
  obligations_names[obligations_names%in%delivery_date_names] = "delivery_date"
  obligations_names[obligations_names%in%scheduled_date_names] = "expected_payment_date"
  obligations_names[obligations_names%in%effective_payment_date_names] = "effective_payment_date"
  obligations_names[obligations_names%in%lfa_adjustments_names] = "lfa_adjustment"
  obligations_names[obligations_names%in%lfa_budget_names] = "lfa_budget"
  obligations_names[obligations_names%in%ct_adjustments_names] = "ct_adjustment"
  obligations_names[obligations_names%in%gf_budget_names] = "gf_budget"
  
  keep_cols = c('module', 'intervention', 'activity_description', 'cost_category', 'budget','delivery_date', 'expected_payment_date',
                'effective_payment_date', 'lfa_adjustment', 'lfa_budget', 'ct_adjustment', 'gf_budget')
  drop_cols = c(commitments_names, obligations_names)
  drop_cols = drop_cols[!drop_cols%in%keep_cols]
  
  # This is a vector of columns we don't want to analyze. 
  # If anything is being dropped that's not in this list, flag it. 
  ok_to_drop = c('#', 'Commentaires', 'Comments', 'Comentarios', NA)
  drop_cols = drop_cols[!drop_cols%in%ok_to_drop]
  if (verbose) {
    print("The following columns are being dropped") 
    print(drop_cols)
  }
  if (length(drop_cols)!=0) stop("Unknown columns are being dropped! Validate function.")
  
  # Reset the names of commitments and obligations, and only keep the columns you need. 
  names(commitments) = commitments_names
  commitments = commitments[, names(commitments)%in%keep_cols, with=FALSE]
  names(obligations) = obligations_names
  obligations = obligations[, names(obligations)%in%keep_cols, with=FALSE]
  
  # Drop the first row, which is names, and the last row, which is a total.
  commitments = commitments[2:(nrow(commitments)-1)]
  obligations = obligations[2:(nrow(obligations)-1)]
  
  # Drop any rows where there's no information 
  commitments = commitments[!(is.na(module) & is.na(intervention) & is.na(activity_description) & is.na(cost_category)) & 
                              !((is.na(budget)|budget==0) & (is.na(lfa_adjustment)|lfa_adjustment==0) & (is.na(gf_budget)|gf_budget==0))]
  obligations = obligations[!(is.na(module) & is.na(intervention) & is.na(activity_description) & is.na(cost_category)) & 
                              !((is.na(budget)|budget==0) & (is.na(lfa_adjustment)|lfa_adjustment==0) & (is.na(gf_budget)|gf_budget==0))]
  
  #-------------------------------------
  # 3. Bind data together, and label it. 
  #-------------------------------------
  commitments[, type:="commitment"]
  obligations[, type:="obligation"]
  
  dt = rbind(commitments, obligations, use.names=T, fill=T)
  return(dt)

}


