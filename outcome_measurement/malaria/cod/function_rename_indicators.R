rename_variable <- function(data_table, variable_to_rename){
  vars <- unique(data_table[, get(variable_to_rename)])
  
  data_table$old_name = data_table[, get(variable_to_rename)] # save the old names in case the renaming messes up
  
  for (v in vars){
    input <- readline(prompt= paste0("What do you want to rename ", v, "? Press ENTER to skip or type 'STOP' to end.  "))
    input <- trimws(input)
    
    if(input == "") { 
      next
    } else if (input== "STOP"){
      break
    } else {
      data_table[get(variable_to_rename) == v, eval(variable_to_rename) := input]
    }
  }
  return(data_table)
}

dt <- copy(dt_pnlp)
dt <- rename_varaible(dt, "indicator")
