rename_indicators <- function(data_table, variable_to_rename){
  vars <- unique(data_table[, get(variable_to_rename)])
  
  data_table$old_name = data_table[, get(variable_to_rename)] # save the old names in case the renaming messes up
  
  for (v in vars){
    input <- readline(prompt= paste0("What do you want to rename ", v, "? Type 'skip' to not rename.  "))
    input <- trimws(input)
    
    if(input=="skip") { 
      data_table[get(variable_to_rename) == v, eval(variable_to_rename) := v] 
    } else {
    data_table[get(variable_to_rename) == v, eval(variable_to_rename) := input]
    }
  }
  
  return(data_table)
}