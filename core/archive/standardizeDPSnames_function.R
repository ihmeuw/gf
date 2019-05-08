


standardizeDPSnames <- function(vector_of_dps_names){
  
  vector_of_dps_names <- gsub("Bas-Uélé", "Bas-Uele", vector_of_dps_names)
  vector_of_dps_names <- gsub("�???quateur", "Equateur", vector_of_dps_names)
  vector_of_dps_names <- gsub("Haut-Uélé", "Haut-Uele", vector_of_dps_names)
  vector_of_dps_names <- gsub("Kasaï", "Kasai", vector_of_dps_names)
  vector_of_dps_names <- gsub("Kasaï-Central", "Kasai Central", vector_of_dps_names)
  vector_of_dps_names <- gsub("Kasaï-Oriental", "Kasai Oriental", vector_of_dps_names)
  vector_of_dps_names <- gsub("Maï-Ndombe", "Mai-Ndombe", vector_of_dps_names)
  
  vector_of_dps_names <- tolower(vector_of_dps_names)
  vector_of_dps_names <- gsub(" ", "-", vector_of_dps_names)
  
  vector_of_dps_names <- gsub("bas-congo", "kongo-central", vector_of_dps_names)
  
  return(vector_of_dps_names)
  
}