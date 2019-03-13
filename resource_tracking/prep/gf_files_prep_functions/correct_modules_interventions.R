# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Correct unmapped modules for all countries in resource tracking database. 
#         Add initial, date, and filepath.
# DATE: Last updated March 2019
# --------------------------------------------------------------------

correct_modules_interventions = function(dt){
  
  #2018-2020 grants 
  dt[module == 'rsshintegratedservicedeliveryandqualityimprovement' & intervention == 'otherinterventionsforadolescentandyouth' & is.na(activity_description), module:='preventionprogramsforadolescentsandyouthinandoutofschool']
  dt[module == 'systcmesdesanteresiliantsetperennesprestationdeservicesintegresetameliorationdelaqualite' & intervention == 'autresinterventionsciblantlesjeunesetlesadolescents', module:='programmesdepreventiondestinesauxadolescentsetauxjeunesscolarisesounon']
  dt[module == 'systcmesdesanteresiliantsetperennessystcmedegestiondelinformationsanitaireetsuivietevaluation' & intervention == 'implicationdetouslesprestatairesdesoins', 
           module:='priseenchargeetpreventiondelatuberculose']
  
  #2015-2017 DRC grants 
  dt[module == 'unspecified' & intervention == 'volet3preventiondelatransmissionverticaleduvih', module:='preventiondelatransmissiondelamcreslenfantptme']
  dt[module=='unspecified' & intervention == 'enquetes', module:='systcmesdesanteresiliantsetperennessystcmedegestiondelinformationsanitaireetsuivietevaluation']
  dt[intervention == 'enquetes', intervention:='enquete']
  dt[module=='unspecified' & intervention == 'analyseexamenettransparence', module:='suivietevaluation']
  dt[module=='casemanagement' & intervention == 'privatesectorcasemanagementother', intervention:='privatesectorcasemanagement']
  dt[intervention == 'preservatifsdanslecadredesprogrammesdestinesauxhommesayantdesrapportssexuelsavecdeshommesetauxtransgenres' & module=='preventionhsmettransgenres', module:='traitementpriseenchargeetsoutien']
  dt[intervention == 'depistageduvihetconseildanslecadredesprogrammesdestinesauxhommesayantdesrapportssexuelsavecdeshommesetauxtransgenres' & module=='preventionhsmettransgenres', module:='traitementpriseenchargeetsoutien']
  dt[module=='6' | module=='4', module:='unspecified']
  dt[intervention == '7' | intervention == '6', intervention:='unspecified']
  
  
return(dt)
}
