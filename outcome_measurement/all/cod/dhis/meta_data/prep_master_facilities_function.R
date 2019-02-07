# Caitlin O'Brien-Carelli
# 1/17/19
# prep function for the master list of facilities
# DHIS2 DRC

prep_facilities = function(x) {
#---------------------------
# create a type of organisational unit variable

# create for all types except facilities 
# add facilities after running the facility level code
x[grep(pattern="du Congo", org_unit),type:='country']
x[grep(pattern="Province", org_unit), type:='dps']
x[grep(pattern="Zone", org_unit), type:='health_zone']
x[grep(pattern="Aire de", org_unit), type:='health_area']

#-----------------------------------------------
# facility level

# create alternate org unit name in lower case and replace the diacritical marks
x[ , org_unit1:=tolower(org_unit)]

# function to remove diacritical marks
fix_diacritics = function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  replace_me = paste(names(replacement_chars), collapse='')
  replace_with = paste(replacement_chars, collapse = '')    
  return(chartr(replace_me, replace_with, x))
  
}

x[ , org_unit1:=fix_diacritics(org_unit1)]

#----------------------
# classify the facilities
# run the clinic code first - since a number of facilities have clinic and another class
# geographic areas will be missing

# health facilities by level of care
x[grep(pattern="\\sclinique", x=org_unit1), level:='clinic']
x[grep(pattern="centre de sante", x=org_unit1), level:='health_center']
x[grep(pattern="centre sante", x=org_unit1), level:='health_center']
x[grep(pattern="centre de sante de reference", x=org_unit1), level:='reference_health_center']

x[grep(pattern="poste de sante", x=org_unit1), level:='health_post']
x[grep(pattern="poste", x=org_unit1), level:='health_post']

x[grep(pattern="centre medical", x=org_unit1), level:='medical_center']

x[grep(pattern="\\shopital", x=org_unit1), level:='hospital']
x[grep(pattern="\\shopital secondaire", x=org_unit1), level:='secondary_hospital']
x[grep(pattern="hopital general de reference", x=org_unit1), level:='general_reference_hospital']
x[grep(pattern="hgr", x=org_unit1), level:='general_reference_hospital']
x[grep(pattern="general de reference", x=org_unit1), level:='general_reference_hospital']

x[grep(pattern="centre hospitalier", x=org_unit1), level:='hospital_center']
x[grep(pattern="dispensaire", x=org_unit1), level:='dispensary']
x[grep(pattern="polyclinique", x=org_unit1), level:='polyclinic']
x[grep(pattern="chirurgical", x=org_unit1), level:='medical_surgical_center']

#-----------------------------------
# drop out the variable used to detect level and type
x[ , org_unit1:=NULL]

#-----------------------------------
# add facility to the type
# 83 facilities have typos in the names

x[is.na(type) & !is.na(level), type:='facility']
#-----------------------------------
return(x)

}


