

pnls_subset = function(x) {
  
# subset the pnls data set to only the relevant elements for analysis
# save a smaller subset with less elements

# load the original prepped data
pnls = readRDS(paste0(dir, 'prepped/pnls_drc_01_2017_07_2018_prepped.rds'))

# drop unecessary variables
pnls[ , c('type', 'drug', 'tableau', 'coordinates',
          'opening_date', 'last_update', 'org_unit_type', 'month'):=NULL]

# drop anything containing soutien as the last word
pnls[ ,last:=word(element, -1)]
pnls = pnls[!grep('soutien', last)]

# drop out elements that end in 'sex' (remainder include sex and age)
pnls = pnls[!grep('sex', last) ]

#---------------------------------------------

#------------------
# classify the elements

# create an element easier to grep
pnls[ , element1:=tolower(element)]

# drop diacritical marks
fix_diacritics <- function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')

  replace_me <- paste(names(replacement_chars), collapse='')
  replace_with <- paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x))

}

# run the function to eliminate diacritical marks
pnls[ , element1:=fix_diacritics(element1)]
pnls[ ,last:=NULL]
#-------------------------------
# save the smaller file
 return(x)
  
  
}
