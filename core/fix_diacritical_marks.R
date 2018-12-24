# Function to eliminate diacritical marks from data 

# fix diacritics function 

fix_diacritics = function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', '�0�8'='A', '�0�9'='A', '�0�0'='A', '�0�1'='A', '�0�2'='A', '�0�3'='A', '�0�4'='A', '�0�5'='C', '�0�6'='E', '�0�7'='E',
                           '�0�8'='E', '�0�9'='E', '�0�0'='I', '�0�1'='I', '�0�2'='I', '�0�3'='I', '�0�5'='N', '�0�6'='O', '�0�7'='O', '�0�8'='O', '�0�9'='O', '�0�0'='O', '�0�1'='O', '�0�2'='U',
                           '�0�3'='U', '�0�4'='U', '�0�5'='U', '�0�6'='Y', '�0�7'='B', '�0�8'='Ss', '��'='a', '��'='a', '�0�9'='a', '�0�0'='a', '�0�1'='a', '�0�2'='a', '�0�3'='a', '�0�4'='c',
                           '��'='e', '��'='e', '��'='e', '�0�5'='e', '��'='i', '��'='i', '�0�6'='i', '�0�7'='i', '�0�8'='o', '�0�9'='n', '��'='o', '��'='o', '�0�0'='o', '�0�1'='o',
                           '�0�2'='o', '�0�3'='o', '��'='u', '��'='u', '�0�4'='u', '�0�5'='y', '�0�5'='y', '�0�6'='b', '�0�7'='y')
  
  replace_me <- paste(names(replacement_chars), collapse='')
  replace_with <- paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x))
  
}
