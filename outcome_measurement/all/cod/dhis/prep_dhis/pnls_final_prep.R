# Final cleaning on PNLS
# Run the function on the subset and 


#---------------------------------------
# export the abbreviated elements for translation

# to do this on the cluster, you must export as an RDS, then use local code to save
elements = x[ ,.(element = unique(element)), by=.(element_id)]
saveRDS(elements, paste0(dir, 'meta_data/translate/pnls_elements.rds'))

#---------------------
# xlsx files do not work well on the cluster
# code to run offline
# to_translate = readRDS(elements, paste0(dir, 'meta_data/translate/pnls_elements.rds'))
# write.xlsx(paste0(paste0(dir,'meta_data/translate/pnls_elements_to_translate.xlsx' )))

# translate using onlinedoctranslator.com and save as file path below
#---------------------

# import the translated elements
new_elements = read.xlsx(paste0(paste0(dir,
                                       'meta_data/translate/pnls_elements_translations.xlsx' )))

# reset the variable name for the merge and merge on element id
setnames(new_elements, 'element', 'element_eng')
x = merge(x, new_elements, by='element_id', all.x=T )

#---------------------------------------