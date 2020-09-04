dt= as.data.table(read.csv(paste0(box, 'tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv')))

dt = dt[loc_name == 'Uganda' & budget_version %in% c('funding_request', 'approved') & grant_period == '2018-2020' & isTopicArea == TRUE, 
        .(loc_name, gf_module, gf_intervention, activity_description, cost_category, disease, pr, budget_version, grant_period, 
          isTopicArea, topicAreaDesc, budget)]


dt_wide = dcast.data.table(dt, loc_name + gf_module + gf_intervention + activity_description + cost_category + disease + pr + grant_period + 
                             isTopicArea + topicAreaDesc ~ budget_version, value.var = 'budget')


write.csv(dt_wide, paste0(box, 'UGA/prepped_data/activity_level_analysis/activity_level_comparison_fr_to_gm.csv'), row.names = FALSE)
