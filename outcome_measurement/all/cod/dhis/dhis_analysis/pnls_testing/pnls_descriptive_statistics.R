# descriptive statistics


# main descriptive table

tab = dt[variable=='Tested and received the results' | variable=='HIV+',.(value=sum(value)), 
         by=.(year=year(date), subpop, variable)][rev(order(variable, value))]

# tests performed
tests = tab[variable=='Tested and received the results' & year==2017]
setnames(tests, 'value', '2017')
tests[ ,year:=NULL]
tests2 = tab[variable=='Tested and received the results' & year==2018]
setnames(tests2, 'value', '2018')
tests2[ ,year:=NULL]
tests = merge(tests, tests2, by=c('subpop', 'variable'), all=T)

# hiv+
hiv = tab[variable=='HIV+' & year==2017]
setnames(hiv, 'value', '2017')
hiv[ ,year:=NULL]
hiv2 = tab[variable=='HIV+' & year==2018]
setnames(hiv2, 'value', '2018')
hiv2[ , year:=NULL]
hiv = merge(hiv, hiv2, by=c('subpop', 'variable'), all=T)

# test positivity rate
setnames(hiv, c('2017', '2018'), c('hiv2017', 'hiv2018'))
setnames(tests, c('2017', '2018'), c('tests2017', 'tests2018'))
hiv[ ,variable:=NULL]
tests[,variable:=NULL]
rate = merge(tests, hiv, by='subpop')
rate[ , ratio2017:=100*hiv2017/tests2017]
rate[ , ratio2018:=100*hiv2018/tests2018]
rate[ , ratio2017:=round(ratio2017, 1)]
rate[ , ratio2018:=round(ratio2018, 1)]

# ratio of positive tests to all tests
# how many tests were performed for one positive test?
rate[ , one_pos2017:=round(tests2017/hiv2017, 1)]
rate[ , one_pos2018:=round(tests2018/hiv2018, 1)]

# round the values - some decimals from imputation 
rate[ , tests2017:=round(tests2017)]
rate[ , tests2018:=round(tests2018)]
rate[ , hiv2017:=round(hiv2017)]
rate[ , hiv2018:=round(hiv2018)]


#----------------------------------------------------
# export the table 

dir = "C:/Users/ccarelli/Documents/pnls_data/"

write.csv(rate, paste0(dir, 'pnls_hiv_testing_table_gf_only.csv'))


#----------------------------------------------------

tests = dt[variable=='Tested and received the results',.(value=sum(value)),
           by=.(subpop, date)]

tests[variable=='Tested and received the results', variable:='tests']
tests[subpop=='Clients', key:='pts']
tests[subpop!='Clients', key:='key']
tot_tests = dt[variable=='Tested and received the results',.(value=sum(value)),
               by=date]
tot_tests[, key:='total']

tests = tests[,.(value=sum(value)),by=.(key, date)]
tests = rbind(tests, tot_tests)
tests = dcast(tests, date~key)

tests[ ,perc_key:=round(100*(key/total), 1)]
mean(tests$perc_key)
min(tests$perc_key)
max(tests$perc_key)



tests = dt[variable=='Tested and received the results' | variable=='HIV+' ,.(value=sum(value)),
           by=.(subpop, variable, date)]

tests[variable=='Tested and received the results', variable:='tests']
tests[variable=='HIV+', variable:='hiv']
tests[ , key:=(subpop=='Clients')]
tests = tests[,.(value=sum(value)),
              by=.(key, variable, date)]


tests = dcast(tests, key+date~variable)

tests[ ,pos:=round(100*(hiv/tests), 1)



dt[variable=='Tested and received the results', sum(value), by=year(date)]



