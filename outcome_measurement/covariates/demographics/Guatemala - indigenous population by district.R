# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2017-12-11
# Here I get indigenous population proportions for each Guatemalan district based on the ENCOVI survey for 2014.
# ----------------------------------------------
# Dependencies
library(haven)
library(data.table)
library(dplyr)

# ----------------------------------------------
# Load data
encovi14P = read_sav("/DATOS/ENCOVI/ENCOVI 2014 Personas.sav")

# ----------------------------------------------
# Prepare data
dtencovi14P = data.table(encovi14P)
dtencovi14P[, ETNIA := recode_factor(as.numeric(P04A11A),  `97` = "N", `99` = "N",  `30` = "Extr", `29` = "Ldn", .default = "Indgn", .missing= "N") ]

total = sum(dtencovi14P$FACTOR)

# ----------------------------------------------
# Attempting to get one known value seen  in the general report of the ENCOVI
# Found out the FACTOR variable is the weight for each sample.
# Proporción de mujeres:

sum(dtencovi14P[PPA02==2, FACTOR]) / total

# [1] 0.5143269
# This seems correct

# ----------------------------------------------
# Proportion of ethnical groups
dtencovi14P[, sum(FACTOR)/total, by=ETNIA]

# ETNIA           V1
# 1:   Ldn 0.608640687
# 2: Indgn 0.387547710
# 3:  Extr 0.001541262
# 4:     N 0.002270341

# Proportio of ethnic groups by department or district:
deptoEtnia = dcast(dtencovi14P[, sum(FACTOR), by=list(DEPTO, ETNIA) ],  DEPTO~ETNIA, fill=0)
# >  deptoEtnia
#
#     DEPTO     N     Extr     Ldn   Indgn
# 1:     1  1155 17885.29 2949274  359974
# 2:     2    NA       NA  167411     109
# 3:     3   231   938.00  227571  110877
# 4:     4  3223       NA  194249  490937
# 5:     5  1084       NA  714963   37814
# 6:     6    NA       NA  340193   30706
# 7:     7   578  1302.00   13908  468201
# 8:     8   178       NA   33623  494546
# 9:     9    NA   666.00  450348  401985
# 10:    10  2176   315.00  405475  155467
# 11:    11  2136   678.00  306000   22365
# 12:    12  2202       NA  739805  364997
# 13:    13    NA       NA  549175  698483
# 14:    14   738       NA  172029  897032
# 15:    15  1333       NA  156742  137795
# 16:    16  4544  1128.00   80098 1155596
# 17:    17    NA       NA  584986  137371
# 18:    18    NA   512.00  287258  162129
# 19:    19  2156   902.00  231554     778
# 20:    20  2830       NA  359394   39143
# 21:    21 11124       NA  317678   26819
# 22:    22   642   337.00  457737    8416
# DEPTO     N     Extr     Ldn   Indgn

indgnProp = deptoEtnia[, .(DEPTO, DEPTOPOB = Indgn/(N+Extr+Ldn+Indgn) ) ]
#indgnProp = deptoEtnia[, .(DEPTO, Indgn/(N+Extr+Ldn+Indgn) ) ]$V2
write.csv(indgnProp, "./PCE/Outcome Measurement Data/Covariates/Demographics/Guatemala_indigenousPobProp.csv")
# DEPTO           V2
# 1:     1 0.1081559048
# 2:     2 0.0006506686
# 3:     3 0.3264765898
# 4:     4 0.7131472715
# 5:     5 0.0501604407
# 6:     6 0.0827880366
# 7:     7 0.9673794239
# 8:     8 0.9360249987
# 9:     9 0.4712608104
# 10:    10 0.2759281050
# 11:    11 0.0675314558
# 12:    12 0.3297160625
# 13:    13 0.5598353074
# 14:    14 0.8385051771
# 15:    15 0.4657281914
# 16:    16 0.9309067592
# 17:    17 0.1901705113
# 18:    18 0.3603675492
# 19:    19 0.0033051532
# 20:    20 0.0975242110
# 21:    21 0.0754145565
# 22:    22 0.0180163209
# DEPTO           V2
# indgnProp = c(0.108155904777471, 0.000650668576886342, 0.326476589805575, 0.713147271462169, 0.0501604407178512, 0.0827880366353104, 0.967379423912527, 0.936024998722431, 0.47126081038782, 0.275928105027572, 0.0675314557988278, 0.329716062453252, 0.559835307432005, 0.838505177140753, 0.465728191435428, 0.930906759166918, 0.190170511256899, 0.360367549161034, 0.00330515315009134, 0.0975242110088772, 0.0754145565081927, 0.0180163208686196)
