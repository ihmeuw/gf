# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2018-07-10
# TB medicine distribution
# 

library(data.table)
library(ggplot2)
library(haven)

codePath = "PCE/gf/"

# Requirements:
source(paste0(codePath, "core/GT_load_data.R"), encoding = "UTF-8")
source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")

# Load data
tbdistr = read.csv("./PCE/Outcome Measurement Data/TUBERCULOSIS/GTM-TB-distribution-2013-2017.csv")
tbdistr = data.table(tbdistr)
tbdistr[, Product:= trimws(Product)]
# Fix mispellings
tbdistr[Product == "RIFAMPICINA, CAPSULA DE 300 MG.", Product:="RIFAMPICINA, TABLETA DE 300 MG."]
tbdistr[Product == "ISONIAZIDA, TAB LETA DE 300 MG.", Product:="ISONIAZIDA, TABLETA DE 300 MG."]
tbdistr[Product == "PIRAZINAMIDA, TABLET DE 500 MG.", Product:="PIRAZINAMIDA, TABLETA DE 500 MG."]

unique(tbdistr$Product)

tbnots = read.csv("PCE/Outcome Measurement Data/TUBERCULOSIS/GTM - TB notifications 2012-2017.csv")
tbnots = data.table(tbnots)

tbnots[, sum(is.na(as.double(PESOKG)))/.N, by=floor(YearMonth/100) ]
tbnots[, sum(is.na(as.double(PESOLBS)))/.N, by=floor(YearMonth/100) ]
# PESOLBS has less NAs. 
# People weighting more than 25KG are considered adults.

# Adult new cases:
nadults = tbnots[ ( #(as.double(PESOLBS) > 25*2.2) | 
                     (EDAD >= 18)) & str_to_lower(trimws(CLASIFICACION)) != "bk negativo" &
          (CONDICIONINGRESO == "nuevo" | is.na(CONDICIONINGRESO)), .N, by= .(COD_DEPT, YearMonth)]
nkids = tbnots[ ( # (as.double(PESOLBS) <= 25*2.2) | 
                  (EDAD > 4) & (EDAD < 18)) & str_to_lower(trimws(CLASIFICACION)) != "bk negativo" &
                    (CONDICIONINGRESO == "nuevo" | is.na(CONDICIONINGRESO)), .N, by= .(COD_DEPT, YearMonth)]

# First line treatments:

# Treatment for adulst:
# 310 x RIFAMPICINA, TABLETA DE 300 MG.
# 155 x ISONIAZIDA, TABLETA DE 300 MG. | ISONIAZIDA, TAB LETA DE 300 MG.
# 150 x PIRAZINAMIDA, TABLETA DE 500 MG.
# 150 x ETAMBUTOL, TABLETA DE 400 MG.

# Treatment for kids: 
# 10 x PIRAZINAMIDA, TABLETA DE 500 MG.
# 16 x RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML.
# 310 x ISONIAZIDA, TABLETA DE 100 MG.

merge(nadults, tbdistr[Product == "RIFAMPICINA, TABLETA DE 300 MG.", .(Rf = sum(Amount)), 
                       by = .(code_dept, YearMonth = ifelse(Month>1, (Year*100+Month-1), (Year-1)*100 + 12 ) ) ],
      by.x=c("COD_DEPT", "YearMonth"), by.y= c("code_dept", "YearMonth"))

nadults_Rsupply = merge(nadults[YearMonth <= 201710, .(Demand = 310 * sum(N), NewAdultsCases = sum(N)), by=.(Year = floor(YearMonth/100))], 
      tbdistr[Product == "RIFAMPICINA, TABLETA DE 300 MG.", .(Distributed = sum(Amount, na.rm=T)), 
             by = .(Year) ],
      by="Year")

nkids_Rsupply = merge(nkids[YearMonth <= 201710, .(Demand = 16 * sum(N), NewChCases = sum(N)), by=.(Year = floor(YearMonth/100))], 
                        tbdistr[Product == "RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML.", .(Distributed = sum(Amount, na.rm=T)), 
                                by = .(Year) ],
                        by="Year")
ggplot(data=melt(nadults_Rsupply[, .(Demand, Distributed, Year)], id.vars = "Year")) + 
    geom_col(aes(x = Year, y = value, fill=variable), position = "dodge") + 
    labs(y="Rifampizine doses", title="TB medicine distribution versus demand for adults.", subtitle="RIFAMPICINA, TABLETA DE 300 MG.") +
    scale_fill_manual(name="", labels = c("Adult Rifampizine doses demand\naccording to notifications", 
                                 "Adult Rifampizine doses distributed\nby PNT"), values = c("blue", "red"))
  
ggplot(data=melt(nkids_Rsupply[, .(Demand, Distributed, Year)], id.vars = "Year")) + 
  geom_col(aes(x = Year, y = value, fill=variable), position = "dodge") + 
  labs(y="Rifampizine doses", title="TB medicine distribution versus demand for children.", subtitle="RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML.") +
  scale_fill_manual(name="", labels = c("Children Rifampizine doses demand\naccording to notifications", 
                                        "Children Rifampizine doses distributed\nby PNT"), values = c("blue", "red"))

tbnots[ ( #(as.double(PESOLBS) > 25*2.2) | 
  (EDAD >= 18)) & 
    (CONDICIONINGRESO == "nuevo" | is.na(CONDICIONINGRESO)), .N, by= .(str_to_lower(trimws(CLASIFICACION)))]
