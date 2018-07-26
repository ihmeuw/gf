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
tbdistr = read.csv("./PCE/Outcome Measurement Data/TUBERCULOSIS/GTM-TB-distribution-2013-2018.csv")
tbdistr = data.table(tbdistr)
tbdistr[, Product:= trimws(Product)]
# Fix mispellings
tbdistr[Product == "RIFAMPICINA, CAPSULA DE 300 MG.", Product:="RIFAMPICINA, TABLETA DE 300 MG."]
tbdistr[Product == "RIFAMPICINA 100MG/5ML SUSPENSION FRASCO 120ML" , Product := "RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML."]
tbdistr[Product == "ISONIAZIDA, TAB LETA DE 300 MG.", Product:="ISONIAZIDA, TABLETA DE 300 MG."]
tbdistr[Product == "PIRAZINAMIDA, TABLET DE 500 MG.", Product:="PIRAZINAMIDA, TABLETA DE 500 MG."]
tbdistr[Product == "ESTREPTOMICINA, VIAL/FRSCO DE 1 GRAMO", Product:= "ESTREPTOMICINA, VIAL/FRASCO DE 1 GRAMO"]
unique(tbdistr$Product)

tbnots = read.csv("PCE/Outcome Measurement Data/TUBERCULOSIS/GTM - TB notifications 2012-2017.csv")
tbnots = data.table(tbnots)

tbnots[, sum(is.na(as.double(PESOKG)))/.N, by=floor(YearMonth/100) ]
tbnots[, sum(is.na(as.double(PESOLBS)))/.N, by=floor(YearMonth/100) ]
# PESOLBS has less NAs. 
# People weighting more than 25KG are considered adults.

# Exploring some variables:
# Lets look at classification 
table(str_to_lower(trimws(tbnots$CLASIFICACION)))
table(str_to_lower(trimws(tbnots$ESQUEMA)))
table(str_to_lower(trimws(tbnots$CONTACTOS)), tbnots$YEAR)
table(str_to_lower(trimws(tbnots$CONDICIONEGRESO)), tbnots$YEAR)
table(str_to_lower(trimws(tbnots$NUEVACONDICIONINGRESO)), tbnots$YEAR)
table(str_to_lower(trimws(tbnots$CONDICIONINGRESO)), tbnots$YEAR)
table(str_to_lower(trimws(tbnots$CONDICIONPX)), tbnots$YEAR)

table(str_to_lower(trimws(tbnots$CLASIFICACION)), str_to_lower(trimws(tbnots$YEAR)), useNA = "always")
table(str_to_lower(trimws(tbnots$RESISTENCIA)), tbnots$YEAR)

# Use "ESQUEMA" column to match medicine demand.

# ESQUEMA A: Treatment for adulst:
# 310 x RIFAMPICINA, TABLETA DE 300 MG.
# 155 x ISONIAZIDA, TABLETA DE 300 MG.
# 150 x PIRAZINAMIDA, TABLETA DE 500 MG.
# 150 x ETAMBUTOL, TABLETA DE 400 MG.

# ESQUEM PEDIATRICO: Treatment for kids: 
# 10 x PIRAZINAMIDA, TABLETA DE 500 MG.
# 16 x RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML.
# 310 x ISONIAZIDA, TABLETA DE 100 MG.

# Quimio profilaxis:  180 x ISONIAZIDA, TABLETA DE 300 MG.   x Contacto

# Exploring schemes column. This appear to be treatment schemes.
esquemas = dcast.data.table(tbnots[, 
              .(Count = .N), by= .(Year = floor(YearMonth/100), esquema = str_replace(str_to_upper(trimws(ESQUEMA)), "Á", "A") )],
                            Year ~ esquema, value.var = "Count" )

plot_data = merge(esquemas[, .(Year, Demand = PEDIATRICO*16)], 
      tbdistr[Product == "RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML.", .(Distributed = sum(Amount, na.rm=T)), 
              by = .(Year) ], by= "Year", all=T,
)[, .(Year, Demand, Distributed)]
plot_data[is.na(plot_data)] = 0
ggplot(data = melt(
    plot_data,
    id.vars = "Year"
)) + geom_col(aes(x = factor(Year), y = value, fill=variable), position = "dodge") + 
    labs(y="Rifampizine doses", title="TB medicine distribution versus demand.\nChildren fist-line drug.", subtitle="16 x RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML.", x = "Year\n\nMedicine distr. from 2013 up to June 2018.\nCases notifications from 2012 to 2017")  +
    scale_fill_manual(name="", labels = c("Children Rifampizine doses demand", 
          "Children Rifampizine doses \ndistributedby PNT"), values = c("#4466DD", "#77AA33")) +
    geom_line(aes(x = factor(Year), y = cumsum( Demand-Distributed), group = 1, color="#AA6600"), 
              data = plot_data[2:6], size = 1.5) + 
    scale_color_manual(values = "#AA6600", labels= "Cumulative gap", name="") +
    theme(text = element_text(size=12))


plot_data = merge(esquemas[, .(Year, Demand = A*310)], 
                  tbdistr[Product == "RIFAMPICINA, TABLETA DE 300 MG.", .(Distributed = sum(Amount, na.rm=T)), 
                          by = .(Year) ], by= "Year", all = T
)[, .(Year, Demand, Distributed)]
plot_data[is.na(plot_data)] = 0
ggplot(data = melt(
    plot_data,
    id.vars = "Year"
)) + geom_col(aes(x = factor(Year), y = value, fill=variable), position = "dodge") + 
    labs(y="Rifampizine doses", title="TB medicine distribution versus demand.\nAdult first-line drug", subtitle="310 x RIFAMPICINA, TABLETA DE 300 MG.", x = "Year\n\nMedicine distr. from 2013 up to June 2018.\nCases notifications from 2012 to 2017")  +
    scale_fill_manual(name="", labels = c("Adult Rifampizine doses demand", 
                                          "Adult Rifampizine doses \ndistributedby PNT"), 
                      values = c("#4466DD", "#77AA33")) +
    geom_line(aes(x = factor(Year), y = cumsum( Demand-Distributed), group = 1, color="#AA6600"), 
              data = plot_data[2:6], size=1.5) + 
    scale_color_manual(values = "#AA6600", labels= "Cumulative gap", name="")+
    theme(text = element_text(size=12))


# Isoniazide
isoData = merge(esquemas[, .(Year, Demand = A*155)], 
          tbdistr[Product == "ISONIAZIDA, TABLETA DE 300 MG.", .(Distributed = sum(Amount, na.rm=T)), 
                  by = .(Year) ], by= "Year", all = T
    )
isoData = merge(isoData, tbnots[CONTACTOS=="quimio", .(Contacts = .N*180), by= .(Year = floor(YearMonth/100))],
                by = "Year", all=T)

isoData[, Demand_ := Demand + Contacts]
isoData$Year = factor(isoData$Year)
ggplot(data = melt(isoData[,c("Demand_", "Distributed", "Year")], id.vars = c("Year"), measure.vars = c("Demand_", "Distributed")) ) + geom_col(aes(x = Year, y = value, fill=variable), position = "dodge") + 
    labs(y="Isoniazid doses", title="TB medicine distribution versus demand.", subtitle="155 x ISONIAZIDA, TABLETA DE 300 MG.", x="Year\n\nMedicine distr. from 2013 up to June 2018.\nCases notifications from 2012 to 2017")  +
    scale_fill_manual(name="", labels = c("Adult Isoniazid doses demand", 
                                          "Adult Isoniazid doses \ndistributedby PNT"), 
                      values = c("#4466DD", "#77AA33")) +
    geom_line(aes(x = factor(Year), y = cumsum( Demand_-Distributed), group = 1, color="#AA6600"), 
              data = isoData[2:6], size=1.5) + 
    scale_color_manual(values = "#AA6600", labels= "Cumulative gap", name="")+
    theme(text = element_text(size=12))

tbdistr[Year ==  2017 & Medicine == "ISONIAZIDA", sum(Amount), by = .(Product, Year
                                                                      )]
# Adult new cases:
nadults = tbnots[ ( #(as.double(PESOLBS) > 55*2.2) | 
                     (EDAD >= 18)) & str_to_lower(trimws(CLASIFICACION)) != "bk negativo" &
          (CONDICIONINGRESO == "nuevo" | is.na(CONDICIONINGRESO)), .N, by= .(COD_DEPT, YearMonth)]
nkids = tbnots[ ( # (as.double(PESOLBS) <= 55*2.2) | 
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

esquemas[, .(year, PEDIATRICO*16)]

table(tbnots$CONTACTOS, tbnots$YEAR)
