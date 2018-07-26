ml17 = read.csv("PCE/Outcome Measurement Data/MALARIA/Distribucion de MTILD/BD 2017/BD MTILD Escuintla P13/EntregaPIIs.csv")
ml17d = read.csv("PCE/Outcome Measurement Data/MALARIA/Distribucion de MTILD/BD 2017/BD MTILD Escuintla P13/EntregaPIIsDetalle.csv")
pabellones = data.table(merge(ml17d[,c("Pabellones", "CodBoleta")], ml17[,c("CodBoleta", "FechaEnt")], by="CodBoleta"))
pabellones2 = pabellones[, sum(Pabellones), by= .(YearMonth = str_sub(FechaEnt, 4, -1) ) ]
pabellones2[,sum(V1), by = str_sub(YearMonth, 4, -1) ] [order(str_sub)]
