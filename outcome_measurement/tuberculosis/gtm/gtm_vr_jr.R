############################################
# Explore the raw Guatemala mortality data #
# J. Ross                                  #
# July 12, 2018                            #
############################################

library(foreign)

mort<-read.spss(file= "J:/DATA/GTM/VITAL_STATISTICS/2014/GTM_VR_2014_DEATHS_Y2016M09D15.SAV", to.data.frame = TRUE)

mort$Caudef<-as.factor(mort$Caudef)
mort1<-mort[,mort$Caudef=="B200"]


levels(mort$Caudef)
typeof(mort1$Caudef)
