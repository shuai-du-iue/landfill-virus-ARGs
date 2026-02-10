setwd("D:/01科研-城环所/01科研文章/垃圾填埋场-病毒/垃圾填埋场病毒ARGs/4env/vpa")
library(vegan)

Host<-read.csv("Host.csv",row.names=1);Host<-t(Host)
mOTUs<-read.csv("mOTUs.csv",row.names=1);mOTUs<-t(mOTUs)
Virus_host<-read.csv("Virus_host.csv",row.names=1);Virus_host<-t(Virus_host)
vOTUs<-read.csv("vOTUs.csv",row.names=1);vOTUs<-t(vOTUs)
Viral_ARGs<-read.csv("Viral_ARGs.csv",row.names=1);Viral_ARGs<-t(Viral_ARGs)
Virus_ARGs<-read.csv("Virus_ARGs.csv",row.names=1);Virus_ARGs<-t(Virus_ARGs)
geo_env<-read.csv("geo_env.csv",row.names=1)

mod1<-varpart(Host,
             ~Longitude+Latitude+Elevation,
             ~Temperature+Precipitation+Humidity+pH+DOC+TN+NH4+Cl+SO4,
             data=geo_env)
plot(mod1,bg=2:4,Xnames=c("Geo","Env"))
summary(mod1)

mod2<-varpart(mOTUs,
              ~Longitude+Latitude+Elevation,
              ~Temperature+Precipitation+Humidity+pH+DOC+TN+NH4+Cl+SO4,
              data=geo_env)
plot(mod2,bg=2:4,Xnames=c("Geo","Env"))
summary(mod2)

mod3<-varpart(Virus_host,
              ~Longitude+Latitude+Elevation,
              ~Temperature+Precipitation+Humidity+pH+DOC+TN+NH4+Cl+SO4,
              data=geo_env)
plot(mod3,bg=2:4,Xnames=c("Geo","Env"))
summary(mod3)

mod4<-varpart(vOTUs,
              ~Longitude+Latitude+Elevation,
              ~Temperature+Precipitation+Humidity+pH+DOC+TN+NH4+Cl+SO4,
              data=geo_env)
plot(mod4,bg=2:4,Xnames=c("Geo","Env"))
summary(mod4)

mod5<-varpart(Viral_ARGs,
              ~Longitude+Latitude+Elevation,
              ~Temperature+Precipitation+Humidity+pH+DOC+TN+NH4+Cl+SO4,
              data=geo_env)
plot(mod5,bg=2:4,Xnames=c("Geo","Env"))
summary(mod5)

mod6<-varpart(Virus_ARGs,
              ~Longitude+Latitude+Elevation,
              ~Temperature+Precipitation+Humidity+pH+DOC+TN+NH4+Cl+SO4,
              data=geo_env)
plot(mod6,bg=2:4,Xnames=c("Geo","Env"))
summary(mod6)

Longitude+Latitude+Elevation
Temperature+Precipitation+Humidity+pH+DOC+TN+NH4+Cl+SO4
