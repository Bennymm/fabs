
# requires fabsbase.R block
#extract species observations and sites only
#summarise by site/month (analyses cannot account for day) 
#merge habitat data

#da.wide.habitat.env.dup <- da.wide.habitat.env
#da.wide.habitat.env.dup <- da.wide.habitat.env.dup[1:108]
#da.wide.habitat.env.dup%<>%
#  group_by(site, year, month)
#da.wide.habitat.env.dup <- summarise_all(da.wide.habitat.env.dup, funs(mean))
#da.wide.habitat.env.dup <- merge(da.wide.habitat.env.dup, habitat, by = "site")


da.obs <- da.wide.habitat.env
da.obs <- da.obs[1:(which(colnames(da.obs) == "yero"))]
da.obs%<>%
  group_by(site, year, month)
da.obs <- summarise_all(da.obs, funs(mean))
da.obs <- merge(da.obs, habitat, by = "site")

#2014 July NMDS
da.wide.habitat.env.july2014 <- filter(da.obs, 
                                       year == "2014", 
                                       month == "Jul")
da.wide.habitat.env.july2014 <- column_to_rownames(da.wide.habitat.env.july2014, 
                                                   var = "site")
nmds.july2014 <- metaMDS(da.wide.habitat.env.july2014[(which(colnames(da.wide.habitat.env.july2014) == "argo")) :
                                                      (which(colnames(da.wide.habitat.env.july2014) == "yero"))],
                         k=3)
ordiplot(nmds.july2014, 
         display="site", 
         type="n",
         xlim = c(-2, 2))
points(nmds.july2014, 
       col="black", 
       pch = (as.integer(da.wide.habitat.env.july2014$veg_subst))) 
ordihull(nmds.july2014, da.wide.habitat.env.july2014$veg_subst, 
         scaling = "symmetric",
         lty=(as.integer(da.wide.habitat.env.july2014$veg_subst))) 
legend("topright", levels(da.wide.habitat.env.july2014$veg_subst), 
       pch=1:(length(levels(da.wide.habitat.env.july2014$veg_subst))), 
       lty=1:(length(levels(da.wide.habitat.env.july2014$veg_subst))))

#2015 July NMDS
da.wide.habitat.env.july2015 <- filter(da.obs, 
                                       year == "2017", 
                                       month == "Jul")
da.wide.habitat.env.july2015 <- column_to_rownames(da.wide.habitat.env.july2015, 
                                                   var = "site")
nmds.july2015 <- metaMDS(da.wide.habitat.env.july2015[(which(colnames(da.wide.habitat.env.july2015) == "argo")) :
                                                      (which(colnames(da.wide.habitat.env.july2015) == "yero"))], 
                         k=3)
ordiplot(nmds.july2015, 
         display="site", 
         type="n",
         xlim = c(-2, 2))
points(nmds.july2015, 
       col="black", 
       pch = (as.integer(da.wide.habitat.env.july2015$veg_subst))) 
ordihull(nmds.july2015, da.wide.habitat.env.july2015$veg_subst, 
         scaling = "symmetric",
         lty=(as.integer(da.wide.habitat.env.july2015$veg_subst))) 
legend("topright", levels(da.wide.habitat.env.july2015$veg_subst), 
       pch=1:(length(levels(da.wide.habitat.env.july2015$veg_subst))), 
       lty=1:(length(levels(da.wide.habitat.env.july2015$veg_subst))))

#2016 July NMDS
da.wide.habitat.env.july2016 <- filter(da.obs, 
                                       year == "2017", 
                                       month == "Jul")
da.wide.habitat.env.july2016 <- column_to_rownames(da.wide.habitat.env.july2016, 
                                                   var = "site")
nmds.july2016 <- metaMDS(da.wide.habitat.env.july2016[(which(colnames(da.wide.habitat.env.july2016) == "argo")) :
                                                      (which(colnames(da.wide.habitat.env.july2016) == "yero"))], 
                         k=3)
ordiplot(nmds.july2016, 
         display="site", 
         type="n",
         xlim = c(-2, 2))
points(nmds.july2016, 
       col="black", 
       pch = (as.integer(da.wide.habitat.env.july2016$veg_subst))) 
ordihull(nmds.july2016, da.wide.habitat.env.july2016$veg_subst, 
         scaling = "symmetric",
         lty=(as.integer(da.wide.habitat.env.july2016$veg_subst))) 
legend("topright", levels(da.wide.habitat.env.july2016$veg_subst), 
       pch=1:(length(levels(da.wide.habitat.env.july2016$veg_subst))), 
       lty=1:(length(levels(da.wide.habitat.env.july2016$veg_subst))))

#2017 July NMDS
da.wide.habitat.env.july2017 <- filter(da.obs, 
                                       year == "2017", 
                                       month == "Jul")
da.wide.habitat.env.july2017 <- column_to_rownames(da.wide.habitat.env.july2017, 
                                                   var = "site")
nmds.july2017 <- metaMDS(da.wide.habitat.env.july2017[(which(colnames(da.wide.habitat.env.july2017) == "argo")) :
                                                      (which(colnames(da.wide.habitat.env.july2017) == "yero"))], 
                         k=3)
ordiplot(nmds.july2017, 
         display="sites", 
         type="n",
         xlim = c(-2, 2))
points(nmds.july2017, 
       col="black", 
       pch = (as.integer(da.wide.habitat.env.july2017$veg_subst))) 
ordihull(nmds.july2017, da.wide.habitat.env.july2017$veg_subst, 
         scaling = "symmetric", 
         lty=(as.integer(da.wide.habitat.env.july2017$veg_subst))) 
legend("topright", levels(da.wide.habitat.env.july2017$veg_subst), 
       pch=1:(length(levels(da.wide.habitat.env.july2017$veg_subst))), 
       lty=1:(length(levels(da.wide.habitat.env.july2017$veg_subst)))) 



#NMDS by year;summer months(June, July, August); all habitats
da.wide.habitat.env.trial <- filter(da.obs,
                                       month == "Jun" | month == "Jul" | month == "Aug",
                                       site == "chp" | site == "fan1" | site == "fan3" | site == "gog1" | site == "hdp" |
                                       site == "kis2" | site == "pba" | site == "ppo" | site == "sni1" | site == "sni2" |
                                       site == "ssp" | site == "wfb")
da.wide.habitat.env.trial$year <- as.factor(da.wide.habitat.env.trial$year)
nmds.trial <- metaMDS(da.wide.habitat.env.trial[(which(colnames(da.wide.habitat.env.trial) == "argo")) :
                                                  (which(colnames(da.wide.habitat.env.trial) == "yero"))], 
                         k=3)
ordiplot(nmds.trial, 
         display="site", 
         type="n") 
points(nmds.trial, 
       col="black", 
       pch = (as.integer(da.wide.habitat.env.trial$year))) 
ordihull(nmds.trial, da.wide.habitat.env.trial$year, 
         scaling = "symmetric",
         lty=(as.integer(da.wide.habitat.env.trial$year))) 
legend("topright", levels(da.wide.habitat.env.trial$year), 
       pch=1:(length(levels(da.wide.habitat.env.trial$year))), 
       lty=1:(length(levels(da.wide.habitat.env.trial$year))))


##NMDS by year; summer months(June, July, August); all seagrass
da.wide.habitat.env.trial <- filter(da.obs,
                                    month == "Jun" | month == "Jul" | month == "Aug",
                                    site == "chp" | site == "fan1" | site == "fan3" | site == "gog1" | site == "hdp" |
                                    site == "kis2" | site == "pba" | site == "ppo" | site == "sni1" | site == "sni2" |
                                    site == "ssp" | site == "wfb", 
                                    veg_subst == "zostera.mud" | veg_subst == "zostera.sand" | veg_subst == "zostera,gravel")
da.wide.habitat.env.trial$year <- as.factor(da.wide.habitat.env.trial$year)
nmds.trial <- metaMDS(da.wide.habitat.env.trial[(which(colnames(da.wide.habitat.env.trial) == "argo")) :
                                                  (which(colnames(da.wide.habitat.env.trial) == "yero"))], 
                      k=3)
ordiplot(nmds.trial, 
         display="site", 
         type="n")
points(nmds.trial, 
       col="black", 
       pch = (as.integer(da.wide.habitat.env.trial$year))) 
ordihull(nmds.trial, da.wide.habitat.env.trial$year, 
         scaling = "symmetric",
         lty=(as.integer(da.wide.habitat.env.trial$year))) 
legend("topright", levels(da.wide.habitat.env.trial$year), 
       pch=1:(length(levels(da.wide.habitat.env.trial$year))), 
       lty=1:(length(levels(da.wide.habitat.env.trial$year))))

##NMDS by year; summer months(June, July, August); sheltered seagrass
da.wide.habitat.env.trial <- filter(da.obs,
                                    month == "Jun" | month == "Jul" | month == "Aug",
                                    site == "chp" | site == "fan1" | site == "fan3" | site == "gog1" | site == "hdp" |
                                      site == "kis2" | site == "pba" | site == "ppo" | site == "sni1" | site == "sni2" |
                                      site == "ssp" | site == "wfb", 
                                    veg_subst == "zostera.mud")
da.wide.habitat.env.trial$year <- as.factor(da.wide.habitat.env.trial$year)
nmds.trial <- metaMDS(da.wide.habitat.env.trial[(which(colnames(da.wide.habitat.env.trial) == "argo")) :
                                                  (which(colnames(da.wide.habitat.env.trial) == "yero"))], 
                      k=3)
ordiplot(nmds.trial, 
         display="site", 
         type="n")
points(nmds.trial, 
       col="black", 
       pch = (as.integer(da.wide.habitat.env.trial$year))) 
ordihull(nmds.trial, da.wide.habitat.env.trial$year, 
         scaling = "symmetric",
         lty=(as.integer(da.wide.habitat.env.trial$year))) 
legend("topright", levels(da.wide.habitat.env.trial$year), 
       pch=1:(length(levels(da.wide.habitat.env.trial$year))), 
       lty=1:(length(levels(da.wide.habitat.env.trial$year))))

##NMDS by year; summer months(June, July, August); exposed seagrass
da.wide.habitat.env.trial <- filter(da.obs,
                                    month == "Jun" | month == "Jul" | month == "Aug",
                                    site == "chp" | site == "fan1" | site == "fan3" | site == "gog1" | site == "hdp" |
                                      site == "kis2" | site == "pba" | site == "ppo" | site == "sni1" | site == "sni2" |
                                      site == "ssp" | site == "wfb", 
                                    veg_subst == "zostera.sand" | veg_subst == "zostera.gravel")
da.wide.habitat.env.trial$year <- as.factor(da.wide.habitat.env.trial$year)
nmds.trial <- metaMDS(da.wide.habitat.env.trial[(which(colnames(da.wide.habitat.env.trial) == "argo")) :
                                                  (which(colnames(da.wide.habitat.env.trial) == "yero"))], 
                      k=3)
ordiplot(nmds.trial, 
         display="site", 
         type="n")
points(nmds.trial, 
       col="black", 
       pch = (as.integer(da.wide.habitat.env.trial$year))) 
ordihull(nmds.trial, da.wide.habitat.env.trial$year, 
         scaling = "symmetric",
         lty=(as.integer(da.wide.habitat.env.trial$year))) 
legend("topright", levels(da.wide.habitat.env.trial$year), 
       pch=1:(length(levels(da.wide.habitat.env.trial$year))), 
       lty=1:(length(levels(da.wide.habitat.env.trial$year))))

##NMDS by year; summer months(June, July, August); sandy habitat
da.wide.habitat.env.trial <- filter(da.obs,
                                    month == "Jun" | month == "Jul" | month == "Aug",
                                    site == "chp" | site == "fan1" | site == "fan3" | site == "gog1" | site == "hdp" |
                                      site == "kis2" | site == "pba" | site == "ppo" | site == "sni1" | site == "sni2" |
                                      site == "ssp" | site == "wfb", 
                                    veg_subst == "sand")
da.wide.habitat.env.trial$year <- as.factor(da.wide.habitat.env.trial$year)
nmds.trial <- metaMDS(da.wide.habitat.env.trial[(which(colnames(da.wide.habitat.env.trial) == "argo")) :
                                                  (which(colnames(da.wide.habitat.env.trial) == "yero"))], 
                      k=3)
ordiplot(nmds.trial, 
         display="site", 
         type="n")
points(nmds.trial, 
       col="black", 
       pch = (as.integer(da.wide.habitat.env.trial$year))) 
ordihull(nmds.trial, da.wide.habitat.env.trial$year, 
         scaling = "symmetric",
         lty=(as.integer(da.wide.habitat.env.trial$year))) 
legend("topright", levels(da.wide.habitat.env.trial$year), 
       pch=1:(length(levels(da.wide.habitat.env.trial$year))), 
       lty=1:(length(levels(da.wide.habitat.env.trial$year))))

#old way
#da.wide.habitat.env.july2017 <- filter(da.obs, 
#                                       month == "Jul",
#                                       veg_subst == "sand")
#da.wide.habitat.env.july2017 <- column_to_rownames(da.wide.habitat.env.july2017, var = "site")
#nmds.july2017 <- metaMDS(da.wide.habitat.env.july2017[5:98], k=3)
#stressplot(nmds.trial)
#ordiplot(nmds.july2017,type="n")
  #ordihull(nmds.july2017,groups=(c(da.wide.habitat.env.july2017$year)),draw="polygon",col="grey90",label=F)
  #orditorp(nmds.july2017,display="sites",col="black",air=0.05)
