#requires fabsbase

da.obs <- da.wide.habitat.env
da.obs <- da.obs[1:(which(colnames(da.obs) == "p.d5"))]
da.obs %<>%
  group_by(site, year, month)
da.obs <- summarise_all(da.obs, funs(mean))
da.obs <- merge(da.obs, habitat, by = "site")

#filter
da.wide.habitat.env.july2014 <- filter(da.obs, 
                                       year == "2015", 
                                       month == "Jul")

da.wide.habitat.env.july2014 <- column_to_rownames(da.wide.habitat.env.july2014, 
                                                   var = "site")

ef <- envfit(nmds.july2014, da.wide.habitat.env.july2014[,c("t.a0", "exposure", "s.a0","subtidal_primary_cover")])
ef



nmds.july2014 <- metaMDS(da.wide.habitat.env.july2014[(which(colnames(da.wide.habitat.env.july2014) == "argo")) :
                                                        (which(colnames(da.wide.habitat.env.july2014) == "yero"))],
                         k=3)

plot(nmds.july2014)
plot(ef)
points(nmds.july2014, 
       col="black" )
text(nmds.july2014, "site")
text(nmds.july2014, "species", col = "darkgreen")

legend("topright", levels(da.wide.habitat.env.july2014$exposure), 
       pch=1:(length(levels(da.wide.habitat.env.july2014$exposure))), 
       lty=1:(length(levels(da.wide.habitat.env.july2014$exposure))))

ordihull(nmds.july2014, da.wide.habitat.env.july2014$exposure, 
         scaling = "symmetric",
         lty=(as.integer(da.wide.habitat.env.july2014$exposure))) 


