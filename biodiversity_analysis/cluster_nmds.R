# requires fabsbase.R block

#extract species observations and sites only
#summarise by site/month (analyses cannot account for day) 
#re-merge habitat data
da.obs <- da.wide.habitat.env
da.obs <- da.obs[1:(which(colnames(da.obs) == "p.d5"))]
da.obs %<>%
  group_by(site, year, month)
da.obs <- summarise_all(da.obs, funs(mean))
da.wide.habitat.env.july2014 <- merge(da.obs, habitat, by = "site")
da.wide.habitat.env.july2014 <- filter(da.wide.habitat.env.july2014, 
                                       year == "2015", 
                                       month == "Jul")
da.wide.habitat.env.july2014 <- column_to_rownames(da.wide.habitat.env.july2014, 
                                                   var = "site")

#bray curtis NMDS ordination
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

#bray-curtis dissimilarity matrix
distmatrix.july2014 <- metaMDSdist(da.wide.habitat.env.july2014[(which(colnames(da.wide.habitat.env.july2014) == "argo")) :
                                           (which(colnames(da.wide.habitat.env.july2014) == "yero"))], distance = "bray", autotransform = TRUE,
            noshare = TRUE, trace = 1, zerodist = "ignore",
            distfun = vegdist)
#cluster analysis and plotting
clusters.july2014average <- hclust(distmatrix.july2014, "average")

#group branches of tree from hclust
plot(clusters.july2014average)
grp <- cutree(clusters.july2014average, 9)
grp
plot(grp)

forest <- spantree(distmatrix.july2014, toolong = 1)
plot(forest, pch=21, col = "black", bg = "yellow", type = "t")


vegemite(da.wide.habitat.env.july2014[(which(colnames(da.wide.habitat.env.july2014) == "argo")) :
                                        (which(colnames(da.wide.habitat.env.july2014) == "yero"))], scale = "log")

#data(dune)
#data(dune.env)
#dune.dist <- vegdist(dune)
dune.ano <- with(da.wide.habitat.env.july2014, anosim(distmatrix.july2014, subtidal_primary_substrate))
summary(dune.ano)
plot(dune.ano)

#adonis - permanova
permveg <- adonis(distmatrix.july2014 ~ subtidal_primary_macroveg * s.a0 * exposure, data = da.wide.habitat.env.july2014)
permveg
densityplot(permustats(permveg))

plot(habitat)

#search for colinearity
ysi1 <- da.wide.habitat.env.july2014[99:108]
ggpairs(ysi1[1:10])
temp.corr <- cor(ysi1[1:10])

symnum(temp.corr)
par(temp.corr)
pairs(temp.corr)

#not functioning
env.o <- order(ysi1)
op <- par(mfrow=c(1,1), pty="s")
pairs(ysi, lower.panel=panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main="Pearson Correlation Matrix")
pairs(ysi1, main="Pearson Correlation Matrix")


#simper - similarity percentages
sim <- with(da.wide.habitat.env.july2014, simper(da.wide.habitat.env.july2014[(which(colnames(da.wide.habitat.env.july2014) == "argo")) :
                                                                                        (which(colnames(da.wide.habitat.env.july2014) == "yero"))], exposure))
summary(sim)



##Indicator species
#function to remove species columns with no counts
remove_zero_cols <- function(df) {
  rem_vec <- NULL
  for(i in 1:ncol(df)){
    this_sum <- summary(df[,i])
    zero_test <- length(which(this_sum == 0))
    if(zero_test == 6) {
      rem_vec[i] <- names(df)[i]
    }
  }
  features_to_remove <- rem_vec[!is.na(rem_vec)]
  rem_ind <- which(names(df) %in% features_to_remove)
  df <- df[,-rem_ind]
  return(df)
}

#remove zeros for indval
da.zero.remove <- da.wide.habitat.env.july2014[(which(colnames(da.wide.habitat.env.july2014) == "argo")) :
                                                 (which(colnames(da.wide.habitat.env.july2014) == "yero"))] %>%
  remove_zero_cols()

#dissimilarity matrix
dis.bc <- dsvdis(da.zero.remove,'bray/curtis')

#indicator species
iva <- indval(da.zero.remove,grp)
iva

#generate dataframe of significant driver species
gr <- iva$maxcls[iva$pval<=0.1]
iv <- iva$indcls[iva$pval<=0.1]
pv <- iva$pval[iva$pval<=0.1]
fr <- apply(da.zero.remove>0, 2, sum) [iva$pval<=0.1]
fidg <- data.frame(group=gr, indval=iv,pvalue=pv, freq=fr)
fidg



sites.survey <- da.wide$site
years.survey <- da.wide$year
survey.summary <- merge(years.survey, sites.survey)
spread(survey.summary, "year", "year")



