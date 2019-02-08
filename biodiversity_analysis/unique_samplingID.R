# requires fabsbase.R block

#month name to numeric
da.wide$month <- factor(da.wide$month, 
                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                        labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

#filter summer, average multiple samples, 
da.unique.annual <- filter(da.wide,
                           month == "Jun" | month == "Jul" | month == "Aug") %>%
  group_by(site) %>%
  summarise_all(funs(mean))
da.unique.annual$uid <- paste(da.unique.annual$site,da.unique.annual$year)
da.unique.annual <- column_to_rownames(da.unique.annual,
                                       var = "site")

nmds.july2014 <- metaMDS(da.unique.annual[(which(colnames(da.wide.habitat.env.july2014) == "argo")) :
                                                        (which(colnames(da.wide.habitat.env.july2014) == "yero"))],
                         k=3)

nmds_plot <- function(data, habitat){
ordiplot(data, 
         display="site", 
         type="n",
         xlim = c(-2, 2))
points(nmds.july2014, 
       col="black", 
       pch = (as.integer(habitat$veg_subst))) 
ordihull(nmds.july2014, habitat$veg_subst, 
         scaling = "symmetric",
         lty=(as.integer(habitat$veg_subst))) 
legend("topright", levels(habitat$veg_subst), 
       pch=1:(length(levels(habitat$veg_subst))), 
       lty=1:(length(levels(habitat$veg_subst))))}

nmds_plot(nmds.july2014, da.wide.habitat.env.july2014)



