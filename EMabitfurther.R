#find normal distributions from a multimodal distribution
#provide approximate means of cohorts
#for year month species


dl.expand <- dl %>%
  complete(site, year, month, species)


#dl.expand$length[is.na(dl.expand$length)] <- 50


#define df for deposition of cohort delineations
#column.names <- c("year", "month", "species", "class", "lamda", "mu", "sigma")
cohorts.df <- data.frame()
cohorts.df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("year", "month", "site", "species", "class", "lamda", "mu", "sigma"))

#function
EMfunction2 <- function(Year, Site, Month, Species) {
  dl.shpe <- dl.expand %>%
    filter(year == Year, site == Site, month == Month, species == Species)
  
  #EMfunction3 <- function(XXX, na.rm=TRUE) {normalmixEM(XXX, lambda = .5, mu = c( 60, 90, 150), sigma = 5)}
  
  EMfunction1 <-  function(Year, Site, Month, Species) {
    shpe1 <- 
      #EMfunction3(dl.shpe$length)
    normalmixEM(dl.shpe$length, lambda = .5, mu = c( 60, 90, 150), sigma = 5)
    #shpe1 <- apply(dl.shpe, MARGIN = 2, FUN = EMfunction3, na.rm = TRUE)
    
    shpe1 <- shpe1[c("lambda", "mu", "sigma")]
    shpe1 <- as.data.frame(shpe1)
    shpe1 <- rowid_to_column(shpe1)
    colnames(shpe1)[colnames(shpe1)=="rowid"] <- "class"
    shpe1$year <- Year
    shpe1$site <- Site
    shpe1$month <- Month
    shpe1$species <- Species
    shpe1 <<- shpe1
    cohorts.df <<- do.call("rbind", list(shpe1, cohorts.df))
  }
  EMfunction1(Year, Site, Month, Species) 
}

#works
for(month in c("Jul", "Aug")){for(i in 2014:2018){EMfunction2( i, "pba", month, "shpe")}}

ggplot(cohorts.df) +
  aes(x = month, y = mu, colour = factor(year), group = year) +
  geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymin = mu - sigma,
                    ymax = mu + sigma), 
                width = 0,
                size = 1,
                position = position_dodge(width = 0.25)) +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression("Mean Length"), colour = "year")

