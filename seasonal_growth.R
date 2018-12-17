


#by month
dl.year.month.site <- dl %>%
   group_by(month, year, site, species, stage) %>%
  summarise(se = sd(length)/sqrt((length(species))), length = mean(length))

#remove year; just seasonality
dl.month.site <- dl %>%
  group_by(month, site, species, stage) %>%
  summarise(se = sd(length)/sqrt((length(species))), length = mean(length))

seasonalgrowth <- function(Species, Stage, Length, Year){
 pasa.length <- filter(dl.year.month.site, 
                         species == Species,
                          stage == Stage,
                          length < Length,
                          year == Year,
                           site == "ssp" | 
                           site == "hdo" | 
                           site == "pba" | 
                           site == "chp" | 
                           site == "ppo" | 
                           site == "wfb" |
                           site == "koe3"| 
                           site == "gog4"| 
                           site == "fei3"| 
                           site == "fei4"| 
                           site == "hab4"| 
                           site == "hab2"| 
                           site == "fan3"| 
                           site == "fan1"| 
                           site == "kis1"| 
                           site == "kis2"| 
                           site == "sni1"| 
                           site == "sni2"| 
                           site == "ris1"| 
                           site == "ris3"
)
   
   ggplot(pasa.length) +
    aes(x = month, y = length, colour = factor(site), group = site) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_line(position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = length - se,
                      ymax = length + se), 
                  width = 0,
                  size = 1,
                  position = position_dodge(width = 0.25)) +
    theme_bw() +
    theme(axis.text.x      = element_text(size = 15, angle = 45, hjust = 1),
          #legend.position  = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text             = element_text(size = 13, vjust = 0.1)) +
    labs(x = "Month", y = expression("Mean Length"), colour = "site", title = Species, subtitle = Year)
}

seasonalgrowth("enso", 3, 150, 2018)

dev.copy(png,'enso.png')
dev.off()
