#multiple cohorts

#create avgerage length variable and SE of length; filter core sites
#same as above but for multiple cohorts(shpe)
dl.year.month.core.cohorts <- dl
dl.year.month.core.cohorts <-
  #filter(dl.year.month.core.cohorts, site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb") %>%
  group_by(dl.year.month.core.cohorts, year, month, species, stage) %>%
  summarise(se = sd(length)/sqrt((length(species))), length = mean(length))

#seasonal mean length function for any species with standard error
#only useful for species with single cohorts; doesn't work for pipefish, perch, herring, etc
mean_length_seasonal_cohorts <- function(Species, Stage) {
  filter(dl.year.month.core.cohorts, species == Species, stage == Stage) %>%
    ggplot() +
    aes(x = month, y = length, colour = factor(year), group = year) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_line(position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = length - se,
                      ymax = length + se), 
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
}
#execute above function
#only useful for species with single cohorts; doesn't work for pipefish, perch, herring, etc
mean_length_seasonal_cohorts("shpe", 1)
