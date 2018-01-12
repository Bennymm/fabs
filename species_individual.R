

#this block of code presents Seasonal abundance for several of the more common species
#run fabsbase.R first to load packages, import all data and perform some basic reformatting

#abundance for individual species is calculated as the total catch for a site visit(seine sets are lumped together)
#error bars are standard error

da.long.year.month.core <- da.long %>%
  filter(site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb") %>%
  group_by(year, month, species) %>%
  summarise(se = sd(abundance)/sqrt((length(species))), abundance = mean(abundance))

#flatfish seasonal
filter(da.long.year.month.core, species == "enso" | species == "saso" |species == "pasa") %>%
  filter(year != "2018") %>%
  filter(abundance > 0) %>%
  ggplot()  +
  aes(x = month, y = abundance, colour = factor(species, labels = c("English Sole", 
                                                                    "Sanddab", 
                                                                    "Sand Sole")), group = species) +
  geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
  geom_line( position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymin = abundance - se,
                    ymax = abundance + se), 
                width = 0,
                size = 1,
                position = position_dodge(width = 0.25))+
  facet_wrap(~year)+
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression("Flatfish Abundance" ~ "(count/site visit)"), colour = "Species")

#rockfish seasonal
filter(da.long.year.month.core, species == "byro" | species == "cqbr" | species == "unro" | species == "coro" | species == "blro"
       | species == "caro" | species == "boro" | species == "qbro" | species == "vero" | species == "yero") %>%
  filter(year != "2018") %>%
  filter(abundance > 0) %>%
  ggplot() +
  aes(x = month, y = abundance, colour = factor(species, labels = c("Black",
                                                                    "Boccacio",
                                                                    "Black/Yellow",
                                                                    "Canary",
                                                                    "Copper",
                                                                    "Copper/Quillback",
                                                                    "Quillback",
                                                                    "Unknown",
                                                                    "Vermillion",
                                                                    "Yellowtail")), group = species) +
  geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
  geom_line( position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymin = abundance - se,
                    ymax = abundance + se), 
                width = 0,
                size = 1,
                position = position_dodge(width = 0.25))+
  facet_wrap(~year)+
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression("Rockfish Abundance" ~ "(count/site visit)"), colour = "Species")

#rockfish seasonal; byro excluded
filter(da.long.year.month.core, species == "cqbr" | species == "unro" | species == "coro" | species == "blro"
       | species == "caro" | species == "boro" | species == "qbro" | species == "vero" | species == "yero") %>%
  filter(year != "2018") %>%
  filter(abundance > 0) %>%
  ggplot() +
  aes(x = month, y = abundance, colour = factor(species, labels = c("Black",
                                                                    "Boccacio",
                                                                    "Canary",
                                                                    "Copper",
                                                                    "Copper/Quillback",
                                                                    "Quillback",
                                                                    "Unknown",
                                                                    "Vermillion",
                                                                    "Yellowtail")), group = species) +
  geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
  geom_line( position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymin = abundance - se,
                    ymax = abundance + se), 
                width = 0,
                size = 1,
                position = position_dodge(width = 0.25))+
  facet_wrap(~year)+
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression("Rockfish Abundance" ~ "(count/site visit)"), colour = "Species")

#rockfish seasonal; byro, cqbr, yellowtail excluded
filter(da.long.year.month.core, species == "unro" | species == "coro" | species == "blro"
       | species == "caro" | species == "boro" | species == "qbro" | species == "yero") %>%
  filter(year != "2018") %>%
  filter(abundance > 0) %>%
  ggplot() +
  aes(x = month, y = abundance, colour = factor(species, labels = c("Black",
                                                                    "Boccacio",
                                                                    "Canary",
                                                                    "Copper/Quillback",
                                                                    "Quillback",
                                                                    "Unknown",
                                                                    "Vermillion")), group = species) +
  geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
  geom_line( position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymin = abundance - se,
                    ymax = abundance + se), 
                width = 0,
                size = 1,
                position = position_dodge(width = 0.25))+
  facet_wrap(~year)+
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression("Rockfish Abundance" ~ "(count/site visit)"), colour = "Species")

#the following is for less frequent species(indicator species)
abundance_seasonal <- function(Species){
  filter(da.long.year.month.core, species == Species) %>%
    filter(year != "2018") %>%
    filter(abundance > 0) %>%
    ggplot() +
    aes(x = month, y = abundance, colour = species, group = species) +
    geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
    geom_line( position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = abundance - se,
                      ymax = abundance + se), 
                  width = 0,
                  size = 1,
                  position = position_dodge(width = 0.25))+
    facet_wrap(~year)+
    theme_bw() +
    theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
          legend.position  = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text             = element_text(size = 13, vjust = 0.1)) +
    labs(x = "Month", y = expression(Abundance ~ "(mean count/site visit)"))
}

#normal
abundance_seasonal("tisc")
lfq_year_month("tisc", 100)
#normal
abundance_seasonal("busc")
lfq_year_month("busc", 150)
#normal
abundance_seasonal("thst")
lfq_year_month("thst", 1000)
#normal
abundance_seasonal("stsc")
lfq_year_month("stsc", 1000)
#not enough data
abundance_seasonal("blgo")
lfq_year_month("blgo", 1000)
#normal
abundance_seasonal("bago")
lfq_year_month("bago", 1000)

#do seasonal abundance also
#reduced numbers through time
abundance_seasonal("crgu")
lfq_year_month("crgu", 1000)
#potential misidentification
abundance_seasonal("pegu")
lfq_year_month("pegu", 1000)
#reduced numbers through time
abundance_seasonal("grsc")
lfq_year_month("grsc", 1000)
#reduced numbers through time
abundance_seasonal("snpr")
lfq_year_month("snpr", 1000)
#reduced numbers through time
abundance_seasonal("sisc")
lfq_year_month("sisc", 150)
