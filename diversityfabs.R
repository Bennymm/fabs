
#new column; shannon H
da.wide.habitat.env$shan <- diversity(
  da.wide.habitat.env[(which(colnames(da.wide.habitat.env) == "argo")) : 
                      (which(colnames(da.wide.habitat.env) == "yero"))],
  index = "shannon")

#new column; specnumber
da.wide.habitat.env$abundance<- specnumber(
  da.wide.habitat.env[(which(colnames(da.wide.habitat.env) == "argo")) :
                      (which(colnames(da.wide.habitat.env) == "yero"))])

#filter for core sites
#filter(site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb")

specaccum(da.wide.habitat.env[
  (which(colnames(da.wide.habitat.env) == "argo")) : 
  (which(colnames(da.wide.habitat.env) == "yero"))]) %>%
plot(ci.type="polygon", ci.col="lightyellow", xlab = "site visits", ylab = "species number")


#diversity(shannons and spp.richness) by site and month; all sites
da.month.diversity<- (group_by(da.wide.habitat.env, month, site))%>%
  summarise_at(vars(shan:abundance), mean)
#abundance
ggplot(da.month.diversity) +
  aes(x = month, y = abundance, colour = site, group = site) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "Month", y = expression("Richness" ~ "(species count/site visit)"))

#shannons
ggplot(da.month.diversity) +
  aes(x = month, y = shan, colour = site, group = site) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression("Shannon's H" ~ "(Shannons H/site visit)"))

#diversity(shannons and spp.richness) by site and month; core sites
da.month.diversity.core<- (group_by(da.wide.habitat.env, month, site))%>%
  summarise_at(vars(shan:abundance), mean) %>%
  filter(site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb")
#abundance
ggplot(da.month.diversity.core) +
  aes(x = month, y = abundance, colour = site, group = site) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "Month", y = expression("Richness" ~ "(species count/site visit)"))

#shannons
ggplot(da.month.diversity.core) +
  aes(x = month, y = shan, colour = site, group = site) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression("Shannon's H" ~ "(Shannons H/site visit)"))

#diversity(shannons and spp.richness) by year, site and month; all sites
da.year<- (group_by(da.wide.habitat.env, year, month, site))%>%
  summarise_at(vars(shan:abundance), mean)
#abundance
ggplot(da.year) +
  aes(x = month, y = abundance, colour = site, group = site) +
  geom_point() +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression("Richness" ~ "(species count/site visit)"))
#shannons H
ggplot(da.year) +
  aes(x = month, y = shan, colour = site, group = site) +
  geom_point() +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression("Shannon's H" ~ "(Shannons H/site visit)"))

#diversity(shannons and spp.richness) by year, site and month; core sites
da.year<- (group_by(da.wide.habitat.env, year, month, site))%>%
  summarise_at(vars(shan:abundance), mean) %>%
  filter(site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb")   
#abundance
ggplot(da.year) +
  aes(x = month, y = abundance, colour = site, group = site) +
  geom_point() +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression("Richness" ~ "(species count/site visit)"))
#shannons H
ggplot(da.year) +
  aes(x = month, y = shan, colour = site, group = site) +
  geom_point() +
  geom_line() +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Month", y = expression("Shannon's H" ~ "(Shannons H/site visit)"))

#diversity by habitat type
#total fish abundance
da.long.habitat <- merge(da.long, habitat, by = "site", incomparables = NA)
group_by(da.long.habitat, year, month, site, veg_subst) %>%
  summarise(abundance1 = sum(abundance, na.rm = TRUE)) %>%
  group_by(year, month, veg_subst) %>%
  summarise(abundance = mean(abundance1), sd(abundance1)) %>%
  filter(month == "Aug" | month == "Jul" | month == "Jun") %>%
  ggplot() +
  aes(x = veg_subst, y = abundance) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "vegetation and substrate", y = expression("Fish abundance" ~ "(total fish count/site visit)"))

# shannon H by habitat
filter(da.wide.habitat.env, month == "Aug" | month == "Jul" | month == "Jun") %>%
  ggplot() +
  aes(x = veg_subst, y = shan) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "vegetation and substrate", y = expression("Shannons H"))

# richness by habitat
filter(da.wide.habitat.env, month == "Aug" | month == "Jul" | month == "Jun") %>%
  ggplot() +
  aes(x = veg_subst, y = abundance) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "vegetation and substrate", y = expression("species count"))

