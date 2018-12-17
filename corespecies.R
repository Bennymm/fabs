da.wide.core <-
  filter(da.wide, site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb")

da.wide.core$site <- as.character(da.wide.core$site)

da.months_sampled <- da.wide.core %>%
  group_by(site) %>%
  summarise(months = length(site))









str(da.wide.core)
