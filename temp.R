#create temperature variable
temp.summary<-
  group_by(da.wide.habitat.env, year, month, site, veg_subst) %>%
  summarise(temp = mean(b2))

#monthly temperature by year and habitat type
group_by(temp.summary, year, month, veg_subst) %>%
  summarise(se = sd(temp)/sqrt((length(veg_subst))), temp = mean(temp), n()) %>%
  filter( year == "2015" | year == "2016" | year == "2017") %>%
  ggplot() +
  aes(x = month, y = temp, colour = factor(veg_subst, labels = c("macro.sand",
                                                                 "mud",
                                                                 "sand",
                                                                 "ulva.sand",
                                                                 "unal.cobble",
                                                                 "unal.gravel",
                                                                 "unal.mud",
                                                                 "unal.sand",
                                                                 "zostera.gravel",
                                                                 "zostera.mud",
                                                                 "zostera.sand")), group = veg_subst) +
  geom_point(size = 2.5, position = position_dodge(width = 0.25)) +
  geom_line( position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymin = temp - se,
                    ymax = temp + se), 
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
  labs(x = "Month", y = expression("temperature"), colour = "veg_subst")

#Temperature by habitat type
group_by(temp.summary, year, month, veg_subst)%>%
  summarise(se = sd(temp)/sqrt((length(veg_subst))), temp = mean(temp), n()) %>%
filter(month == "Aug" | month == "Jul" | month == "Jun") %>%
ggplot() +
aes(x = veg_subst, y = temp) +
geom_boxplot() +
facet_wrap(~year) +
theme_bw() +
theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
      legend.position  = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      text             = element_text(size = 13, vjust = 0.1))+
labs(x = "vegetation and substrate", y = expression("temperature"))

#temperature by habitat type
filter(temp.summary,
       year != "2014",
       month == "Jun" | month == "Jul", 
       site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb") %>%
group_by(year, veg_subst)%>%
  summarise(se = sd(temp, na.rm = TRUE)/sqrt((length(veg_subst))), 
            temp = mean(temp, na.rm = TRUE), n()) %>%
  ggplot() +
  aes(x = veg_subst, y = temp, colour = factor(year, labels = c("2015",
                                                                "2016",
                                                                "2017")), group = year) +
  geom_point(size = 2.5, 
             position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(ymin = temp - se,
                    ymax = temp + se), 
                width = 0,
                size = 1,
                position = position_dodge(width = 0.25)) +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position= "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1))+
  labs(x = "vegetation and substrate", y = expression("temperature"), color = "Year")

