
#template for length graphics
#change species and length filter
#Note: multiple cohort species generate uninterpretable results (Bapi, shpe)

#length frequency graphics function by year and bi-month
lfq_year_month <- function(Species, Length) {
  plotlfq <-  dl.temp <- filter(dl.month.dup, species == Species)
  dl.temp <- filter(dl.temp, site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb")
  dl.temp <- filter(dl.temp, length < Length)
  dl.lfq  <- lfqCreate(dl.temp, Lname = "length", Dname = "date", Fname = NA, bin_size = 2,
                       length_unit = "cm", plus_group = FALSE, aggregate_dates = FALSE,
                       plot = FALSE)
  plot(dl.lfq, Fname = "catch", ylab = "Length (mm)")
}
#execute above function
#function of species and length filter(outliers)
lfq_year_month("tisc", 100)



dl.temp <- filter(dl, species == "tisc")
max(dl.temp$length)

#seasonal mean length function for any species with standard error
#only useful for species with single cohorts; doesn't work for pipefish, perch, herring, etc
mean_length_seasonal <- function(Species) {
filter(dl.year.month.core, species == Species) %>%
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
mean_length_seasonal("enso")

#size distribution function defined
#core sites, July
#not included in report
lfq_July <- function(Species) {
filter(dl, species == Species) %>%
  filter(stage == 3) %>%
  filter(month == "Jul") %>%
  filter(site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb") %>%
  ggplot() +
  aes(length, colour = factor(year), group = year) +
  geom_freqpoly(binwidth = 20) +
  theme_bw() +
  theme(axis.text.x      = element_text(size = 10, angle = 45, hjust = 1),
        #legend.position  = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text             = element_text(size = 13, vjust = 0.1)) +
  labs(x = "Length (mm)", y = expression("Count"), colour = "Year")
}
#execute above function
lfq_July("ling")



