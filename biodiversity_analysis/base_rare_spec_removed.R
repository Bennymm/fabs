library("tidyverse", lib.loc="~/R/win-library/3.5")
library("dplyr", lib.loc="~/R/win-library/3.5")
library("vegan", lib.loc="~/R/win-library/3.5")
library("BiodiversityR", lib.loc="~/R/win-library/3.5")
library("dismo", lib.loc="~/R/win-library/3.5")
library("knitr", lib.loc="~/R/win-library/3.5")
library("yaml", lib.loc="~/R/win-library/3.5")
library("markdown", lib.loc="~/R/win-library/3.5")
library("MASS", lib.loc="~/R/win-library/3.5")
library("magrittr", lib.loc="~/R/win-library/3.5")
library("lubridate", lib.loc="~/R/win-library/3.5")
library("rjags", lib.loc="~/R/win-library/3.5")
library("TropFishR", lib.loc="~/R/win-library/3.5")
library("mixtools", lib.loc="~/R/win-library/3.5")


# make fabsdivlength name of and create working file 
setwd("C:/Users/FABS/Desktop/Rwd_Ben/FABS/working_files/data")

# read in da, dl, ysi and habitat
da      <- read.csv ("C:/Users/FABS/Desktop/Rwd_Ben/FABS/working_files/data/da2018.csv")
dl      <- read.csv ("dl2018.csv")
habitat <- read.csv ("habitat.csv")
ysi     <- read.csv ("ysi.csv")

#create da.wide
da.wide <- da %>%
  group_by(year, month, day, site, species) %>%
  summarise(abundance1 = sum(abundance, na.rm = TRUE))%>%
  spread("species", "abundance1")
#replace na with 0
da.wide[is.na(da.wide)] <- 0
#create long data with null observations
da.long <- gather(da.wide, species, abundance, argo:yero)

#remove replicates and duplicates from ysi
ysi <- ysi[!(ysi$replicate == 2), ]              
ysi$replicate <- NULL
ysi <- unique(ysi[ ,1:10])

#create temperature wide data from ysi data; can do with salinity and ph also
temp.wide <- ysi
#remove other variables(ph, salinity)
temp.wide[8:10] <- NULL
#remove depth
temp.wide[6] <- NULL
#spread to wide format
temp.wide %<>%
  group_by(site, year, month, day, location) %>%
  spread("location", "temp")

#merge temperature data
da.wide.habitat.env <- merge(da.wide, temp.wide,  
                             by = c("year", "month", "day", "site"), 
                             all = TRUE)
#merge habitat data
da.wide.habitat.env <- merge(da.wide.habitat.env, habitat, by = "site")
da.wide.habitat.env[5] <- NULL

#assign levels to month
da.wide.habitat.env$month <- factor(da.wide.habitat.env$month, 
                                    levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                                    labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

da.long$month <- factor(da.long$month, 
                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                        labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

#create date variable for TropFishR
#multiple monthly sample events justify bimonthly sample units
dl.month.dup <- dl  
dl.month.dup$bimonth <- if_else(dl.month.dup$day < 15, 7, 22)
dl.month.dup$date <- ymd(paste(dl.month.dup$year, dl.month.dup$month, dl.month.dup$bimonth))

#assign levels to month
dl$month <- factor(dl$month, 
                   levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                   labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

#create avgerage length variable and SE of length; filter core sites
dl.year.month.core <- dl
dl.year.month.core <-
  filter(dl.year.month.core, site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb") %>%
  group_by(year, month, species) %>%
  summarise(se = sd(length)/sqrt((length(species))), length = mean(length))

#create avgerage length variable and SE of length; filter core sites
#same as above but for multiple cohorts(shpe)
dl.year.month.core.cohorts <- dl
dl.year.month.core.cohorts <-
  filter(dl.year.month.core.cohorts, site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb") %>%
  group_by(year, month, species, stage) %>%
  summarise(se = sd(length)/sqrt((length(species))), length = mean(length))

species.count <-
group_by(da, species) %>%
 summarise(tcount = sum(abundance, na.rm = TRUE))
