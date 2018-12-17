#the best way; create function
lfq_year_month <- function(Species, Length) {
  plotlfq <-  dl.temp <- filter(dl.month.dup, species == Species)
  dl.temp <- filter(dl.temp, site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb")
  dl.temp <- filter(dl.temp, length < Length)
  dl.lfq  <- lfqCreate(dl.temp, Lname = "length", Dname = "date", Fname = NA, bin_size = 2,
                       length_unit = "cm", plus_group = FALSE, aggregate_dates = FALSE,
                       plot = FALSE)
  plot(dl.lfq, Fname = "catch", ylab = "Length (mm)")
}
#call above function
lfq_year_month("bapi", 1000)


#golden****
filter(dl, species == "") %>%
  #filter(site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb") %>%
  lfqCreate(Lname = "length", Dname = "date", Fname = NA, bin_size = 5,
                 length_unit = "cm", plus_group = FALSE, aggregate_dates = TRUE,
                 plot = TRUE)



# another way
dl.temp <- filter(dl.month.dup, species == "smsc" | species == "pasc")
dl.temp <- filter(dl.temp, site == "ssp" | site == "hdo" | site == "pba" | site == "chp" | site == "ppo" | site == "wfb")
dl.temp <- filter(dl.temp, length < 150)
dl.lfq  <- lfqCreate(dl.temp, Lname = "length", Dname = "date", Fname = NA, bin_size = 2,
                     length_unit = "cm", plus_group = FALSE, aggregate_dates = TRUE,
                     plot = FALSE)
plot(dl.lfq, Fname = "catch", ylab = "Length (mm)")

summary(dl.enso)
max(dl$length)



#calculates growth metrics
ELEFAN(dl.lfq, Linf_fix = NA, Linf_range = NA, K_range = exp(seq(log(0.1),
                                                              log(10), length.out = 100)), C = 0.9, ts = 0, MA = 5, addl.sqrt = FALSE,
       agemax = 3, flagging.out = TRUE, method = "optimise",
       cross.date = NULL, cross.midLength = NULL, cross.max = FALSE,
       hide.progressbar = FALSE, plot = FALSE, contour = FALSE,
       add.values = TRUE, rsa.colors = terrain.colors(20), plot_title = TRUE)
#fits curves to lfq graphics
lfqFitCurves(dl.lfq, par = list(Linf = 212.5, K = 0.35, t_anchor = 0.7562892, C = 0.9, ts =
                               0.5), agemax = 2, flagging.out = TRUE, lty = 2, lwd = 1, col = 1,
             draw = TRUE, tincr = 0.05)



#
#
#


#others
data(alba)

# plot raw catch frequencies
plot(alba, Fname = "catch")

View(alba)

alba <- lfqRestructure(alba, MA=5)
plot(alba, Fname = "rcounts")


# ELEFAN_SA fitting
set.seed(1)
fitSA <- ELEFAN_SA(
  alba, seasonalised = TRUE,
  init_par = list(Linf=14.5, K=1.1, t_anchor=0.4, ts=0, C=0.2),
  low_par = list(Linf=13, K=0.7, t_anchor=0, ts=0, C=0),
  up_par = list(Linf=15.5, K=1.5, t_anchor=1, ts=1, C=1),
  SA_time = 60
)
unlist(fitSA$par)
fitSA$Rn_max
# plot ELEFAN_SA results
plot(alba, Fname = "catch", draw = FALSE)
lfqFitCurves(fitSA, col=2, par=fitSA$par, draw=TRUE)$Rn_max

data <- data.frame(length.mm. = sample(c(rpois(300, lambda = 60),
                                         rpois(200, lambda = 100), rpois(100, lambda = 150)),
                                       size = 1000, replace = TRUE),
                   dates = seq.Date(as.Date("2015-10-02"),as.Date("2016-08-28"),
                                    length.out = 1000))
lfq_dat <- lfqCreate(data,Lname = "length.mm.", Dname = "dates", aggregate_dates = TRUE,
                     length_unit = "mm", bin_size = 0.5, plot=TRUE, plus_group=c(TRUE,15.75))

dat <- list(midLengths = seq(2,98,4),
            catch = c(0.6,17.6,93,83.2,12.6,0.3,0,0,0,1,17.1,51.4,
                      26.1,2.2,0.2,4.5,21.6,17.6,3.7,8.7,10.6,6.3,5.6,2.9,0.8),
            Linf = 100,
            K = 0.5,
            t0 = 0)
recruitment(param = dat, tsample = 0.25)

data(synLFQ1)
data(synLFQ1)
str(synLFQ1)
summary(synLFQ1)

set.seed(1)
data <- data.frame(length.mm. = sample(c(rpois(300, lambda = 60),
                                         rpois(200, lambda = 100), rpois(100, lambda = 150)),
                                       size = 1000, replace = TRUE),
                   dates = seq.Date(as.Date("2015-10-02"),as.Date("2016-08-28"),
                                    length.out = 1000))
# create lfq data
lfq_dat <- lfqCreate(data,Lname = "length.mm.", Dname = "dates", aggregate_dates = TRUE,
                     length_unit = "mm", bin_size = 0.5, plot=TRUE)

