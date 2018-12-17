
library("mixtools", lib.loc="~/R/win-library/3.5")


function(Year, Month, Site, Species, Class, Lambda, Mu, Sigma)
  
dl.shpe <- dl 
dl.shpe <- filter(dl.shpe, species == "shpe", year == "2017", month == "Jul")

shpe1 <- normalmixEM(dl.shpe$length, lambda = .5, mu = c( 30, 80, 120 ), sigma = 5)

shpe1[c("lambda", "mu", "sigma")]

plot(shpe1, breaks = 100, density=TRUE, cex.axis=1.4, cex.lab=1.4, cex.main=1.8)

hist(dl.shpe$length, breaks = 100, main="length/dist",
     xlab="length", ylab="frequency", cex.main=1.5, cex.lab=1.5, cex.axis=1.4)

column.names <- c("year", "month", "site", "species", "class", "lamda", "mu", "sigma")

cohorts.df <- data.frame()

cohorts.df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("year", "month", "site", "species", "class", "lamda", "mu", "sigma"))

