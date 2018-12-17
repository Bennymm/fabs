#find normal distributions from a multimodal distribution
#provide approximate means of cohorts
#for year month species

#define df for deposition of cohort delineations
column.names <- c("year", "month", "species", "class", "lamda", "mu", "sigma")
cohorts.df <- data.frame()
cohorts.df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("year", "month", "site", "species", "class", "lamda", "mu", "sigma"))

#function
EMfunction2 <- function(Year, Month, Species) {
  dl.shpe <- dl %>%
  filter(year == Year, month == Month, species == Species)

EMfunction3 <- function(XXX, na.rm=TRUE) {normalmixEM(XXX, lambda = .5, mu = c( 60, 90, 150), sigma = 5)}
  
EMfunction1 <-  function(Year, Month, Species) {
    shpe1 <- 
      EMfunction3(dl.shpe$length)
      #normalmixEM(dl.shpe$length, lambda = .5, mu = c( 60, 90, 150), sigma = 5)
      #shpe1 <- apply(dl.shpe, MARGIN = 2, FUN = EMfunction3, na.rm = TRUE)
    
    shpe1 <- shpe1[c("lambda", "mu", "sigma")]
    shpe1 <- as.data.frame(shpe1)
    shpe1 <- rowid_to_column(shpe1)
    colnames(shpe1)[colnames(shpe1)=="rowid"] <- "class"
    shpe1$year <- Year
    shpe1$month <- Month
    shpe1$species <- Species
    shpe1 <<- shpe1
    cohorts.df <<- do.call("rbind", list(shpe1, cohorts.df))
    }
EMfunction1(Year, Month, Species) 
}

#what i want; skip months that don't exist in data
for(month in c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")){for(i in 2014:2018){EMfunction2( i, month, "shpe")}}
#works
for(month in c("Jul","Aug")){for(i in 2014:2018){EMfunction2( i, month, "shpe")}}

#ideal, if it worked
EMfunction4 <- function(na.rm=TRUE) 
  {for(month in c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")){for(i in 2014:2018){EMfunction2( i, month, "shpe")}}}

EMfunction4(na.rm=TRUE)

for(i in 2014:2018){for(month in c(unique("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))){EMfunction2( i, month, "shpe")}}

month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
year <- c(2014:2018)   
species <-
  

list <- data.frame(list(unique(dl$month)))
list.1 <- as.value(list)

dl.shpe <- dl %>%
  filter(year == 2018, month == "Jul" | month == "Sep", species == "shpe")
normalmixEM(dl.shpe$length, lambda = .5, mu = c( 60, 90, 150), sigma = 5)


