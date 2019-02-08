habitat$exposure <- as.integer(factor(habitat$exposure, 
                   levels = c("vp", "p", "sp", "se", "e", "ve"), 
                   labels = c("vp", "p", "sp", "se", "e", "ve")))



da.long$month <- factor(da.long$month, 
                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                        labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

str(habitat)
