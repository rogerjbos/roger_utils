library(devtools)
library(roxygen2)

create("roger")
setwd("M:/R_HOME/roger")
document()
setwd("..")
install("roger")

library(roger)
?gg
