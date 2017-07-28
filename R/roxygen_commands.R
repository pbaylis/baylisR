library(devtools)
library(roxygen2)

setwd("~/github/baylisR")
create("baylisR")
document()
install("baylisR")