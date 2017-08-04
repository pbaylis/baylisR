library(devtools)
library(roxygen2)

setwd("~/github/baylisR")
setwd("B:/github/baylisR")
create("baylisR")
document()
install("baylisR")

library(baylisR)
