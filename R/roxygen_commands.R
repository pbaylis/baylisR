library(devtools)
library(roxygen2)

setwd("~/github/baylisR")
setwd("B:/github/baylisR")
create("baylisR")
document()
install("baylisR")

library(baylisR)

install_github("pbaylis/baylisR", auth_token="ebb64529e5a9127f17624c261dbd727b4ab7c9e6")
