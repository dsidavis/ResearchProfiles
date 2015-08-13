# This script uses crossref metadata to ID papers where the author's institution is explicitly Davis
# and then uses those papers to find the GS profile of the Davis person.
# Could also use those papers directly to build up content area of researchers.
source("../rcrossref/crossrefFun.R")
source("gsProfile.R")
source("run.R")  # For the curl handle

fac = loadFaculty()
set.seed(95616)
oneEachFac = lapply(fac, function(x) as.character(x$name[[sample(length(x$name), 1)]]))
lapply(oneEachFac, getConfirmedGSProfileURL)
