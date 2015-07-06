
library(XML)
dir = "~/Data/NSFAwards/2014"
#ff = list.files(dir, full = TRUE, pattern = "xml$")

ff = unlist(lapply(as.character(1996:2014), list.files, full = TRUE, pattern = "xml$"))

inst = sapply(ff, function(x) xpathSApply(xmlParse(x), "//Institution/Name", xmlValue))
i = grep("Davis", inst)

davis = ff[i]

sapply(davis, function

info = lapply(ff, getAwardInfo)



