
library(XML)
dir = "~/Data/NSFAwards"
#ff = list.files(dir, full = TRUE, pattern = "xml$")

ff = unlist(lapply(sprintf("%s/%d", dir, 1996:2014), list.files, full = TRUE, pattern = "xml$"))

inst = sapply(ff, function(x) xpathSApply(xmlParse(x), "//Institution/Name", xmlValue))
i = grep("Davis", inst)

davis = ff[i]

#sapply(davis, function

info = lapply(davis, getAwardInfo)

numPIs = sapply(info, function(x) length(x$people))
table(numPIs)

amt = as.integer(sapply(info, `[[`, "amount"))

a = as.data.frame(do.call(rbind, info), stringsAsFactors = FALSE)
#names(a)[ncol(a)] = "programType"

vars = c("abstract", "directorate", "division", "amount", "title")  # , "programType")
a[vars] = lapply(a[vars], unlist)

tmp = lapply(a$dates, as.Date, "%m/%d/%Y")
a$startDate = structure(sapply(tmp, `[[`, 1), class = "Date")
a$endDate = structure(sapply(tmp, `[[`, 2), class = "Date")
a = a [ - match("dates", names(a)) ]

plot(density(as.numeric(difftime(a$endDate, a$startDate, units = "weeks")/52), bw = .02))






