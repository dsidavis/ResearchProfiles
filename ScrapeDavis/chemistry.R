source("funcs.R")

doc = htmlParse("http://chemistry.ucdavis.edu/faculty/discipline.html")
nodes = getNodeSet(doc, "//tr/td[1]/a")

chem = getNames(nodes)

chem$areas = xpathSApply(doc, "//tr/td[2]", xmlValue, trim = TRUE)

saveRDS(chem, "chemistry.rds")
