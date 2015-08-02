source("funcs.R")
u = "http://biosci3.ucdavis.edu/Faculty/Profile/ActiveFaculty"
doc = htmlParse(u)

nodes = getNodeSet(doc, "//a[contains(@href, 'Faculty/Profile/View')]")

biosci = getNames(nodes)
save(biosci, file = "biosci.rda")

