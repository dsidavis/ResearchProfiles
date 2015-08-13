library(XML)
u = "http://education.ucdavis.edu/faculty-directory"

doc = htmlParse(u)

nodes = getNodeSet(doc, "//a[starts-with(@href, '/faculty-profile') and @title]")
ed = data.frame(name = sapply(nodes, xmlGetAttr, "title"), url = getRelativeURL(sapply(nodes, xmlGetAttr, "href") , u))
rownames(ed) = NULL
save(ed, file = "education.rda")


