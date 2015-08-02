source("funcs.R")

doc = htmlParse("http://www.physics.ucdavis.edu/people/Faculty.html")
nodes = getNodeSet(doc, "//tr/td[1]/a")

phy = getNames(nodes)


tt = readHTMLTable("http://www.physics.ucdavis.edu/people/Faculty.html")[[1]]
phy$title = tt[[2]]

saveRDS(phy, "physics.rds")