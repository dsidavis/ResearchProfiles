source("funcs.R")

u = "https://www.math.ucdavis.edu/people/"
doc = htmlParse(I(getURLContent(u)))

#nodes = getNodeSet(doc, "//table[1]//tr/td[1]//a")
main = getNodeSet(doc, "//table")[[1]]
nodes = getNodeSet(main, ".//tr/td[1]") 
math = getNames(nodes, comma = FALSE)

math$areas = xpathSApply(main, ".//tr/td[2]", xmlValue, trim = TRUE)

saveRDS(math, "math.rds")

