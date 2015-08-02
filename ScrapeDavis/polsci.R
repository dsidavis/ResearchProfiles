source("funcs.R")
u = "http://ps.ucdavis.edu/people/political-science-faculty"
tt = readHTMLTable(u)

doc = htmlParse(u)
nodes = getNodeSet(doc, "//table[position() = 1 and @class = 'listing']//td[@class = 'personDetails']")


nodes = getNodeSet(doc, "//table//tbody[2]//td[@class = 'personDetails']/..")

polsci = getNames(xpathSApply(doc, "//table//tbody[2]//td[@class = 'personDetails']/a"))
polsci$url = getNodeSet(doc, "//table//tbody[2]//td[@class = 'personDetails']/a/@href")



polsci$area =  sapply(nodes, function(x) {
                              tmp = xpathSApply(x, ".//div[@class = 'specialties']", xmlValue, trim = TRUE)
                              if(!length(tmp))
                                ""
                              else
                                tmp
                           })

polsci$area =  gsub("[[:space:]]+", " ", polsci$area )

polsci$title =  sapply(nodes, function(x) {
                              tmp = xpathSApply(x, ".//ul[@class = 'jobTitle']", xmlValue, trim = TRUE)
                              if(!length(tmp))
                                ""
                              else
                                tmp
                           })


saveRDS(polsci, "polsci.rds")








