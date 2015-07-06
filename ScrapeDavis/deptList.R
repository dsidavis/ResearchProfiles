library(XML)

u = 'http://ucdavis.edu/academics/academic-depts.html'

doc = htmlParse(u)
# a = getNodeSet(doc, "//li/a[not(ancestor::div[@id='footer'])]")

a = getNodeSet(doc, "//li/a[ancestor::div[@id='main_section']]")

dept.urls = structure(sapply(a, xmlGetAttr, "href"), names = sapply(a, xmlValue))

save(dept.urls, file = "dept.urls.rda")


#dept.docs = lapply(dept.urls, htmlParse)
dept.docs = lapply(dept.urls, function(x) try(htmlParse(x)))

dept.docs = lapply(dept.urls, function(x) try(htmlParse(getURLContent(x, followlocation = TRUE), asText = TRUE)))



i = sapply(dept.docs, is, "HTMLInternalDocument")
mapply(saveXML, dept.docs[i], sprintf("Pages/%s.html", gsub("/", "_", names(dept.docs)[i])))

