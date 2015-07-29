library(rcrossref)
library(XML)

getCrossrefMeta = function(name, max = 50)
{
  papers = cr_search(name, rows = max)
  pages = lapply(papers$doi, function(x) try(htmlParse(x), TRUE))
  pages = pages[sapply(pages, function(x) !"try-error" %in% attr(x, "class"))]
  lapply(pages, function(page) try(getDOIinfo(page)))
  
}

getDOIinfo = function(doc, fields = c("name", "content"))
{
  sapply(fields, function(x)
    xpathSApply(doc, "//head/meta", xmlGetAttr, x))
}