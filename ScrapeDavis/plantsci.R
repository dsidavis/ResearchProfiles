
u = "http://www.plantsciences.ucdavis.edu/plantsciences/databases/directory/faculty.aspx"
doc = htmlParse(u)


f = function(x) {

  name = xpathSApply(x, "./strong", xmlValue, trim = TRUE)

  txt = xmlValue(x[[3]], trim = TRUE)
  title = gsub("--.*",  "", txt)

  u = xpathSApply(x, "./a[not(starts-with(@href, 'mailto'))]", xmlGetAttr, "href")
  if(!length(u))
     u = ""

  area = paste(XML:::trim(gsub(".*Section:", "", xmlValue(x))), gsub(".*--",  "", txt))
  data.frame(name = name, title = title, url = u, area = area, stringsAsFactors = FALSE)
}


plantsci = do.call(rbind, xpathApply(doc, "//td[strong]", f))

saveRDS(plantsci, "plantsci.rds")
