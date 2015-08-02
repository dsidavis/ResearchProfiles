u = "http://wineserver.ucdavis.edu/people/faculty.html"

doc = htmlParse(u)

f = 
function(x)
{
  u = getRelativeURL(xmlGetAttr(x, "href"), docName(x))
  name = xpathSApply(x, ".//h3", xmlValue, trim = TRUE)
  title = xpathSApply(x, ".//h4", xmlValue, trim = TRUE)
  area = xpathSApply(x, ".//br/following-sibling::text()", xmlValue, trim = TRUE)
  if(!length(area))
    area = ""

  data.frame(name = name, title = title, url = u, area = area, stringsAsFactors = FALSE)
}

enology = do.call(rbind, xpathApply(doc, "//tr/td[2]/a", f))

saveRDS(enology, "enology.rda")
