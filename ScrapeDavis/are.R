library(XML)
library(RCurl)

u = 'https://are.ucdavis.edu/en/people/faculty/'

txt = getURLContent(u)
doc = htmlParse(txt, asText = TRUE)
a = getNodeSet(doc, "//h1[@role = 'heading']/a")

are = data.frame(name = sapply(a, xmlValue), 
                 url = getRelativeURL(sapply(a, xmlGetAttr, "href"), u), stringsAsFactors = FALSE, row.names = NULL)


getInfo =
function(u)
{
  txt = getURLContent(u)
  doc = htmlParse(txt)
  ra = xpathSApply(doc, "//h1[. = 'Fields of Interest']/following-sibling::p[1]", xmlValue)

  tmp = xpathSApply(doc, "//h1[. = 'Education']/following-sibling::p[1]", xmlValue)
  # Don't process the year here. We'll do them collectively and keep the full details in case
  # the format isn't what we expect.
#  year = gsub(".*,", "", tmp)
  
  list(interests = ra, phdYear = tmp)
}

info = lapply(are$url, getInfo)

tmp = cbind(are, as.data.frame(do.call(rbind, info), stringsAsFactors = FALSE))
year = as.integer(gsub(".*([0-9]{4}).*", "\\1", tmp$phdYear))
are = subset(tmp, select = -phdYear)
are$year = year
