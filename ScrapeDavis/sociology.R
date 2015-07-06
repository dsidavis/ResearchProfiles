library(XML)
library(RCurl)


u = 'http://sociology.ucdavis.edu/people'

doc = htmlParse(u)
tb = getNodeSet(doc, "//thead[contains(., 'Faculty')]/following-sibling::tbody")[[1]]

td = getNodeSet(tb, ".//td[@class = 'personDetails']")

getInfo =
function(node)
{
   list(name = xmlValue(node[["a"]]),
        url = xmlGetAttr(node[["a"]], "href"),
        title = xmlValue(node[["ul"]]))
}
info = lapply(td, getInfo)

soc = as.data.frame(do.call(rbind, info), stringsAsFactors = FALSE)
