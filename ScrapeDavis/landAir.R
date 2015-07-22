library(RCurl)
library(XML)

if(FALSE) {
u = 'http://www.lawr.ucdavis.edu/directory_faculty_atm.htm'
doc = htmlParse(u)
atm = xpathSApply(doc, "//div[@id = 'image_left']", getInfo)
}


getInfo =
function(node)
{
  url = getNodeSet(node, ".//a[@href and not(starts-with(@href, 'mailto'))]/@href")[[1]]
  nxt = getNodeSet(node, ".//following-sibling::div[1]")[[1]]
  name = xpathSApply(nxt, ".//strong", xmlValue)[[1]]
  c(name = name, url = as.character(url), title = NA)
}




us = c(atm = 'http://www.lawr.ucdavis.edu/directory_faculty_atm.htm',
       hyd = 'http://www.lawr.ucdavis.edu/directory_faculty_hyd.htm',
       sbg = 'http://www.lawr.ucdavis.edu/directory_faculty_sbg.htm',
       coopExt = 'http://www.lawr.ucdavis.edu/directory_faculty_coop.htm' )

fac = lapply(us, function(u) {
                doc = htmlParse(u)
                as.data.frame(do.call(rbind, xpathApply(doc, "//div[@id = 'image_left']", getInfo)), stringsAsFactors = FALSE)
            })

landAir = do.call(rbind, fac)
landAir$Dept = rep(c("Atmospheric Science", "Hydrology", "Soils & BioGeochemistry", "Cooperative Extension"), sapply(fac, nrow))
