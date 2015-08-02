
library(XML)
library(RCurl)

source("funcs.R")


tt = getURLContent("http://faculty.engineering.ucdavis.edu/", useragent = "Firefox")
doc = htmlParse(tt)
u = xpathSApply(doc, "//div[@class = 'menu-departments-container']//a/@href")




#u = c("http://faculty.engineering.ucdavis.edu/biological-and-agricultural-engineering/",
#      "http://faculty.engineering.ucdavis.edu/biomedical-engineering/"


faculty = 
function(u)
{
   tt = getURLContent(u, useragent = "Firefox")

   doc = htmlParse(I(tt))
   nodes = getNodeSet(doc, "//a[starts-with(@href, 'http://faculty.engineering.ucdavis.edu/') and @target = '_blank']")

   getNames(nodes)
}

eng = lapply(u,  faculty)

eng = as.data.frame(do.call(rbind, eng), stringsAsFactors = FALSE)

save(eng, file = "engineering.rda")


#bioAgEng = 

