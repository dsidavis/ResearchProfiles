library(XML)
library(RCurl)
getNames = 
function(nodes, comma = TRUE, base = docName(nodes[[1]]))
{
   names = sapply(nodes, xmlValue)
   if(comma) {
       tmp = strsplit(names, ",[  ]")
       first = sapply(tmp, `[`, 2)
       last = sapply(tmp, `[`, 1)
       names = sapply(tmp, function(x) paste(rev(x), collapse = " "))
    } else {
       tmp = strsplit(names, " +")
       first = sapply(tmp, function(x) paste(x[1:(length(x) - 1)], collapse = " "))
       last = sapply(tmp, function(x) x[length(x)])
   }

   u = sapply(nodes, xmlGetAttr, "href", "")
   u = getRelativeURL(u, base)

   data.frame(name = names,  url = u, first = first, last = last, stringsAsFactors = FALSE, row.names = NULL)
}
