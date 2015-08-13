# Description:
#   Utility functions for scraping faculty pages.

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

sanitize =
function(string)
  # Remove unnecessary whitespace from a string.
{
  string = gsub("^[[:space:]]*|[[:space:]]*$", "", string)
  string = gsub("\\t|\\r|\\n", "", string)
  gsub("  +", " ", string)
}

