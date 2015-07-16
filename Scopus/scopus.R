library(RCurl)
library(XML)
library(RJSONIO)

getDoc =
function(doi, key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle(), ...)
{
  url = paste0("http://api.elsevier.com/content/article/DOI:%s", doi)
  getURLContent(url, httpheader = c('X-ELS-APIKey' = key), ...)
}



searchScopus =
function(q, key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle(), ...)
{
     #http://api.elsevier.com/content/search/scopus
    
}
