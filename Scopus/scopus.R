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

scoAffiliation =
function(query, ..., key = getOption("ScopusKey", stop("need the scopus API key")),  url = "http://api.elsevier.com/content/search/affiliation", curl = getCurlHandle())
{
   scopusQuery(q = query, ..., key = key, curl = curl, url = url)
}

scopusQuery =
function(..., url, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list())
{
  .opts$httpheader = c('X-ELS-APIKey' = key)
  getForm(url, .params = list(...), .opts = .opts, curl = curl)
}
