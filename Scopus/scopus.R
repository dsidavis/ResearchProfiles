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
function(query, ..., max = NA, key = getOption("ScopusKey", stop("need the scopus API key")),  url = "http://api.elsevier.com/content/search/affiliation", curl = getCurlHandle())
{
  ans = scopusQuery(query = sprintf("affil(%s)", query), ..., key = key, curl = curl, url = url)

  numResults = as.integer(ans[[1]][[1]]) # totalResults
  if((!is.na(max) && numResults < max)  || numResults <= length(ans[[1]]$entry))
    return(ans)

  c(ans, getNextPages(as.integer(ans[[1]][[2]]), query, ..., key = key, url = url, curl = curl, max = max))

}

scopusQuery =
function(..., url, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list())
{
  .opts$httpheader = c('X-ELS-APIKey' = key)
  ans = getForm(url, .params = list(...), .opts = .opts, curl = curl)
  fromJSON(ans)
}


getNextPages = 
function(start, ..., url, max = NA, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list())
{

}