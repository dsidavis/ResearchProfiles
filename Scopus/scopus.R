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

#  numResults = as.integer(ans[[1]][[1]]) # totalResults
#  if((!is.na(max) && numResults < max)  || numResults <= length(ans[[1]]$entry))
#    return(ans)
 
# c(ans, getNextPages(as.integer(ans[[1]][[2]]), query, ..., key = key, url = url, curl = curl, max = max))

}

scopusQuery =
function(..., url, max = NA, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list())
{
  .opts$httpheader = c('X-ELS-APIKey' = key)
  ans = getForm(url, .params = list(...), .opts = .opts, curl = curl)
  res = fromJSON(ans)
  getNextPages(res, res, max = max, url = url, curl = curl)
}


getNextPages = 
function(ans, ..., url, max = NA, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list(), .varName = "scopusResults")
{
    info = ans[[1]]
    results = info$entry

    page = 1L
    while(is.na(max) || length(results) < max) {
        u = do.call(rbind, info[["link"]])
        i = match("next", u[, "@ref"])
        if(!is.na(i)) {
             u = u[i, "@href"]
             page = page + 1L
	     if(verbose)
                 cat("querying page", page, "\n")
             tmp = fromJSON(getURLContent(I(u), curl = curl))
             info = tmp[[1]]
             results = c(results, info$entry)

 	     if(!is.na( .varName ) && nchar(.varName) > 0)
                assign(.varName, results, globalenv())
        } else
           break
    }

    results
}


#  http://api.elsevier.com/documentation/AUTHORSearchAPI.wadl
scoGetAuthor =
function(last, affil = NA, first = NA, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list())
{
    q = sprintf("authlastname(%s)", last)
    if(!is.na(affil))
       q = sprintf("%s AND af-id(%s)", q, as.character(affil))

    ans = scopusQuery(query = q, url = "http://api.elsevier.com/content/search/author", curl = curl, key = key, .opts = .opts)

#    num = as.integer(ans[[1]][[1]])
#    if(num > length(ans[[1]]$entry))  
#       ans = getNextPages(ans[[1]])
#    else
#       ans[[1]]$entry
}


getArticlesByAffiliation =
# 
#  ...  can include terms such as dateloaded
#
# art = getArticlesByAffiliation(60014439, max = 2000)

function(affil, max = NA, ..., curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list())
{
   q = sprintf("af-id(%s)", as.character(affil))
   ans = scopusQuery(query = q, max = max, url = 'http://api.elsevier.com/content/search/index:SCOPUS', curl = curl, key = key, .opts = .opts) 

   

}



getDocInfo =
function(u, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list())
{
  .opts$httpheader = c('X-ELS-APIKey' = key)
  txt = getURLContent(u, curl = curl, .opts = .opts)
  doc = xmlParse(txt, asText = TRUE)
}

processDocResults =
function(doc)
{
  # num authors, title, abstract, date, issn, "journal" type &  name/identifier, citedby-count, paste author id's and separate by ';'  link to another table
  #  publisher, scopus identifier, doi,  subject-areas
  # references 

  # What is an SGR for refd-itemidlist

  r = xmlRoot(doc)

  abs = getNodeSet(doc, "//x:abstract/ce:para",  c(x = "http://www.elsevier.com/xml/svapi/abstract/dtd", ce = "http://www.elsevier.com/xml/ani/common"))
}