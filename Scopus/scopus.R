library(RCurl)
library(XML)
library(RJSONIO)

buildQuery =
function(parameters)
{
  query = sprintf("%s(%s)", names(parameters), parameters)
  paste0(query, collapse = " AND ")
}

getAuthorID = 
function(..., key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle())
  # Get author IDs matching the specified query parameters.
  #
  # Query parameters include authfirst, authlast, affil, and any others listed
  # in the Scopus Author Search API.
{
  query = buildQuery(list(...))

  # TODO: Fetch all relevant author IDs up to max retrievable, not just 25.

  # Query Scopus to get author ID.
  response = scopusAuthorSearch(query = query, key = key, curl = curl)
  response = fromJSON(response)

  results = response[["search-results"]]

  n_entries = results[["opensearch:totalResults"]]

  if (n_entries > 0) {
    # Extract author ID from each entry.
    entries = results$entry

    vapply(entries, function(entry) {
      sub("AUTHOR_ID:", "", entry[["dc:identifier"]])
    }, "")
  } else {
    # No authors found.
    character(0)
  }
}

scopusAuthorSearch =
function(..., url,
  key = getOption("ScopusKey", stop("need the scopus API key")),
  curl = getCurlHandle(), .opts = list())
  # Query the Scopus Author Search API.
{
  .opts$httpheader = c("X-ELS-APIKey" = key)
  url = "http://api.elsevier.com/content/search/author"
  getForm(url, .params = list(...), .opts = .opts, curl = curl)
}

getDoc =
function(doi, key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle(), ...)
{
  url = paste0("http://api.elsevier.com/content/article/DOI:%s", doi)
  getURLContent(url, httpheader = c('X-ELS-APIKey' = key), ...)
}



searchScopus =
function(q, field, key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle(), ...)
{
     #http://api.elsevier.com/content/search/scopus
    
}

getAuthorDocsIds = 
function(id, field = "dc:identifier", key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle(), ...)
{
  u = "http://api.elsevier.com/content/search/scopus"
  q = sprintf("AU-ID(%s)", as.character(id))
  ans = scopusQuery(query = q, field = field, url = u, key = key, curl = curl, .opts = list(...))
  as.data.frame(do.call(rbind, ans), stringsAsFactors = FALSE)
}



# http://api.elsevier.com/documentation/retrieval/AbstractRetrievalViews.htm
DocInfoFields = c("authors", "title", "publicationName", "volume", "issueIdentifier", "dc:description", "subject-areas",
                  "prism:pageRange" , "coverDate", "article-number",  "doi", "citedby-count", "prism:aggregationType")

getDocInfo = 
function(id, fields = DocInfoFields, view = "FULL", key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle(), ...)
{
  u = paste0("http://api.elsevier.com/content/abstract/scopus_id/", id)
  .params = if(!is.na(view))
               list(view = view)
            else if(length(fields) == 1 && fields %in% c("META", "META_ABS", "FULL", "REF", "ENTITLED"))
               list(view = fields)
            else 
               list(field = paste(fields, collapse = ","))
     
  .params$httpAccept = "application/json"
  ans = scopusQuery(.params = .params,  url = u, key = key, curl = curl, .opts = list(...))
  ans[[1]]
}

getAuthorDocs = 
function(name, ..., max = 25, key = getOption("ScopusKey", stop("need the scopus API key")), curl = getCurlHandle())
{
   # Assumes get only id back. If more, we ignore them. And this could be multiple people.
    r.id = scoGetAuthor(name, idOnly = TRUE, key = key, curl = curl)
    doc.ids = getAuthorDocsIds (r.id[1], curl = curl)
    docs = lapply(doc.ids[,2], getDocInfo, curl = curl)
}


scoAffiliation =
function(query, ..., max = 25, key = getOption("ScopusKey", stop("need the scopus API key")),  url = "http://api.elsevier.com/content/search/affiliation", curl = getCurlHandle())
{
  ans = scopusQuery(query = sprintf("affil(%s)", query), ..., key = key, curl = curl, url = url, max = max)

  ids = gsub("AFFILIATION_ID:", "", sapply(ans, `[[`, "dc:identifier"))
  name = sapply(ans, `[[`, "affiliation-name")
  names(ids) = name
  ids
}


getArticleText = 
# 
#  not all articles are available.
#
function(id, ..., httpAccept = "text/xml", key = getOption("ScopusKey", stop("need the scopus API key")),  
         curl = getCurlHandle(), .opts = NULL, idType = guessIDType(id))
{
  u = switch(idType, 
               DOI = "http://api.elsevier.com/content/article/doi/",
               PII = "http://api.elsevier.com/content/article/pii/",
               EID = "http://api.elsevier.com/content/article/eid/",
               Scopus = "http://api.elsevier.com/content/article/scopus_id/",
               Medline = "http://api.elsevier.com/content/article/doi/"
            )
  
  url = sprintf("%s%s", u, id)
  scopusQuery(url = url, ..., httpAccept = httpAccept, curl = curl, key = key, .opts = .opts)
}

guessIDType = 
function(id)
{
  if(grepl("/", id))
    "DOI"
  else 
   stop("not recognized yet")
}


scopusQuery =
function(..., url, max = NA, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list(), 
         .params = list(...))
{
  .opts$httpheader = c('X-ELS-APIKey' = key)
  ans = getForm(url, .params = .params, .opts = .opts, curl = curl)
  res = fromJSON(ans)
  getNextPages(res, res, max = max, url = url, curl = curl)
}


getNextPages = 
function(ans, ..., url, max = NA, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list(), 
         .varName = "scopusResults", verbose = TRUE)
{
    info = ans[[1]]
    if(!("opensearch:totalResults" %in% names(info)))
        return(ans)

    results = info$entry

    totalNum = as.integer(info[[1]])

    page = 1L
    while(is.na(max) || length(results) < max) {
        if(is.null(info$link) || is.character(info$link)) # not a list of links
             break
        u = do.call(rbind, info[["link"]])
        i = match(c("next", "last"), u[, "@ref"])
        if(any(!is.na(i))) {
              u = u[i[ !is.na(i) ][1], "@href"]
             page = page + 1L
	     if(verbose)
                 cat("querying page", page, "\n")
             tmp = fromJSON(getURLContent(I(u), curl = curl))
             info = tmp[[1]]
             results = c(results, info$entry)

 	     if(!is.na( .varName ) && nchar(.varName) > 0)
                assign(.varName, results, globalenv())

             if(is.na(i)[1])
                break
        } else {
#      	   browser()
           break
        }
    }

    structure(results, totalNumResults = totalNum)
}


#  http://api.elsevier.com/documentation/AUTHORSearchAPI.wadl
scoGetAuthor =
#
# scoGetAuthor("Temple Lang", "Davis")
# scoGetAuthor("Smith", "Davis", "MacKenzie")
# mck = scoGetAuthor("Smith", 60014439, "MacKenzie")
# scoGetAuthor("Tomich", "Davis")
# sz = scoGetAuthor("Sawyer", 60014439, "Suzana")
# wp = scoGetAuthor(c("Polonik", "Wolfgang"), 60014439)
# na = scoGetAuthor(c("Anderson", "Nicholas"), 60014439)
# jq = scoGetAuthor(c("Quinn", "Jim"), 60014439)
# dh = scoGetAuthor(c("Halfmann"))
# sch = scoGetAuthor(c("Shauman"))
# dn = scoGetAuthor(c("Niemeier"))  #  2 answers - same person?
# nina = scoGetAuthor(c("Amenta"))
# kwanliu = scoGetAuthor(c("Ma", "Kwan-Liu")) # 5 answers
# joy = scoGetAuthor(c("Joy", "Ken"))
# raissa = scoGetAuthor(c("D'Souza", "Raissa")) # 2 - Davis and SFI
# prem = scoGetAuthor(c("Devanbu")) # 2
# jimc = scoGetAuthor(c("Crutchfield"))
# tony = scoGetAuthor(c("Tyson"))   # 6 and not all the same person.
# prabir = scoGetAuthor(c("Burman"))
# ethan = scoGetAuthor(c("Anderes"))
# jie = scoGetAuthor(c("Peng", "Jie"))
# debashis = scoGetAuthor(c("Paul", "Debashis"))
# hans = scoGetAuthor(c("Muller", "Hans"))
# thomas = scoGetAuthor(c("Lee", "Thomas"))  # 5
# ben = scoGetAuthor(c("Houlton"))
# vlad = scoGetAuthor(c("Filkov"))
# norm = scoGetAuthor(c("Matloff")) 
# patrice = scoGetAuthor(c("Koehl")) # 2 not the same
# prasad = scoGetAuthor(c("Naik", "Prasad"))
# jdo = scoGetAuthor(c("Owens", "John"))  # 5 but 1st is the one I know.
# bertram = scoGetAuthor(c("Ludaescher"))
# prasant = scoGetAuthor(c("Mohapatra", "Prasant"))
# louise = scoGetAuthor(c("Kellogg", "Louise")) # 3 or 5 w/o Louise
# dawn = scoGetAuthor(c("Sumner"))  # 8
# colin = scoGetAuthor(c("Cameron")) # 13
# joe = scoGetAuthor(c("Dumit"))
# jiming = scoGetAuthor(c("Block"))


# block = scoGetAuthor(c("Block"))

#
function(last, affil = 60014439, first = NA, idOnly = FALSE, curl = getCurlHandle(), key = getOption("ScopusKey", stop("need the scopus API key")), .opts = list())
{
    if(length(last) == 2 && is.na(first)) {
        first = last[2]
        last = last[1]
    }

    q = sprintf("authlastname(%s)", last)
    if(!is.na(affil))
       q = sprintf("%s AND af-id(%s)", q, as.character(affil))

    if(!is.na(first))
       q = sprintf("%s AND authfirst(%s)", q, as.character(first))

    ans = scopusQuery(query = q, url = "http://api.elsevier.com/content/search/author", curl = curl, key = key, .opts = .opts)

    if(idOnly) 
      gsub("^AUTHOR_ID:", "", sapply(ans, `[[`, "dc:identifier"))
    else
       ans
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



getDocInfo.xxx =
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
