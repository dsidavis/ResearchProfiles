library(XML)
library(RCurl)
library(aRxiv)

getArxivCollaborators = function(name, removeMiddleInitial = TRUE)
  # Takes an author, returns a data.frame with their arxiv pubs. 
  # Note that we could snowball to an arbitrary length of connections here.
{
  name = tolower(name)
  initialQ = "\\s[a-z]\\.?\\s"
  if(removeMiddleInitial) 
    name = gsub(initialQ, " ", name)
  arts = arxiv_search(paste0('au:\"', name, '\"'))
  collab = strsplit(tolower(arts$authors), "\\|", perl = TRUE)
  if(removeMiddleInitial)
    collab = lapply(collab, function(x) gsub(initialQ, " ", x))
  collab = unlist(lapply(collab, function(x) x[x != name]))  # This will need to be more flexible
  structure(as.data.frame(table(collab)), names = c("name", "count"))
}


gh = getCurlHandle(cookiejar = "", useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:39.0) Gecko/20100101 Firefox/39.0",
                   verbose = TRUE, followlocation = TRUE,
                   httpheader = c(Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                                  'Accept-Language' = "en-US,en;q=0.5"  # ,'Accept-Encoding' = "gzip, deflate"
                   ))
invisible(getURLContent("http://www.google.com/search", curl = gh))

getArxivCollabAndInst = function(name, ...)
{
  col = getArxivCollaborators(name, ...)
  col$uni = sapply(col$name, affiliationByGoogle)
  col
}

if(FALSE)
{
  source("affiliation/googleAffiliation.R")
  dtl = getArxivCollabAndInst("duncan temple lang")
  coop = getArxivCollabAndInst("graham coop")
}

# We could get name's collaborators' articles and snowball out any length:
if(FALSE){
  collabsArts =
    lapply(collab, function(paper) lapply(paper, function(auth) 
      arxiv_search(
        parse(text = paste0("'au:\"", auth, "\"'")))
    ))
}  

if(FALSE)
{
# There is a link to author's email on the abstract page, but it requires login.
# Holding off on going down that road for now. 
curl = getCurlHandle()
dtl1 = getURLContent(dtl[1, "link_abstract"], curl = curl)
dtl1Abs = htmlParse(readLines(url(dtl[1, "link_abstract"])))
emailLink = xpathSApply(dtl1Abs, "//div[@class = 'submission-history']/a", xmlGetAttr, "href")
}