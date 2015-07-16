library(RCurl)
library(XML)

GoogleScholarBaseURL = "http://scholar.google.com/scholar"
googleScholar = 
function(q, max = NA, url = GoogleScholarBaseURL, curl = getCurlHandle(...), ...)
{    
   o = getForm(url, hl = "en", q = q, btnG= "", as_sdt = "1%2C5", as_sdtp = "", binary = TRUE, curl = curl)

   articles = list()
   
   while(is.na(max) || length(articles) < max) {

     ans = rawToChar(o)
     doc =  htmlParse(ans, asText = TRUE)

     articles = c(articles, getArticles(doc = doc))
       
     u = nextPage(doc)
     if(length(u) == 0)
        break
     
     o = getURLContent(u, curl = curl, binary = TRUE)
   }

   articles
}

getArticles =
function(txt, doc = htmlParse(txt))
{
   nodes = getNodeSet(doc, "//div[@class = 'gs_ri']")
   lapply(nodes, processArticle)
}

processArticle =
function(node)
{
  tiNode =  getNodeSet(node, ".//h3[@class = 'gs_rt']")[[1]]
  ti = xmlValue(tiNode)
  a = getNodeSet(tiNode, ".//a")
  if(length(a))
      link = xmlGetAttr(a[[1]], "href")
  else
      link = ""
  
  authors = getNodeSet(node, ".//div[@class = 'gs_a']")[[1]]

  authors = separateAuthors(authors)
  
  list(title = ti, authors = authors, link = link)
}

separateAuthors =
function(node)
{
  a = getNodeSet(node, ".//a")
  if(length(a))
    ans = structure(sapply(a, xmlValue), names = getUserID(sapply(a, xmlGetAttr, "href")))
  else
    ans = character()
  removeNodes(a)
  txt = xmlValue(node)

  c(ans, txt)
}

getUserID =
    # x is something like "/citations?user=tQVe-fAAAAAJ&amp;hl=en&amp;oi=sra"
function(x)
{
   els = getFormParams(gsub("^/citations?", "", x))
   els["user"]
}

nextPage =
function(doc)
{
  ans =  getNodeSet(doc, "//a[contains(.//b, 'Next')]/@href")
  if(length(ans) == 0)
    character()
  else
    getRelativeURL(ans[[1]], GoogleScholarBaseURL)
}


if(FALSE) {
dp = googleScholar(q = "Debashis Paul")

}
