source("googleScholar.R")
source("../affiliation/googleAffiliation.R")
source("../search/loadFaculty.R")
source("../utilityFuns.R")
source("run.R")  # For the curl handle

getProfileArticles = function(author, maxArticles = 1e3, theCurl = gh)
{
  # For well-formated journal articles "other" is journal name, volume, issue, pages, and year
  gsURL = getProfileURL(author)
  start = 0
  articles = list()
  while(start < maxArticles) 
  {
    url = paste0(gsURL, "&cstart=", start, "&pagesize=100")
    doc = htmlParse(getURLContent(url, curl = theCurl))
    if(xmlValue(getNodeSet(doc, "//tbody/tr/td")[[1]]) == "There are no articles in this profile.")
      break
    articles = c(articles, digestProfileArticles(doc))
    start = start + 100
  }
  do.call(rbind, articles)
}

getProfileURL = function(name)
{
  lastName = getLastName(name)
  gsSearchResult = gsPage1(name)
  gsProfTest = paste0("//div[@class = 'gs_a']/a[@href and contains(.,'", lastName, "')]/@href")
  Mode(unlist(
    getNodeSet(gsSearchResult, gsProfTest, 
               fun = function(x) paste0("https://scholar.google.com", as.character(x)))
  ))
}

gsPage1 = 
  function(q, url = GoogleScholarBaseURL, curl = gh)
  {
    o = getForm(url, hl = "en", q = q, btnG= "", as_sdt = "1%2C5", as_sdtp = "", binary = TRUE, curl = curl)
    ans = rawToChar(o)
    htmlParse(ans, asText = TRUE)
  }

digestProfileArticles = function(page)
{
  articles = getNodeSet(page, "//td[@class = 'gsc_a_t']")
    lapply(articles, function(art){
      title = xmlValue(getNodeSet(art, "./a")[[1]])
      info = getNodeSet(art, "./div[@class = 'gs_gray']")
      authors = xmlValue(info[[1]])
      otherInfo = xmlValue(info[[2]])
      c(title = title, authors = authors, other = otherInfo)
  })
}

if(FALSE)
{
  fac = loadFaculty()
  are1 = getProfileArticles(fac$are$name[[1]], max = 150)
  head(are1)
}