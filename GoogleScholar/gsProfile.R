source("googleScholar.R")
source("../affiliation/googleAffiliation.R")
source("../utilityFuns.R")
source("run.R")  # For the curl handle

getProfileArticles = function(author, maxArticles = 1e3, theCurl = gh)
{
  # For well-formated journal articles "other" is journal name, volume, issue, pages, and year
  gsURL = getProfileURL(author)

  # In case Google has stopped returning results
  if(is.null(gsURL))
    return(NULL)
  
  # In case author doesn't have a profile
  if(is.na(gsURL))
    return(NA)

  start = 0
  articles = list()
  while(start < maxArticles) 
  {
    if(start > 0)
    curlSetOpt(curl = theCurl, referer = url)
    url = paste0(gsURL, "&cstart=", start, "&pagesize=100")
    doc = htmlParse(getURLContent(url, curl = theCurl))
    if(xmlValue(getNodeSet(doc, "//tbody/tr/td")[[1]]) == "There are no articles in this profile.")
      break
    articles = c(articles, digestProfileArticles(doc))
    start = start + 100
    Sys.sleep(runif(1, .1, 1))  # Pause to try to throw google off. Maybe need longer pause?
  }
  curlSetOpt(curl = theCurl, referer = "https://scholar.google.com/")
  do.call(rbind, articles)
}

getProfileURL = function(name)
{
  lastName = getLastName(name)
  gsSearchResult = gsPage1(name)
  # Break if Google is onto us
  if("Please show you're not a robot" %in% xpathApply(gsSearchResult, "//h1", xmlValue))
    return(NULL)

  # This works for one profile
  test2 = '//h3[@class = "gs_rt"]/following-sibling::table/tr/td/div/text()'
  if(any(
      xpathSApply(gsSearchResult, test2, function(x) 
        grepl('Verified email at ucdavis.edu', xmlValue(x)))
  ))
    return(
      xpathApply(gsSearchResult, 
                 paste0('//h3[@class = "gs_rt" and contains(., "', name, '")]/following-sibling::table//a[@href]'),
                 fun = function(x) paste0("https://scholar.google.com", xmlAttrs(x)))[[1]])
  
  # This works for multiple profiles of one name:
  test = '//span[@class="gs_nph" and contains(., "Verified email at ucdavis.edu")]'
  sibs = getNodeSet(gsSearchResult, test)
  if(length(sibs) == 0)
    return(NA)
  container = getSibling(sibs[[1]], after = FALSE)
  xpathApply(container, './a[@href]', 
             fun = function(x) paste0("https://scholar.google.com", xmlAttrs(x)))[[1]]
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