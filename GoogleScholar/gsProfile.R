####### This is unfinished ########

source("googleScholar.R")
source("../search/loadFaculty.R")
source("run.R")  # For the curl handle

gsPage1 = 
  function(q, url = GoogleScholarBaseURL, curl = gh)
  {
    o = getForm(url, hl = "en", q = q, btnG= "", as_sdt = "1%2C5", as_sdtp = "", binary = TRUE, curl = curl)
    ans = rawToChar(o)
    htmlParse(ans, asText = TRUE)
  }

gsProfile = function(name)
{
  lastName = if(grepl(",", name)) { 
    strsplit(name, ",")[[1]][1] 
  } else {
    x = strsplit(name, "\\s")[[1]]
    x[length(x)]
  }
  gsProfTest = paste0("//div[@class = 'gs_a']/a[@href and contains(.,'", lastName, "')]/@href")
  Mode(unlist(getNodeSet(gsPage1(name), gsProfTest, 
                                  fun = function(x) paste0("https://scholar.google.com", as.character(x))
  )))
}

getProfileArticles =
  function(doc)
  {
    nodes = getNodeSet(doc, "//td[@class = 'gsc_a_t']")
    lapply(nodes, processArticle)
  }

if(FALSE)
{
fac = loadFaculty()

areProfiles = unlist(sapply(fac[[1]]$name, gsProfile))  # excludes faculty w/o a profile
arePages = 
  lapply(areProfiles, function(url) 
    htmlParse(getURLContent(url, curl = gh)))
sink("gsprof.txt", split = TRUE, append = FALSE)
arePages[[1]]
sink()

test = getNodeSet(arePages[[1]], "//td[@class = 'gsc_a_t']")
processArticle(test[[1]])
test = getProfileArticles(arePages[[1]])
}
