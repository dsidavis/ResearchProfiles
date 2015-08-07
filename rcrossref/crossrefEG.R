source("crossrefFun.R")
source("../affiliation/googleAffiliation.R")
source("../search/loadFaculty.R")

getConfirmedUniTitles = function(name, max = 50, institution = "Davis")
{
  meta = getDOIMeta(name, max)
  instPapers = meta[grepl(institution, sapply(meta, getAuthorInst), ignore.case = TRUE)]
  sapply(instPapers, function(x) x[, 2][x[, 1] == "citation_title"])
}
dtlTitles = getConfirmedUniTitles("duncan temple lang", 12)
mnTitles = getConfirmedUniTitles("meredith niles", 22)

fac = loadFaculty()
# Get some papers for faculty that we can confidently assign to the UCD person:
set.seed(123)
oneEachFac = lapply(fac, function(x) as.character(x$name[[sample(length(x$name), 1)]]))
# get (max) 10 papers' meta info for each:
meta10 = lapply(oneEachFac, getCrossrefMeta, 10)
# Some faculty don't have any articles indexed on crossref and available from off campus
### Suspect this will be better on campus
# For each paper, get the querried author's institution:
davisPapers = 
  lapply(meta10, function(facArts) {
    if(length(facArts) == 0)
      return(NA)
    inst = sapply(facArts, getAuthorInst)
    grepl("davis", tolower(inst))
  })


# Try to get the first 5 articles for the first three faculty in each department
# Takes several minutes
articles2 = 
  lapply(fac, function(x) 
    lapply(x$name[1:3], function(person) 
      getCrossrefMeta(person, max = 5)))

# Extract each article's authors and their institutions, where available. Otherwise, let google guess.
# This is rough in several ways, e.g. it takes authors and institutions sequentially, doesn't
# match them. There are at least occassionally different numbers of the two.
collabs = 
  lapply(articles, function(dept) 
    lapply(dept, function(fac)
      lapply(fac, function(article) 
        if(!is.matrix(article)) {
          NA
        } else {
          authors = article[, 2][article[, 1] == "citation_author"]
          if("citation_author_institution" %in% article[, 1]) { 
            institutions = article[, 2][article[, 1] == "citation_author_institution"]
          } else {
            institutions = sapply(authors, affiliationByGoogle)
          }
          data.frame(authors = authors, uni = institutions)
        }
      )))

