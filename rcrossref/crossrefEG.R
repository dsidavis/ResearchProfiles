source("crossrefFun.R")
source("../affiliation/googleAffiliation.R")
source("../search/loadFaculty.R")

# Try to get the first 5 articles for the first three faculty in each department
# Takes several minutes
fac = loadFaculty()
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

