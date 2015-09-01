source("../GoogleScholar/googleScholar.R")
source("../GoogleScholar/run.R")
source("../search/loadFaculty.R")
source("../utilityFuns.R")
library(httr)


#s = lapply(nameSample, function(dep) lapply(dep, googleScholar, max = 100))  # pisses Google off
urls = lapply(s, 
              function(dep) unlist(lapply(dep, 
                                          function(pers) sapply(pers, 
                                                                function(art) parse_url(art$link)$hostname))))

# Sometimes we get lucky and the author's institution is in crossref's metadata:
q = 'Aluminum-mycorrhizal interactions in the physiology of pitch pine seedlings'
author = 'jonathan cumming'
getAuthorInst(getDOIMeta(q = q, name = author, max = 1), verbose = TRUE)

scienceDirectAffiliation = function(sdLink, author)
{
  # Need to handle multiple matches of last name in authors at some point
  doc = htmlParse(getURLContent(sdLink))
  q = paste0("//a[@class = 'authorName svAuthor' and @data-ln = '", 
             simpleCap(getLastName(author)), 
             "']/../a[@title]")
  affs = xpathApply(doc, q, function(x) xmlAttrs(x)['href'])
  affs = affs[grepl("#", affs)]  # Elimates emails
  affs = gsub("#", "", affs)
  sapply(affs, function(a) 
    xpathApply(doc, paste0("//li[@id = '", a, "']/span"), xmlValue)[[1]])
}


if(FALSE)
{
author = "duncan temple lang"
dtl = googleScholar(author, 20)
sdLinks = unlist(lapply(dtl, function(x) grep("sciencedirect", x$link, value = TRUE)))
scienceDirectAffiliation(sdLinks[[1]], author)

author = "mark lubell"
ml = googleScholar(author, 50)
sdLinks = unlist(lapply(ml, function(x) grep("sciencedirect", x$link, value = TRUE)))
lapply(sdLinks, function(x) try(scienceDirectAffiliation(x, author)))

}


### Just begun...
if(FALSE)
{
# tandfAffiliation(dtl[[2]]$link, author)
tfLinks = unlist(lapply(dtl, function(x) grep("tandf", x$link, value = TRUE)))
unlist(lapply(dtl, function(x) x$link))



tandfAffiliation = function(tandfLink, author)
{
  doc = htmlParse(getURLContent(tandfLink))
}

}