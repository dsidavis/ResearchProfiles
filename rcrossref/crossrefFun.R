library(rcrossref)
library(XML)

getConfirmedGSProfileURL = function(name, depth = 50, gscurl = gh)
{
  # This function relies on rCrossRef to identify with confidence a set of articles that belong to the Davis researcher,
  # then looks for a Google Scholar profile associated with the author of those articles.
  # Returns the GS Profile URL, or NA if one doesn't exist or isn't found.
  # Getting doi meta (via getConfirmedUniTitles) is slow, so try first on first five articles, 
  # then the whole first page (20 articles), then page by page to depth.
  noProfLink = 0
  first5 = getConfirmedUniTitles(name, 5)
  gsProf = titlesToGSProfile(first5, name, noProfLink = noProfLink)
  if(gsProf != 'continue')
    return(gsProf)
  
  page = 1
  noProfLink = attr(gsProf, 'noProfLink')
  while(gsProf == 'continue' && page < ceiling(depth / 20)) {
    cat('Found', noProfLink, 'articles without a profile link for', name, '.\n')
    titles = getConfirmedUniTitles(name, page = page, max = 20)
    gsProf = titlesToGSProfile(titles, name, noProfLink = noProfLink)
    noProfLink = noProfLink + attr(gsProf, 'noProfLink')
    page = page + 1
  }
  
  if(gsProf != 'continue')
    return(gsProf)
  
  warning(paste("Didn't find an articles for", name, "with institutional affilaition in crossref,
                or (much less likely), went through all such articles but didn't find a GS page for any of them.
                In any case, didn't find a GS profile, returning NA."))
  return(NA)
}

titlesToGSProfile = function(titles, name, noProfLink, curl = gscurl)
{
  for(title in titles) {
    spurl = paste0('https://scholar.google.com/scholar?as_q=&as_epq="', gsub("\\s", "+", title), 
                   '"&as_oq=&as_eq=&as_occt=any&as_sauthors=', gsub("\\s", "+", name))
    gsArticlePage = htmlParse(getURLContent(spurl, curl = curl))
    q = paste0("//div[@class = 'gs_a']/a[contains(., '", simpleCap(getLastName(name)), "')]/@href")
    node = getNodeSet(gsArticlePage, q)
    if(!is.null(node))  # If find a link on the author's name, that's their GS Profile URL
      return(paste0("https://scholar.google.com", unique(as.character(node))))
    if(length(getNodeSet(gsArticlePage, paste0("//div[@class = 'gs_a' and contains(., '", simpleCap(getLastName(name)), "')]"))) > 0) 
      # If we find the name without a link, they don't have a profile, so exit.
      ## Not true. Dan Gusfield, e.g., has a neuroanatomical pub that crossref
      ## id's as his from Davis, but it's not claimed on his GS profile.
      ## If this happens five times, let's say we exit.
      noProfLink = noProfLink + 1
    if(noProfLink  > 4){
      warning(paste(name, "doesn't appear to have a GS profile. Returning NA."))
      return(NA)
    }
    
  }
  structure('continue', 'noProfLink' = noProfLink)
}
  
getConfirmedUniTitles = function(name, max = 50, institution = "Davis", page = 1)
{
  meta = getDOIMeta(name, max = max, page = page)
  meta = meta[grepl(institution, sapply(meta, getAuthorInst), ignore.case = TRUE)]
  sapply(meta, `[`, 'citation_title')
}

getDOIMeta = function(q, name = q, max = 50, page)
{
  # Originally, this was written with only name and max as argument, with the idea
  # that a name would be queried and should then stay attached to the data.
  # Now want to use this set of functions on individual papers, so adding the q argument
  # to distinguish the two. Author names should probably still be passed along for clarity.
  papers = cr_search(q, rows = max, page = page)
  pages = lapply(papers$doi, function(x) try(getCrossrefMeta(x, author = name), silent = TRUE))
  pages[sapply(pages, class) == "try-error"] = NA
  pages
}

getAuthorInst = function(meta, verbose = FALSE, author = attr(meta, "author"))
{
  if(length(meta) == 1)
    meta = meta[[1]]
  if(!is.character(meta))
    return(NA)
  if(!"citation_author_institution" %in% names(meta)) {
    if(verbose) warning(paste0("No author institution for ", author))
    return(NA)
  }

  authRow = which(grepl(getLastName(author), meta, ignore.case = TRUE) & names(meta) == "citation_author")
  if(length(authRow) == 0) {
    if(verbose) warning(paste0(author, " doesn't seem to be an author of this paper"))
    return(NA)
  }
  
  authInstPos = authRow[names(meta)[authRow + 1] == "citation_author_institution"] + 1
  if(length(authInstPos) == 0) {
    if(verbose) warning(paste0("An institution didn't immediately follow the name ", author))
    return(NA)
  }
  if(length(authInstPos))
    return(meta[authInstPos])
  # If there are multiple author-name matches with institutions, look for one with Davis
  # in the name and, if found, return it (the first if there are multiples).
  # Otherwise, return the modal institution for matches which will very likely be the first one.
  davisInst = grep('davis', meta[authInstPos], ignore.case = TRUE, value = TRUE)
  if(length(davisInst) > 0)
    return(Mode(davisInst))
  warning(paste0('Multiple authors with last name ', getLastName(author), 'found, none w/ Davis affiliation. 
                 Returning modal institution (first position if same number of each).'))
  Mode(meta[authInstPos])
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

getCrossrefMeta = function(doi, author, url = "https://doi.org/") # or dx.doi.org
{
  if(!grepl('^http', doi))
    doi = paste0(url, doi)
  page = try(htmlParse(doi), TRUE)
  if("try-error" %in% attr(page, "class"))
    return(NA)
  doiInfo = getDOIinfo(page)
  if(!(is.character(doiInfo) && "citation_author" %in% names(doiInfo)))
    return(NA)
  structure(doiInfo, author = author)
}


getDOIinfo = function(doc, fields = c("name", "content"))
{
  mat = sapply(fields, function(x) xpathSApply(doc, "//head/meta", xmlGetAttr, x))
  mat[apply(mat, 1:2, function(x) is.null(unlist(x)))] = 'null'
  structure(unlist(mat[, 2]), "names" = unlist(mat[, 1]))
}

