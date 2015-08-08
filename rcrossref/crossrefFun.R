library(rcrossref)
library(XML)

getConfirmedGSProfileURL = function(name, depth = 50)
{
  # This function relies on rCrossRef to identify with confidence a set of articles that belong to the Davis researcher,
  # then looks for a Google Scholar profile associated with the author of those articles.
  # Returns the GS Profile URL, or NA if one doesn't exist or isn't found.
  
  titles = getConfirmedUniTitles(name, depth)
  if(length(titles) == 0) {
    warning(paste("Didn't find any articles for", name, "with institutional affilaition in crossref. Returning NA."))
    return(NA)
  }
  for(title in titles) {
    spurl = paste0('https://scholar.google.com/scholar?as_q=&as_epq="', gsub("\\s", "+", title), 
                   '"&as_oq=&as_eq=&as_occt=any&as_sauthors=', gsub("\\s", "+", name))
    gsArticlePage = htmlParse(getURLContent(spurl))
    q = paste0("//div[@class = 'gs_a']/a[contains(., '", simpleCap(getLastName(name)), "')]/@href")
    node = getNodeSet(gsArticlePage, q)
    if(!is.null(node))  # If find a link on the author's name, that's their GS Profile URL
      return(paste0("https://scholar.google.com", as.character(node)))
    if(length(getNodeSet(gsArticlePage, paste0("//div[@class = 'gs_a' and contains(., '", simpleCap(getLastName(name)), "')]"))) > 0) {
      # If we find the name without a link, they don't have a profile, so exit
      warning(paste(name, "doesn't appear to have a GS profile. Returning NA."))
      return(NA)
    }
  }
  warning(paste("Went through all the titles for", name, "but didn't find a GS page for any of them. Weird. Returning NA."))
  NA
}

getDOIMeta = function(name, max = 50)
{
  papers = cr_search(name, rows = max)
  pages = lapply(papers$doi, function(x) try(getCrossrefMeta(x, author = name), silent = TRUE))
  pages[sapply(pages, class) == "try-error"] = NA
  pages
}

getCrossrefMeta = function(doi, author)
{
  page = try(htmlParse(doi), TRUE)
  if("try-error" %in% attr(page, "class"))
    return(NA)
  doiInfo = getDOIinfo(page)
  if(!(is.character(doiInfo) && "citation_author" %in% names(doiInfo)))
    return(NA)
  structure(doiInfo, author = author)
}

getAuthorInst = function(meta, verbose = FALSE, author = attr(meta, "author"))
{
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
  if(names(meta)[authRow + 1] != "citation_author_institution") {
    if(verbose) warning(paste0("An institution didn't immediately follow the name ", author))
    return(NA)
  }
  meta[authRow + 1]
}

getDOIinfo = function(doc, fields = c("name", "content"))
{
  mat = sapply(fields, function(x) xpathSApply(doc, "//head/meta", xmlGetAttr, x))
  structure(unlist(mat[, 2]), "names" = mat[, 1])
}

getConfirmedUniTitles = function(name, max = 50, institution = "Davis")
{
  meta = getDOIMeta(name, max)
  meta = meta[grepl(institution, sapply(meta, getAuthorInst), ignore.case = TRUE)]
  sapply(meta, `[`, 'citation_title')
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}