library(rcrossref)
library(XML)

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
  if(!(is.matrix(doiInfo) && "citation_author" %in% doiInfo[, 1]))
    return(NA)
  structure(doiInfo, author = author)
}

getAuthorInst = function(meta, author = attr(meta, "author"))
{
  if(!is.matrix(meta))
    return(NA)
  if(!"citation_author_institution" %in% meta[, 1]) {
    warning(paste0("No author institution for ", author))
    return(NA)
  }
  authRow = which(grepl(getLastName(author), meta[, 2], ignore.case = TRUE) & meta[, 1] == "citation_author")
  if(length(authRow) == 0) {
    warning(paste0(author, " doesn't seem to be an author of this paper"))
    return(NA)
  }
  if(meta[authRow + 1, 1] != "citation_author_institution") {
    warning(paste0("author's institution didn't immediately follow ", author))
    return(NA)
  }
  unlist(meta[authRow + 1, 2])
}

getDOIinfo = function(doc, fields = c("name", "content"))
{
  sapply(fields, function(x)
    xpathSApply(doc, "//head/meta", xmlGetAttr, x))
}
