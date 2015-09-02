library(XML)
library(RCurl)
library(aRxiv)

snowballArxiv = function(seed = attr(prior, 'seed'), rounds = 1, prior = NULL)
{
  # Prior is previous snowballed network, to be built on. 
  # If prior is passed, rounds is additional number rounds to add.
  if(!is.null(prior)) {
    edgelist = prior
    doRounds = (attr(prior, 'snowballRounds') + 1):(attr(prior, 'snowballRounds') + rounds)
    names = unique(edgelist$source)
  } else {
    names = seed
    edgelist = getArxivCollaborators(seed, FALSE)
    doRounds = 1:rounds
  }

  for(i in doRounds)
  {
    newNames = unique(edgelist$collabs[!edgelist$collabs %in% names])
    names = c(names, newNames)
    edgelist = rbind(edgelist, 
                     do.call(rbind, lapply(newNames, function(n) {
                       cat('Searching arxiv for', n, '\n\t', length(newNames) - which(newNames==n), 
                           'names remain in round', i, 'of', max(doRounds), '\n')
                       getArxivCollaborators(n)
                     }))
    )
  }
  structure(edgelist, "seed" = seed, "snowballRounds" = max(doRounds))
}

getArxivCollaborators = function(name, removeMiddleInitial = FALSE)
  # Takes an author, returns a data.frame with their arxiv pubs. 
  # Middle initial handling matters
  # Note that we could snowball to an arbitrary length of connections here.
{
  name = tolower(name)
  initialQ = "\\s[a-z]\\.?\\s"
  if(removeMiddleInitial) 
    name = gsub(initialQ, " ", name)
  arts = arxiv_search(paste0('au:\"', nameForArxiv(name), '\"'))
  if(!nrow(arts))  # If no articles found, break
    return(NULL)
  collab = strsplit(tolower(arts$authors), "\\|", perl = TRUE)
  if(removeMiddleInitial)
    collab = lapply(collab, function(x) gsub(initialQ, " ", x))
  collab = unlist(lapply(collab, function(x) x[x != name]))  # This may need to be more flexible
  colTab = as.data.frame(table(collab), stringsAsFactors = FALSE)
  if(!nrow(colTab))  # Not sure how this can happen with the break above, but it does
    return(NULL)
  structure(cbind(name, colTab), 
            names = c("source", "collabs", "count"))
}

nameForArxiv = function(names) 
{
  names = replaceForeignChar(names)  
  names = gsub('\\.', '', names)
  names = strsplit(names, '\\s')
  sapply(names, function(x) {
    if(length(x) == 1) return(x)
    paste0(x[length(x)], '_', paste(x[1:(length(x) - 1)], collapse = '_'))
  })
}

# Pairs from http://stackoverflow.com/questions/20495598/; had to add the first (seemingly common but omitted -- other common chars missing?)
# Could be made comprehensive by scraping: https://en.wikipedia.org/wiki/List_of_Latin-script_letters#Letters_with_diacritics
replaceForeignChar = function(char)
{
  tmp = c('ü'='u',
          'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='B', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  chartr(paste(names(tmp), collapse = ''), paste(tmp, collapse = ''), char)
}

# Probably want to get institutions separately, but leaving this here:
getArxivCollabAndInst = function(name, ...)
{
  col = getArxivCollaborators(name, ...)
  col$uni = sapply(col$name, affiliationByGoogle)
  col
}

if(FALSE)
{
  gh2 = getCurlHandle(cookiejar = "", useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:39.0) Gecko/20100101 Firefox/39.0",
                     verbose = TRUE, followlocation = TRUE,
                     httpheader = c(Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                                    'Accept-Language' = "en-US,en;q=0.5"  # ,'Accept-Encoding' = "gzip, deflate"
                     ))
  invisible(getURLContent("http://www.google.com/search", curl = gh2))

  source("affiliation/googleAffiliation.R")
  dtl = getArxivCollabAndInst("duncan temple lang")
  coop = getArxivCollabAndInst("graham coop")
}

# We could get name's collaborators' articles and snowball out any length:
if(FALSE){
  collabsArts =
    lapply(collab, function(paper) lapply(paper, function(auth) 
      arxiv_search(
        parse(text = paste0("'au:\"", auth, "\"'")))
    ))
}  

if(FALSE)
{
# There is a link to author's email on the abstract page, but it requires login.
# Holding off on going down that road for now. 
curl = getCurlHandle()
dtl1 = getURLContent(dtl[1, "link_abstract"], curl = curl)
dtl1Abs = htmlParse(readLines(url(dtl[1, "link_abstract"])))
emailLink = xpathSApply(dtl1Abs, "//div[@class = 'submission-history']/a", xmlGetAttr, "href")
}