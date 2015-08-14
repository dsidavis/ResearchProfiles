affiliationByGoogle = function(q, returnMode = TRUE, curl = getCurlHandle(...)) 
{
  # q is a character string, or vector thereof, to send to google search
  # Returns a vector of the .edu urls returned on google's first page for that query
  # or the most common of those, if returnMode = TRUE
  if(length(q) > 1)
    q = paste(q, collapse = "+")
  q = gsub(" ", "+", q)
  qURL = paste0("http://www.google.com/search?hl=en&lr=&ie=ISO-8859-1&q=", q, "&btnG=Search")
  doc = htmlParse(getURLContent(qURL, curl = curl), asText = TRUE)
  eduLinks = grep(".edu", getHTMLLinks(doc), value = TRUE)
  if(length(eduLinks) == 0)  # Note that this will miss some international scholars
      return(NA)
  insts = pullEduURLs(eduLinks)
  if(returnMode)
    return(Mode(insts))  # Tie goes to the highest ranked page
  insts
}

pullEduURLs = function(eduLinks)
{
  x = strsplit(eduLinks, NULL)
  x = lapply(x, function(l) paste(rev(l), collapse = ""))
  trimmed = substr(x, unlist(gregexpr("ude.", x)) + 4, nchar(x))
  revd = substr(trimmed, 1, regexpr("[^a-z]", trimmed) - 1)
  sapply(revd, function(l) paste(rev(strsplit(l, NULL)[[1]]), collapse = ""))
}