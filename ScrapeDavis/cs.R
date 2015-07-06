library(XML)


# Scrape the list of names and URLs
u = 'http://www.cs.ucdavis.edu/people/faculty/'
ll = getHTMLLinks(u)
  # Need to filter these to discard generic links.


# But there is another page of faculty and research areas
u = 'http://www.cs.ucdavis.edu/people/faculty/research-areas/'

doc = htmlParse(u)
p = getNodeSet(doc, "//p")


info = 
function(x)
{
  a = getNodeSet(x, ".//a")
  ans = if(length(a))  {
     a = a[[1]]
    list(name = xmlValue(a),
         url = xmlGetAttr(a, "href"),
         title = gsub("^, ", "", xmlValue(getNodeSet(a, ".//following-sibling::text()")[[1]])))

  } else {
     tmp = xmlValue(getNodeSet(x, ".//br/preceding-sibling::text()")[[1]])
     e = strsplit(tmp, ", ")[[1]]
     list(name = e[[1]], url = NA, title = e[[2]])
  }

  ans$areas = XML:::trim(xpathSApply(x, ".//br/following-sibling::text()", xmlValue))
  ans
}

ra = as.data.frame(do.call(rbind, lapply(p, info)), stringsAsFactors = FALSE)



if(FALSE) {
     # if they all had a <a> </a> element, but they don't
ra = data.frame(name = sapply(p, function(x) xmlValue(getNodeSet(x, ".//a")[[1]])))
ra$url = sapply(p, function(x) getNodeSet(x, ".//a/@href"))
ra$areas = sapply(p, function(x) xmlValue(getNodeSet(x, ".//br/following-sibling::text()")))
}
