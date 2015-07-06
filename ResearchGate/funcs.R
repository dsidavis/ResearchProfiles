library(XML)
library(RCurl)


#### Need to get the current researchers RG identifier and to authors.
####  Currently, this author is omitted from the author list!


rg =
function(name, max = NA)
{
    u = sprintf('https://www.researchgate.net/profile/%s/publications', gsub(" ", "_", name))

    ans = list()
    while(is.na(max) || length(ans) < max) {
        doc = htmlParse(getURLContent(u, curl = curl), asText = TRUE)
        ans = c(ans, xpathApply(doc, "//div[./h5[@class = 'pub-type-and-title']]/..", getInfo))
        u = getNextPage(doc)
        if(length(u) == 0)
            break
    }
    
    ans
}

getNextPage =
function()
{

}


getInfo =
function(node)
{
   list( title = getTitle(node),
         authors = getAuthors(node),
         type = getPubType(node))
}


getAuthors =
function(node)
{
   xpathSApply(node, ".//div[@class = 'authors']/a", getAuthor)
}

getAuthor =
function(node)
{
  structure(xmlValue(node), names = xmlGetAttr(node, "href"))
}

getPubType =
function(node)
{
   xpathSApply(node, ".//span[@class = 'publication-type']", xmlValue)
}

getTitle =
function(node)
   xpathSApply(node, ".//span[contains(@class, 'publication-title')]", xmlValue)    
