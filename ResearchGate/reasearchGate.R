library(XML)
library(RCurl)


# We need to login to ResearchGate. So read their front page to get the login form
# and create a corresponding R function.
library(RHTMLForms)
frms = getHTMLFormDescription('http://www.researchgate.net/')
login = createFunction(frms[[1]])


# We will login using this function  and make certain to use the same
# curl handle for all requests. The login will cause the cookie to be set in the handle
# and then when we use this in subsequent requests, the cookie will be resent.
curl = getCurlHandle(cookiejar = "", followlocation = TRUE)
con = login("dtemplelang@ucdavis.edu", ResearchGatePassword, .curl = curl)


# Now we can query authors. Let's look at Carl Boettiger.
u = 'https://www.researchgate.net/profile/Carl_Boettiger/publications'
doc = htmlParse(getURLContent(u, curl = curl), asText = TRUE)

a = getNodeSet(doc, "//div[./h5[@class = 'pub-type-and-title']]/..")
sapply(a, function(x) xpathSApply(x, ".//span[contains(@class, 'publication-title')]", xmlValue))


info = xpathApply(doc, "//div[./h5[@class = 'pub-type-and-title']]/..", getInfo)




