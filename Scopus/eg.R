library(RCurl)
library(RJSONIO)

doi = "10.1111/j.2041-210X.2012.00247.x"
txt = getDoc(doi)



# This doesn't work. It is from the federated search page, but the URL is incorrect
#txt = getForm("http://api.elsevier.com/content/search/index:SCOPUS", query = "visualization", view = "COMPLETE",
#               .opts = list(httpheader = c('X-ELS-APIKey' = getOption("ScopusKey"))))



Scopus Search
txt = getForm("http://api.elsevier.com/content/search/scopus", query = "visualization",  #view = "COMPLETE",
               .opts = list(httpheader = c('X-ELS-APIKey' = getOption("ScopusKey"))))

# count can be set more than 25?


Affiliation search
getForm("http://api.elsevier.com/documentation/AFFILIATIONSearchAPI.wadl", 


id = "60014439"        
http://api.elsevier.com/content/affiliation/

