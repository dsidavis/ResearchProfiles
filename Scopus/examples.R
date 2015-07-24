source("scopus.R")

if(!length(getOption("ScopusKey")))
  message('Set the option(ScopusKey = "your api key")')


o = scoAffiliation("Davis")     #  , .opts = list(verbose = TRUE))

names(o)
names(o[[1]])

#[1] "opensearch:totalResults" "opensearch:startIndex"  
#[3] "opensearch:itemsPerPage" "opensearch:Query"       
#[5] "link"                    "entry"                  

length(o[[1]]$entry)

