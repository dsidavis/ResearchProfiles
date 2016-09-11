# Institutions identifiers.

x = scopusQuery(query = "affil(Davis California)",  url = "http://api.elsevier.com/content/search/affiliation")
x2 = scopusQuery(query = "affil(Davis AND California)",  url = "http://api.elsevier.com/content/search/affiliation")
x3 = scopusQuery(query = "affil(Davis) AND affil( California)",  url = "http://api.elsevier.com/content/search/affiliation")


sapply(x, `[[`, "affiliation-name")

# Includes 
#  "Univ. of Miami North Research Office"                                                          
# "REGENTS OF THE UNIVERSITY OF CALIFORNIA"
# "Evoluiton and Ecology University of California"                                                

# Many med school...
# [111] "University of California at Davis School of Medicine"                                          
#[112] "University of California at Davis School of Medicine"                                          
#[113] "University of California at Davis School of Medicine"                                          


sapply(x, `[[`, "document-count")

ndoc = as.integer(sapply(x, `[[`, "document-count")) 
w = ndoc > 0
table(w)
# FALSE  TRUE 
#   81   130 
inst = data.frame(name = sapply(x[w], `[[`, "affiliation-name"),
                  id = gsub("AFFILIATION_ID:", "", sapply(x[w], `[[`, "dc:identifier")),
                  ndocs = ndoc[w] )




# Authors

joe = getAuthorID(authlast = "Dumit", authfirst = "joseph")

getAuthorID(AFFIL = "Davis", authlast = "Marx",  AUTHFIRST = "john", SUBJABBR = "ARTS")

mx = scopusQuery(query = "AFFIL(Davis) and authlast(Marx) and AUTHFIRST(john) and SUBJABBR(ARTS)", url = "http://api.elsevier.com/content/search/author")



joe.docIDs = getAuthorDocsIds(joe)
#   @_fa                                                      prism:url         dc:identifier
#1  true  http://api.elsevier.com/content/abstract/scopus_id/0033244314  SCOPUS_ID:0033244314
#2  true http://api.elsevier.com/content/abstract/scopus_id/84980373484 SCOPUS_ID:84980373484
#3  true  http://api.elsevier.com/content/abstract/scopus_id/2342513539  SCOPUS_ID:2342513539
#4  true http://api.elsevier.com/content/abstract/scopus_id/84877826780 SCOPUS_ID:84877826780
#5  true http://api.elsevier.com/content/abstract/scopus_id/77955940728 SCOPUS_ID:77955940728
#6  true http://api.elsevier.com/content/abstract/scopus_id/29144502417 SCOPUS_ID:29144502417
#7  true http://api.elsevier.com/content/abstract/scopus_id/84887765235 SCOPUS_ID:84887765235
#8  true http://api.elsevier.com/content/abstract/scopus_id/84901834408 SCOPUS_ID:84901834408
#9  true http://api.elsevier.com/content/abstract/scopus_id/84899148897 SCOPUS_ID:84899148897
#10 true http://api.elsevier.com/content/abstract/scopus_id/84917513989 SCOPUS_ID:84917513989
#11 true http://api.elsevier.com/content/abstract/scopus_id/84960516932 SCOPUS_ID:84960516932
#12 true http://api.elsevier.com/content/abstract/scopus_id/84885549753 SCOPUS_ID:84885549753
#13 true http://api.elsevier.com/content/abstract/scopus_id/84917356214 SCOPUS_ID:84917356214
#14 true http://api.elsevier.com/content/abstract/scopus_id/84955147628 SCOPUS_ID:84955147628
#15 true http://api.elsevier.com/content/abstract/scopus_id/33748313191 SCOPUS_ID:33748313191
#16 true http://api.elsevier.com/content/abstract/scopus_id/84880677618 SCOPUS_ID:84880677618
#17 true http://api.elsevier.com/content/abstract/scopus_id/84860763458 SCOPUS_ID:84860763458
#18 true http://api.elsevier.com/content/abstract/scopus_id/84917383258 SCOPUS_ID:84917383258
#19 true http://api.elsevier.com/content/abstract/scopus_id/34547299594 SCOPUS_ID:34547299594
#20 true http://api.elsevier.com/content/abstract/scopus_id/84878912364 SCOPUS_ID:84878912364
#21 true http://api.elsevier.com/content/abstract/scopus_id/84867345337 SCOPUS_ID:84867345337


i = getDocInfo(joe.docIDs[1,3])
names(i)
#[1] "coredata"      "authors"       "language"      "authkeywords"  "idxterms"      "subject-areas" "item"         
i$"subject-areas"

names(i$coredata)
# [1] "prism:url"             "dc:identifier"         "eid"                   "dc:title"              "prism:aggregationType" "srctype"               "citedby-count"         "prism:publicationName" "source-id"             "prism:issn"           
#[11] "prism:volume"          "prism:issueIdentifier" "prism:startingPage"    "prism:endingPage"      "prism:pageRange"       "prism:coverDate"       "dc:creator"            "dc:description"        "intid"                 "link"                 

length(i$item[[2]][[3]][[1]][[2]])
# This is the bibliography.


joeDocs = getAuthorDocs(c("Dumit", "Joseph"))








#Serial Title API

jcgs = scopusQuery(title = "journal of computational and graphical statistics", url = "http://api.elsevier.com/content/serial/title")
jcgs.by.issn = scopusQuery(issn = "1061-8600", url = "http://api.elsevier.com/content/serial/title")

agri = scopusQuery(subj = "AGRI", oa = "all", url = "http://api.elsevier.com/content/serial/title")


# Subject Classifications
scopusQuery( url = "http://api.elsevier.com/content/subject/scopus")
