
doc = htmlParse("http://communication.ucdavis.edu/people")

div = getNodeSet(doc, "//div[h4[@class = 'personName']]")

tmp = lapply(div, function(x) {
                data.frame(name = gsub(",[[:space:]]+(PhD|Ph\\.D\\.|M\\.A\\.),?", "", xmlValue(x[["h4"]])),
                           url = as.character(getNodeSet(x, ".//a/@href")[[2]]),
                           title = xmlValue(x[["ul"]], trim = TRUE), stringsAsFactors = FALSE)

             })

comm = as.data.frame(do.call(rbind, tmp), stringsAsFactors = FALSE)

comm = comm[ grepl("Professor|Lecturer", comm$title), ]

save(comm, file = "communication.rda")
