source("../affiliation/googleAffiliation.R")
source("arxiv.R")
source("../utilityFuns.R")
gh2 = getCurlHandle(cookiejar = "", useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:39.0) Gecko/20100101 Firefox/39.0",
                    verbose = TRUE, followlocation = TRUE,
                    httpheader = c(Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                                   'Accept-Language' = "en-US,en;q=0.5"  # ,'Accept-Encoding' = "gzip, deflate"
                    ))
invisible(getURLContent("http://www.google.com/search", curl = gh2))
curlSetOpt(curl = gh2, referer = "http://www.google.com/search")

if(FALSE) {
dtl1 = snowballArxiv('duncan temple lang', 1)
system.time({ # 2'
  dtl2 = snowballArxiv(prior = dtl1)
})
system.time({ # 18'
  dtl3 = snowballArxiv(prior = dtl2)
})
saveRDS(dtl3, 'dtlNet3Layers.RDS')
}

dtl3 = readRDS('dtlNet3Layers.RDS')
coop3 = readRDS('coopNet3Layers.RDS')

library(network)
dtlNet = network(dtl3, 
                 matrix.type = 'edgelist',
                 directed = FALSE,
                 ignore.eval = FALSE,
                 names.eval = 'numberPapers')

coopNet = network(coop3, 
                  matrix.type = 'edgelist',
                  directed = FALSE,
                  ignore.eval = FALSE,
                  names.eval = 'numberPapers')

if(FALSE)
{
system.time({  # This takes about .5" each, so ~45' for dtl.
  dtlNet %v% 'institution' = sapply((dtlNet %v% 'vertex.names'), affiliationByGoogle)
})

aff = data.frame(node = unique(dtl3$source), affiliation = "")
aff$affiliation = 
  sapply(aff$node, function(x) {
    if(sample(10, 1)==10)
      Sys.sleep(runif(1, 3, 5))  # This doesn't fix it, but Google seems to remove
    # the block quite quickly. So maybe IDing when it happens and then sleeping
    # long enough to reset would work
    affiliationByGoogle(x)
    })
dtlNet %v% 'institution' = aff$affiliation[match(dtlNet %v% 'vertex.names', aff$node)]
}

head((dtlNet %v% 'vertex.names')[order(evcent(dtlNet), decreasing = TRUE)])

dtlNet %v% 'isD' = dtlNet %v% 'vertex.names' == 'duncan temple lang'

png('dtlNetPlotID2.png', height = 3000, width = 3000)
plot(dtlNet, 
     vertex.col = c('red', 'blue')[dtlNet %v% 'isD' + 1],
     vertex.cex = .25,
     edge.col = adjustcolor('gray', .25),
     vertex.border = adjustcolor('white', 0))
dev.off()

coopNet %v% 'isC' = coopNet %v% 'vertex.names' == 'graham coop'

png('coopNetPlotID.png', height = 1200, width = 1600)
plot(coopNet, vertex.col = coopNet %v% 'isC')
dev.off()
