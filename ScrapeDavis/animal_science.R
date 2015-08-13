#!/usr/bin/env Rscript
# Description:
#   Scrape Animal Science faculty.

library(XML)
library(RCurl)
source("funcs.R")

animal_science =
function()
{
  base_url = "http://animalscience.ucdavis.edu/faculty/"
  response = getURLContent(base_url, useragent = "Firefox")
  document = htmlParse(response)

  xpath = "//td[@class = 'content']//p[b or strong or a[b or strong]]"
  nodes = getNodeSet(document, xpath)

  # Extract names and urls.
  names = sapply(nodes, xmlValue)
  urls = sapply(nodes,
    function(node) {
      a = node[["a"]]
      if (is.null(a)) NA
      else xmlGetAttr(a, "href")
    })
  known = !is.na(urls)
  urls[known] = getRelativeURL(urls[known], base_url, sep = "")

  # Break down "last, first - title" into separate variables.
  names = sanitize(names)
  names = strsplit(names, " ?- ?")
  faculty = sapply(names,
    function(x) {
      name = strsplit(x[1], ", ?")[[1]]
      title = paste0(x[-1], collapse = " - ")
      c(last = name[[1]], first = name[[2]], title = title)
    })

  data.frame(t(faculty), urls, stringsAsFactors = FALSE)
}

saveRDS(animal_science(), file = "animal_science.rds")
