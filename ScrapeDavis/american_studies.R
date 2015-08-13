#!/usr/bin/env Rscript
# Description:
#   Scrape American Studies faculty.

library(RCurl)
library(xml2)
source("funcs.R")

# http://ams.ucdavis.edu/faculty

fetch =
function(base_url)
{
  response = getURLContent(base_url, cookiejar = "", followlocation = TRUE)
  read_html(response)
}

scrape =
function(document, base_url)
{
  # TODO: figure out the xpath for this
  xpath = "//table[@class = 'faculty']//td"
  nodes = xml_find_all(document, xpath)

  faculty = lapply(nodes, scrape_node, base_url)
  not_null = !sapply(faculty, is.null)
  faculty = simplify2array(faculty[not_null])

  data.frame(t(faculty), stringsAsFactors = FALSE)
}

scrape_node =
function(node, base_url)
{
  name = xml_find_all(node, ".//strong")
  if (length(name) == 0)
    return(NULL)

  name = xml_text(name)
  name = strsplit(name, " ")[[1]]
  first = name[[1]]
  last = name[[2]]

  title = strsplit(as.character(node), "<br/>", fixed = TRUE)[[1]][[3]]
  title = sanitize(title)

  email = xml_find_all(node, ".//a[contains(text(), 'email')]")
  email =
    if (length(email) == 0) NA
    else xml_attr(email, "href")
  email = gsub("mailto:", "", email)

  url = xml_find_all(node, ".//a[contains(text(), 'website')]")
  url =
    if (length(url) == 0) NA
    else url_absolute(xml_attr(url, "href"), base_url)

  c(first = first, last = last, title = title, email = email, url = url)
}

base_url = "http://ams.ucdavis.edu/faculty"
doc = fetch(base_url)
saveRDS(scrape(doc, base_url), "american_studies.rds")
