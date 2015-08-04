#!/usr/bin/env Rscript
# Description:
#   Scrape Art Studio faculty.

library(RCurl)
library(xml2)

# http://arts.ucdavis.edu/art-studio-faculty

art_studio =
function()
{
  base_url = "http://arts.ucdavis.edu/art-studio-faculty"
  response = getURLContent(base_url, useragent = "Firefox")
  document = read_html(response)

  xpath = "//div[@id = 'content']//div[@class = 'node-inner']"
  nodes = xml_find_all(document, xpath)

  faculty = sapply(nodes,
    function(node) {
      # Header: scrape name, url, title.
      header = xml_find_one(node, "./h2/a")
      #url = url_absolute(xml_attr(header, "href"), base_url)

      name = xml_text(xml_find_one(header, "./span[@class = 'title']"))
      name = strsplit(name, " ")
      first = sapply(name, `[`, 1)
      last = sapply(name, `[`, 2)
      title = xml_text(xml_find_all(header, "./span[@class = 'subtitle']"))
      title = paste0(title, collapse = "; ")

      # Meta / Content
      content = xml_find_all(node, "./div[contains(@class, 'node-meta') or contains(@class, 'node-content')]")
      content = paste0(xml_text(content), collapse = "\n")

      # Contact
      email = xml_find_one(node, ".//div[contains(@class, 'field-email')]/a")
      email = xml_attr(email, "href")
      email = gsub("mailto:", "", email)
      email = gsub("[remove-this-and-replace-with-at]", "@", email, fixed = TRUE)

      # Links
      links = xml_find_all(node, "./div[contains(@class, 'node-links')]//a")
      url = tail(links, 1)
      url = url_absolute(xml_attr(url, "href"), base_url)

      c(first = first, last = last, title = title, url = url, email = email,
        content = content)
    })

  data.frame(t(faculty), stringsAsFactors = FALSE)
}

saveRDS(art_studio(), "art_studio.rds")
