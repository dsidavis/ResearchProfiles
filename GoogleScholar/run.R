#
# This is an attempt to make the scraping look as much like a browser as we can.
# We create a curl handle and set the useragent and other header fields for the request.
# We then make a request to the GS front page. This will get us a cookie and we then pass that
# back in each actual request

u = "http://scholar.google.com"

gh = getCurlHandle(cookiejar = "", useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:39.0) Gecko/20100101 Firefox/39.0",
                   verbose = TRUE, followlocation = TRUE,
                   httpheader = c(Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                               'Accept-Language' = "en-US,en;q=0.5"  # ,'Accept-Encoding' = "gzip, deflate"
                               ))

invisible(getURLContent(u, curl = gh))


curlSetOpt(curl = gh, referer = "https://scholar.google.com/")

dtl = googleScholar("Duncan Temple Lang", curl = gh, max = 30)
