library(RCurl)
library(XML)

if(FALSE) {
u = 'http://www.nsf.gov/awardsearch/download.jsp'

txt = getURLContent(u)

doc = htmlParse(txt, asText = TRUE)
a = getNodeSet(doc, "//div[@class = 'download']//p/a/@href")

us = getRelativeURL(a, u)


a = getNodeSet(doc, "//div[@class = 'download']//p/a")
us = sapply(a, xmlGetAttr, 'href')
year = gsub(" -.*", "", sapply(a, xmlValue))
cmds = sprintf("wget -O %s.zip '%s'", year, getRelativeURL(us, u))

status = sapply(cmds, system, intern = TRUE)



library(Rcompression)
sapply(us, downloadYear)
}


downloadYear =
function(u) {
    print(u)
    tmp = getURLContent(u, header = TRUE, binary = TRUE)
    out = getFilename(tmp$header)
    out = gsub("\\.zip", "", out)
    if(!file.exists(out))
        dir.create(out)
    writeZipArchive(tmp$body$body, out)     
                
#                writeBin(tmp$body$body, getFilename(tmp$header)) ## Why two body!
}    

writeZipArchive =
function(data, dir)
{
   ar = zipArchive(data)
   ids = names(ar)
   files = sprintf("%s/%s", dir, ids)
   mapply(function(id, filename) 
             cat(ar[[id]], file = filename),
          ids, files)
  files 
}

getFilename =
function(header)    
{
    o = header["Content-disposition"]
    tmp = strsplit(o, ";")[[1]]
    w = grepl("filename=", tmp)
    if(!any(w))
        NA
    else
        XML:::trim(gsub('filename="?([^"]+)"?$', "\\1", tmp[w]))
}



