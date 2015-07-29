loadFaculty = function(loc = "../ScrapeDavis/")
{
  depts = list.files(loc, pattern = "\\.rda",  full.names = TRUE)
  depts = depts[!grepl("urls", depts)]
  fac = lapply(depts, function(x) get(load(x)))
  nm = strsplit(depts, c("/|\\."))
  structure(fac, "names" = sapply(nm, function(x) x[length(x) - 1]))
}