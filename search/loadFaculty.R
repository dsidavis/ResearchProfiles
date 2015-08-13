loadFaculty = function(loc = "../ScrapeDavis/")
{
  # rda files:
  depts = list.files(loc, pattern = "\\.rda",  full.names = TRUE)
  depts = depts[!grepl("urls", depts)]
  fac = lapply(depts, function(x) get(load(x)))

  # Add rds files:
  rdsDepts = list.files(loc, pattern = "\\.rds",  full.names = TRUE)
  fac = c(fac, lapply(rdsDepts, readRDS))
  
  # Add names
  nm = strsplit(c(depts, rdsDepts), c("/|\\."))
  structure(fac, "names" = sapply(nm, function(x) x[length(x) - 1]))
}