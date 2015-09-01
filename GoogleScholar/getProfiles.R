source("gsProfile.R")
f = read.csv("../export-academic-20150804.csv")
# To bound the problem, keep to (presumably) active faculty:
activeFac = grepl('prof', f$TITLE, ignore.case = TRUE) & !grepl('emerit', f$TITLE, ignore.case = TRUE)
#sort(table(f$TITLE[activeFac]))
fnames = paste(f$FIRSTNAME, f$LASTNAME)[activeFac]

i = 0
for(n in fnames)
{
  g = getProfileArticles(n)
  if(length(g) == 0)
    break
  saveRDS(g, file = paste0("profileArticles/", gsub("\\s", "", n), ".rds"))
  i = i + 1
  cat(i, 'of', length(fnames), 'done\n')
  Sys.sleep(runif(1, 1, 5))
}

profs = lapply(list.files('profileArticles/', full.names = TRUE), readRDS)
names(profs) = list.files('profileArticles/')
length[!is.na(profs)]