source("gsProfile.R")
f = read.csv("../export-academic-20150804.csv")
# To bound the problem, keep to (presumably) active faculty:
activeFac = grepl('prof', f$TITLE, ignore.case = TRUE) & !grepl('emerit', f$TITLE, ignore.case = TRUE)
#sort(table(f$TITLE[activeFac]))
fnames = paste(f$FIRSTNAME, f$LASTNAME)[activeFac]

# Getting about 100 before Google gets upset (w/o the reset)
#i = 0
for(n in fnames[i:length(fnames)])
{
  g = structure(getProfileArticles(n), 'name' = n)
  if(length(g) == 0)
    break
  saveRDS(g, file = paste0("profileArticles/", gsub("\\s", "", n), ".rds"))
  i = i + 1
  cat(i, 'of', length(fnames), 'done\n')
  Sys.sleep(runif(1, 1, 5))
#  if(sample(20, 1)==20) {
#    reset(gh)
#    invisible(getURLContent(GoogleScholarBaseURL, curl = gh))
#    curlSetOpt(curl = gh, referer = GoogleScholarBaseURL)
#    Sys.sleep(runif(1, 5, 10))
#  }
}

# test if google is blocking
# getProfileArticles('mark lubell')


profs = lapply(list.files('profileArticles/', full.names = TRUE), readRDS)
names(profs) = list.files('profileArticles/')  #fnames[1:length(profs)]
haveProfs = profs[!is.na(profs)]
length(haveProfs) / length(profs)  # 13% of first 182
plot(density(sapply(haveProfs, nrow)), main = 'number articles per profile')  
names(haveProfs)
names(profs[is.na(profs)])  # Googling some of these names, they look like
# the same population as the faculty for which we get profiles.
# 13% is pretty low, but those we do get are mostly well formatted.
