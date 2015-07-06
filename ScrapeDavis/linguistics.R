u = 'http://linguistics.ucdavis.edu/People/faculty'
getInfo =
function(node)
{
   who = xpathSApply(node, ".//*[@class = 'personName']", xmlValue)
   url = xpathSApply(node, ".//*[@class = 'moreInfo']/a/@href")
   title =    who = xpathSApply(node, ".//*[@class = 'jobTitle']/li[1]", xmlValue)
   list(name = who, url = url, title = title)   
}


linguistics = as.data.frame(do.call(rbind, xpathApply(htmlParse(u), "//div[./*[@class = 'personName']]", getInfo)))
linguistics$Dept = rep("Linguistics", nrow(linguistics))

save(linguistics, file = "linguistics.rda")


u = 'http://linguistics.ucdavis.edu/People/linguistics-faculty-staff-students/linguistics-faculty-staff-students/linguistics-faculty-staff-students/graduate-group-faculty'
linguisticsGradGroup = as.data.frame(do.call(rbind, xpathApply(htmlParse(u), "//div[./*[@class = 'personName']]", getInfo)))
