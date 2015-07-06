getAwardInfo =
function(filename, doc = xmlParse(filename))
{

  abs = xmlValue(getNodeSet(doc, "//AbstractNarration")[[1]])

  dates = xpathSApply(doc, "//AwardEffectiveDate | //AwardExpirationDate", xmlValue)

  directorate = xpathSApply(doc, "//Directorate", xmlValue)

  people = xpathApply(doc, "//Investigator", getPersonInfo)

  list(abstract = abs, dates = dates, directorate = directorate, people = people)
}


getPersonInfo =
function(node)
{
  list( first = xmlValue(node[["FirstName"]]), last = xmlValue(node[["LastName"]]), role = xmlValue(node[["RoleCode"]]))
}
