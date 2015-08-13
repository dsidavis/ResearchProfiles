getAwardInfo =
function(filename, doc = xmlParse(filename))
{

  abs = xmlValue(getNodeSet(doc, "//AbstractNarration")[[1]])

  dates = xpathSApply(doc, "//AwardEffectiveDate | //AwardExpirationDate", xmlValue)

  directorate = xpathSApply(doc, "//Directorate", xmlValue)
  division = xpathSApply(doc, "//Division", xmlValue)

  amount = xpathSApply(doc, "//AwardAmount", xmlValue)  

  people = xpathApply(doc, "//Investigator", getPersonInfo)
  title = xpathSApply(doc, "//AwardTitle", xmlValue)
  foa = xpathSApply(doc, "//FoaInformation/Name", xmlValue)  

  refs = xpathSApply(doc, "//ProgramReference/Text", xmlValue)
  progType = xpathSApply(doc, "//ProgramElement/Text", xmlValue)
  if(length(progType) == 0)
      progType = NA

  list(abstract = abs, dates = dates, directorate = directorate, division = division, people = people, amount = amount, title = title, areas = refs, programType = progType, foa = foa)
}


getPersonInfo =
function(node)
{
  list( first = xmlValue(node[["FirstName"]]), last = xmlValue(node[["LastName"]]), role = xmlValue(node[["RoleCode"]]))
}
