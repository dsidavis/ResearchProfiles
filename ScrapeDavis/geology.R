source("funcs.R")
doc = htmlParse("http://geology.ucdavis.edu/people/faculty/index.php")

nodes = getNodeSet(doc, "//div[@class = 'border_box' and a[@class = 'image_link ']]//p/strong/a[1]")

geo = getNames(nodes, FALSE)
rownames(geo) = NULL


