
hp = "http://www.michaellevy.name/research.html"
ll = getHTMLLinks(hp)
ll = getRelativeURL(ll, hp)

doiRX = "s00267"
links = grep(doiRX, ll, value = TRUE)

