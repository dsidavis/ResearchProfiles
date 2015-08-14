getLastName = function(name)
{
  if(grepl(",", name)) { 
    strsplit(name, ",")[[1]][1] 
  } else {
    x = strsplit(name, "\\s")[[1]]
    x[length(x)]
  }
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}