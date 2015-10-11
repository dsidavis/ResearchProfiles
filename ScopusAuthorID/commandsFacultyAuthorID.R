load("faculty")
library(parallel)

# Get names in correct form for input into getAuthorID.
facultyNames = apply(faculty[, c("LASTNAME", "FIRSTNAME")], c(1,2), as.character)
colnames(facultyNames) = NULL
facultyNames = lapply(1:nrow(facultyNames), function(i, x) 
                      { c(x[i, 1], x[i, 2]) }, facultyNames)

loopGetAuthorID =
  function(i, x)
  {
    getAuthorID(authlast = x[[i]][1], authfirst = x[[i]][2], affil = "Davis")
  }

numFac = length(facultyNames)
cl = makeCluster(30, "FORK")

facultyID_A = parLapply(cl, 1:1000, loopGetAuthorID, facultyNames)
facultyID_B = parLapply(cl, 1001:2000, loopGetAuthorID, facultyNames)
facultyID_C = parLapply(cl, 2001:3000, loopGetAuthorID, facultyNames)
facultyID_D = parLapply(cl, 3001:4000, loopGetAuthorID, facultyNames)
# Don't have E yet. (group 4001 to 5000)
facultyID_E = parLapply(cl, 4001:5000, loopGetAuthorID, facultyNames)
facultyID_F = parLapply(cl, 5001:numFac, loopGetAuthorID, facultyNames)

stopCluster(cl)


# Connect author wit their IDs and find those with multiple IDs
facultyID.A_D = c(facultyID_A, facultyID_B, facultyID_C, facultyID_D)
ind = sapply(1:length(facultyID.A_D), function(i, x){length(x[[i]])>=2}, facultyID.A_D)
facultyA_D = faculty[1:4000]
facultyA_D[, "ID_NUM"] = list(facultyID.A_D)
facultyMultipleID = facultyA_D[ind, ]