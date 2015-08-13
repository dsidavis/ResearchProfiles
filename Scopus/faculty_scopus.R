# Description:
#   Attempt at using the university faculty list to query Scopus.

source("scopus.R")

main = function(...)
{
  faculty = read_faculty(...)

  # Drop all but names.
  faculty = faculty[1:2]

  # TODO: Memory may become a problem since the list is quite long.
  results = mapply(
    function(first, last) {
      # Avoid making too many queries per second.
      Sys.sleep(0.1)

      scoGetAuthor(last = last, first = first)
    }
    , faculty$FIRSTNAME
    , faculty$LASTNAME
    , SIMPLIFY = FALSE
  )

  saveRDS(results, "faculty_scopus.rds")

  results
}

read_faculty = function(file = "faculty_list.csv", ...)
{
  colClasses = c(
    FIRSTNAME = "character"
    , LASTNAME = "character"
    , TITLE = "factor"
    , ADMIN_DEPT = "factor"
    , WORK_DEPT = "factor"
    , EMAIL = "character"
    # Columns below are really Y/N logical.
    # TODO: Convert to logical.
    , ACADEMIC = "factor"
    , ACADEMIC_SENATE = "factor"
    , ACADEMIC_FEDERATION = "factor"
    , TEACHING_FACULTY = "factor"
    , LADDER_RANK = "factor"
  )

  read.csv(file, colClasses = colClasses, ...)
}

main(nrow = 1000)
