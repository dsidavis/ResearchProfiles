#!/usr/bin/env Rscript
# Description:
#   Attempt at using the university faculty list to query Scopus.

source("scopus.R")
source("utilities.R")

SCOPUS_SUBJECTS = c(
  # List of Scopus subject abbreviations.
  "AGRI", "ARTS", "BIOC", "BUSI", "CENG", "CHEM", "COMP", "DECI", "DENT"
  , "EART", "ECON", "ENER", "ENGI", "ENVI", "HEAL", "IMMU", "MATH", "MEDI"
  , "NEUR", "NURS", "PHAR", "PHYS", "PSYC", "SOCI", "VETE", "MULT"
)

scopus_to_data_frame = function(scopus_data, queries)
  #' Convert data from Scopus Author Search to a data frame.
  #'
  #' @param scopus_data list of results from Scopus Author Search
  #' @param queries name queried for each element of scopus_data
{
  if (missing(queries))
    queries = names(scopus_data)

  # Set up a skeleton of the final data frame.
  hits = sapply(scopus_data, length)
  faculty = data.frame(
    query_id = seq_along(queries)
    , query = queries
    , hits = hits
    , stringsAsFactors = FALSE
  )
  hits[hits == 0] = 1
  faculty = faculty[rep.int(seq_along(hits), hits), ]

  # Extract the rest of the columns from the Scopus data.
  scopus_matrix = lapply(scopus_data,
    function(entry_set)
      # Convert a set of entries into a matrix.
    {
      if (is.null(entry_set))
        return(NA)

      entry_matrix = sapply(entry_set,
        function(entry)
          # Convert a single entry into a matrix row.
        {
          affiliations = entry[["affiliation-current"]]

          # TODO: Should we use name-variant somehow?
          name = entry[["preferred-name"]]
          first = name[["given-name"]]
          last = name[["surname"]]
          name = paste(first, last)

          subjects = sapply(entry[["subject-area"]], `[[`, "@abbrev")
          subjects = SCOPUS_SUBJECTS %in% subjects
          names(subjects) = paste0("subject_", SCOPUS_SUBJECTS)

          c(
            prism_url = entry[["prism:url"]]
            , author_id = entry[["dc:identifier"]]
            , electronic_id = entry[["eid"]]
            , document_count = entry[["document-count"]]

            , affiliation = affiliations[["affiliation-name"]]
            , affiliation_id = affiliations[["affiliation-id"]]

            , last = last
            , first = first
            , name = name

            , subjects
          )
        })
      entry_matrix = t(entry_matrix)

      #lev_dist = stringdist(entry_matrix[1, "name"], entry_matrix[, "name"])
      #cbind(entry_matrix, dist_first = lev_dist)
    }
  )
  scopus_matrix = do.call(rbind, scopus_matrix)
  rownames(scopus_matrix) = NULL
  faculty = data.frame(faculty, scopus_matrix, stringsAsFactors = FALSE)

  # Correct the column classes.
  cols = c("query_id", "hits", "document_count")
  faculty[cols] = lapply(faculty[cols], as.integer)

  cols = c("query", "last", "first", "name")
  faculty[cols] = lapply(faculty[cols], sanitize_whitespace)

  cols = c("affiliation", "affiliation_id")
  faculty[cols] = lapply(faculty[cols], as.factor)

  cols = paste0("subject_", SCOPUS_SUBJECTS)
  faculty[cols] = lapply(faculty[cols], as.logical)

  return(faculty)
}

read_faculty = function(file = "faculty_list.csv", ...)
  # Read the faculty CSV file.
  #
  # This is just a wrapper for read.csv() with the column classes specified.
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


query_scopus_for_faculty = function(first_names, last_names, file_names
  , data_dir = "author_queries", max_per_sec = 100)
  # Query scopus using the faculty CSV file.
{
  # Older versions of R don't support dir.exists().
  #if (!dir.exists(data_dir)) {
  if (!file.exists(data_dir)) {
    dir_was_created = dir.create(data_dir)
    if (!dir_was_created)
      stop(sprintf("Unable to create directory '%s'.", data_dir))
  }

  file_names = file.path(data_dir, paste0(file_names, ".rds"))

  mapply(
    function(first, last, file)
      # Run a Scopus query for any faculty we haven't already run through.
    {
      if (!file.exists(file))
        try({
          entries = scoGetAuthor(last = last, first = first)
          names(entries) = paste(first, last)
          saveRDS(entries, file)
        })

      Sys.sleep(1 / max_per_sec)
    }
  , first_names, last_names, file_names)

  invisible()
}

main = function()
{
  faculty = read_faculty()

  file_names = extract_email_username(faculty$EMAIL)

  query_scopus_for_faculty(faculty$FIRSTNAME, faculty$LASTNAME, file_names)
}

main()
