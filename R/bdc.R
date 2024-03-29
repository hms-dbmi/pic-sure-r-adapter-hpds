#' @import jsonlite stringr httr urltools tibble
NULL

library(jsonlite)
library(stringr)
library(httr)
library(urltools)
library(tibble)

#' Creates a session to access a PIC-SURE instance
#'
#' @param url Url to a PIC-SURE instance
#' @param token A user token to access this instance. This can be found in the "User Profile" tab
#' on the PIC-SURE site
#' @param psama_url (Optional) URL to override the default PSAMA endpoint
#' @return An object which provides access to a PIC-SURE instance
#' @examples
#'
#'# session <- picsure::bdc.initializeSession(url="http://your.server/PIC-SURE/", token="your-security-token")
#'
#' @export
bdc.initializeSession <- function(url, token, psama_url=FALSE) {
  session <- initializeSession(url, token, psama_url, initializeDictionary = bdc.initializeDictionary, defaultResource = "auth-hpds")
  return (session)
}

#' Set the resource to use for queries.
#'
#' @param session Current session
#' @param resourceName Name of resource to select. Valid values are OPEN or AUTH
#' @return A PIC-SURE session
#' @examples
#'
#'# session <- picsure::bdc.setResource(session, 'AUTH')
#'
#' @export
bdc.setResource <- function(session, resourceName) {
  if (toupper(resourceName) == "OPEN") {
    session$currentResource = session$resources$`open-hpds`
    return (session)
  }
  if (toupper(resourceName) == "AUTH") {
    session$currentResource = session$resources$`auth-hpds`
    return (session)
  }

  print("Invalid resourceName. Please specify 'OPEN' or 'AUTH'")
  return (session)
}


#' Performs a search of variables in PIC-SURE for a given keyword
#'
#' @param session Current session
#' @param keyword Keyword to search for
#' @param resultType Optional parameter to specify result type: Valid values:
#' DATA_FRAME (default) which converts the response to a data frame
#' DICTIONARY which returns a list
#' VARIABLE_PATHS which returns the unique variable paths
#' @param includeValues Boolean to enable including all values with categorical
#' variables. Default is FALSE. Use with caution, this can be a large amount of data
#' @param limit Optonal, default 0, use to limit results. Returns the first N results
#' @param offset Optional, default 0, use to paginate results. Returns results starting at N
#' @return A PIC-SURE session
#' @examples
#'
#' searchResults <- picsure::bdc.searchPicsure(session, "heart", resultType = "DATA_FRAME")
#'
#' @export
bdc.searchPicsure <- function(session, keyword = "", resultType = "DATA_FRAME", includeValues = FALSE, limit = 0, offset = 0) {
  searchResults = bdc.search(session, keyword, limit, offset, includeValues = includeValues)
  filteredResults = projectAndFilterResults(searchResults$results$searchResults, session$profile$queryScopes, FALSE)

  if(toupper(resultType) == "DICTIONARY")
    return (filteredResults$results)
  if (toupper(resultType) == "VARIABLE_PATHS")
    return (unique(filteredResults$paths))
  if (toupper(resultType) == "DATA_FRAME")
    return (data.frame(do.call(rbind.data.frame, filteredResults$results)))
}

bdc.search <- function(session, keyword, limit = 0, offset = 0, includeValues = FALSE) {
  query = list(
    searchTerm = keyword,
    includedTags = list(),
    excludedTags = list(),
    returnTags = FALSE,
    offset = offset,
    limit = if(limit == 0) 1000000 else limit
  )
  if(!includeValues)
    query = c(query, variableValuesLimit = 0)
  queryJSON <- jsonlite::toJSON(list(query = query), auto_unbox=TRUE)

  result <- postJSON(session, paste("search/", session$resources$dictionary, sep = ""), queryJSON)
  return (result)
}

bdc.initializeDictionary <- function(session) {
  dictionary <- bdc.searchPicsure(session)
  # Including these special cases, as they are valid variables that a user can use in queries
  dictionary <- dictionary %>% tibble::add_row(name = "\\_consents\\", categorical = TRUE)
  dictionary <- dictionary %>% tibble::add_row(name = "\\_harmonized_consent\\", categorical = TRUE)
  dictionary <- dictionary %>% tibble::add_row(name = "\\_topmed_consents\\", categorical = TRUE)

  # BDC has a separate dictionary resource, which does not include genomic annotations
  # in it's response like HPDS normally does in other environments
  message("Loading genomic annotations...")
  genomicAnnotations <- tryCatch(
    { initializeGenomicAnnotations(session) },
    error = function(error){
      print(error$message)
      message("Unable to load genomic annotations, variant queries will not function properly.")
      return (data.frame())
    }
  )
  return (list(
    phenotypes = dictionary,
    info = genomicAnnotations
  ))
}

initializeGenomicAnnotations <- function(session) {
  result <- postJSON(session, paste("search/", session$currentResource, sep = ""), "{\"query\":\"\"}")
  result <- result$results$info
  annotations = list()
  for (conceptName in names(result)) {
    concept = result[[conceptName]]
    conceptValues = if (!is.null(concept$values)) {
      concept$values
    } else {
      concept$categoryValues
    }
    annotations[[(length(annotations) + 1)]] = list(
      name = conceptName,
      description = concept$description,
      values = toString(conceptValues),
      continuous = concept$continuous
    )
  }
  return(data.frame(do.call(rbind.data.frame, annotations)))
}

# Maps the search results to a more user friendly format, which is valid to be turned into a data frame
projectAndFilterResults = function(results, scopes, showAll) {
  scopes = if (is.null(scopes)) c() else stringr::str_replace_all(scopes[str_detect(scopes, "^\\\\")], "\\\\", "")
  in_scope = function(study) Reduce(function(acc, scope) (acc | stringr::str_detect(study, fixed(scope))), scopes, init=FALSE)

  include_list <- c()
  paths <- c()

  if (length(results) < 1) return(list(results = list(), paths = paths))

  for (index in 1:length(results)) {
    resultMetadata <- results[[index]]$result$metadata
    categorical = results[[index]]$result$is_categorical
    if (!(showAll | in_scope(resultMetadata$columnmeta_HPDS_PATH))) next

    paths <- c(paths, resultMetadata$columnmeta_HPDS_PATH)
    results[[index]] <- list(
      name = resultMetadata$columnmeta_HPDS_PATH,
      var_id = resultMetadata$derived_var_id,
      var_name = resultMetadata$derived_var_name,
      var_description = resultMetadata$derived_var_description,
      data_type = resultMetadata$columnmeta_data_type,
      group_id = resultMetadata$derived_group_id,
      group_name = resultMetadata$derived_group_name,
      group_description = resultMetadata$derived_group_description,
      study_id = resultMetadata$derived_study_id,
      study_description = resultMetadata$derived_study_description,
      is_stigmatized = resultMetadata$is_stigmatized,
      min = if (categorical) NA else as.numeric(resultMetadata$columnmeta_min),
      max = if (categorical) NA else as.numeric(resultMetadata$columnmeta_max),
      categorical = categorical,
      values = toString(results[[index]]$result$values)
    )
    include_list <- c(include_list, index)
  }
  return(list(results=results[include_list], paths=paths))
}

#' Creates a query object to query a PIC-SURE instance
#'
#' @param session Current PIC-SURE session
#' @return A query object
#' @examples
#'
#'# query <- picsure::bdc.newQuery(session)
#'
#' @export
bdc.newQuery <- function(session) {
  return (newQuery(session))
}

#' Re-creates a query object from a queryUUID
#'
#' @param session Current PIC-SURE session
#' @param queryUUID The query UUID as copied from PIC-SURE
#' @examples
#'
#'# query <- picsure::bdc.getQueryByUUID(session, query = "your query uuid")
#'
#' @export
bdc.getQueryByUUID <- function(session, queryUUID) {
  return (getQueryByUUID(session = session, queryUUID = queryUUID))
}

#' Add a clause to a query
#'
#' @param query A query object
#' @param keys A key or list of keys to add a clause for
#' @param type One of: (“SELECT”, “REQUIRE”, “ANYOF”, “FILTER”)
#' @param min Optional parameter for continuous "FILTER" clauses. Specifies the minimum value
#' @param max Optional parameter for continuous "FILTER" clauses. Specifies the maximum value
#' @param categories Optional parameter for categorical "FILTER" clauses to match
#' @return A query object
#' @examples
#'
#'# query <- picsure::bdc.newQuery(session)
#'# query <- picsure::bdc.addClause(query, "\\phs000001\\pht000001\\phv00000001\\variable1\\", "FILTER", min = 1, max = 2)
#'# query <- picsure::bdc.addClause(query, "\\phs000001\\pht000001\\phv00000001\\variable2\\", "FILTER", categories = list("Yes", "Maybe"))
#'# count <- picsure::getCount(query)
#'# results <- picsure::getResults(query)
#'
#'# query <- picsure::bdc.newQuery(session)
#'# query <- picsure::bdc.addClause(query, "\\phs000001\\pht000001\\phv00000001\\variable3\\", "SELECT")
#'# results <- picsure::getResults(query)
#'
#'# query <- picsure::bdc.newQuery(session)
#'# query <- picsure::bdc.addClause(query, "\\phs000001\\pht000001\\phv00000001\\variable4\\", "REQUIRE")
#'# results <- picsure::getResults(query)
#'
#'# query <- picsure::bdc.newQuery(session)
#'# query <- picsure::bdc.addClause(query, "\\phs000001\\pht000001\\phv00000001\\variable5\\", "ANYOF")
#'# results <- picsure::getResults(query)
#'
#' @export
bdc.addClause <- function(query, keys, type = "FILTER", min = NULL, max = NULL, categories = NULL) {
  return (addClause(query, keys, type, min, max, categories))
}
