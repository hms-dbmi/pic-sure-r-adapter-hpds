#' @import jsonlite stringr httr urltools
NULL

library(jsonlite)
library(stringr)
library(httr)
library(urltools)

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
  session <- initializeSession(url, token, psama_url, getDictionary = bdc.searchPicsure)
  session <- bdc.setResource(session, "AUTH")
  return (session)
}

#' Set the resource to use for queries.
#'
#' @param session Current session
#' @param resourceName Name of resource to select. Valid values are OPEN or AUTH
#' @return A PIC-SURE session
#' @examples
#'
#'# session <- picsure::bdc.setResource(session, AUTH)
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

#' @export
bdc.search <- function(session, keyword, limit = 0, offset = 0, includeValues = FALSE) {
  query = list(
    searchTerm = keyword,
    includedTags = list(),
    excludedTags = list(),
    returnTags = FALSE,
    offset = offset,
    limit = if(limit == 0) 10000 else limit #revert, testing only
    #limit = if(limit == 0) 1000000 else limit
  )
  if(!includeValues)
    query = c(query, variableValuesLimit = 0)
  queryJSON <- jsonlite::toJSON(list(query = query), auto_unbox=TRUE)

  result <- postJSON(session, paste("search/", session$resources$dictionary, sep = ""), queryJSON)
  return (result)
}


#' @export
bdc.getInfoColumns <- function(query) {
  queryJSON = generateQueryJSON(query, expectedResultType = 'INFO_COLUMN_LISTING')

  result = postJSON(query$session, "query/sync/", queryJSON)
  return (result)
}

projectAndFilterResults = function(results, scopes, showAll) {
  # todo: do this when creating the session?
  scopes = if (is.null(scopes)) c() else str_replace_all(scopes[str_detect(scopes, "^\\\\")], "\\\\", "")
  in_scope = function(study) Reduce(function(acc, scope) (acc | str_detect(study, fixed(scope))), scopes, init=FALSE)

  include_list <- c()
  paths <- c()

  if (length(results) < 1) return(list(results = list(), paths = paths))

  for (index in 1:length(results)) {
    resultMetadata <- results[[index]]$result$metadata
    categorical = results[[index]]$result$is_categorical
    if (!(showAll | in_scope(resultMetadata$columnmeta_HPDS_PATH))) next

    paths <- c(paths, resultMetadata$columnmeta_HPDS_PATH)
    results[[index]] <- list(
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
      HPDS_PATH = resultMetadata$columnmeta_HPDS_PATH,
      min = if (categorical) NA else resultMetadata$columnmeta_min,
      max = if (categorical) NA else resultMetadata$columnmeta_max,
      categorical = categorical,
      values = toString(results[[index]]$result$values)
    )
    include_list <- c(include_list, index)
  }
  return(list(results=results[include_list], paths=paths))
}

bdc.lookupVariables <- function(query, keys) {
  return (query$session$dictionary[query$session$dictionary$HPDS_PATH %in% keys, ])
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
#'# query <- picsure::bdc.addClause(query, "\\phs000284\\pht001902\\phv00122593\\ZM\\", "FILTER", min = 1)
#'
#' @export
bdc.addClause <- function(query, keys, type = "FILTER", min = NULL, max = NULL, categories = NULL) {
  return (addClause(query, keys, type, min, max, categories, bdc.lookupVariables))
}
