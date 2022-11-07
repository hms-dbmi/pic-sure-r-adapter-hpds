bdc.connect <- function(url, token, psama_url=FALSE) {
  connection <- connect(url, token, psama_url)
  # todo: validate resources
  return (connection)
}

bdc.setResource <- function(connection, resourceName) {
    if (toupper(resourceName) == "OPEN" || toupper(resourceName == "AUTH")) {
      connection <- c(connection, currentResource = "resourceName")
    } else {
      print("Invalid resourceName. Please specify 'OPEN' or 'AUTH'")
    }
    return (connection)
}


# todo: remove limit and offset?
bdc.searchPicsure <- function(connection, keyword = "", resultType = "DATA_FRAME", limit = 0, offset = 0, includeValues = FALSE) {
  searchResults = bdc.search(connection, keyword, limit, offset, includeValues = includeValues)
  filteredResults = projectAndFilterResults(searchResults$results$searchResults, connection$profile.queryScopes, FALSE)

  if(toupper(resultType) == "DICTIONARY")
    return (filteredResults$results)
  if (toupper(resultType) == "VARIABLE_PATHS")
    return (unique(filteredResults$paths))
  if (toupper(resultType) == "DATA_FRAME")
    return (data.frame(do.call(rbind.data.frame, filteredResults$results)))
}

bdc.search = function(connection, keyword, limit = 0, offset = 0, includeValues = FALSE) {
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

  result <- postJSON(connection, paste("search/", connection$resources.dictionary, sep = ""), queryJSON)
  return (result)
}

in_scope = function(study, scopes) {
  return (Reduce(function(acc, scope) (acc | str_detect(study, fixed(scope))), scopes, init=FALSE))
}

projectAndFilterResults = function(results, scopes, showAll) {
  # todo: do this when creating the session?
  scopes = if (is.null(scopes)) c() else str_replace_all(scopes[str_detect(scopes, "^\\\\")], "\\\\", "")

  include_list <- c()
  paths <- c()

  if (length(results) < 1) return(list(results = list(), paths = paths))

  for (index in 1:length(results)) {
    resultMetadata <- results[[index]]$result$metadata
    categorical = results[[index]]$result$is_categorical
    if (!(showAll | in_scope(resultMetadata$columnmeta_HPDS_PATH, scopes))) next

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
      values = toString(results[[index]]$result$values)
    )
    include_list <- c(include_list, index)
  }
  return(list(results=results[include_list], paths=paths))
}
