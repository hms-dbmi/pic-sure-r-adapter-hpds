NULL

#' Creates a query object to query a PIC-SURE instance
#'
#' @param session Current PIC-SURE session
#' @return A query object
#' @examples
#'
#'# query <- picsure::newQuery(session)
#'
#' @export
newQuery <- function(session) {
  query = list(session = session)

  query$resourceId = determineResource(session)

  query$fields = list()
  query$requiredFields <- list()
  query$numericFilters <- list()
  query$categoryFilters <- list()
  query$variantInfoFilters <- list()
  query$anyRecordOf <- list()
  query$requiredFields <- list()
  query$crossCountFields <- list()

  query = parseQueryTemplate(query)

  # TODO: load the default queryTemplate values. BDC only?
  # self$load(self$session$profile_info$queryTemplate)
  return (query)
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
#'# query <- picsure::newQuery(session)
#'# query <- picsure::addClause(query, "\\demographics\\AGE\\", max = 10)
#'
#' @export
addClause <- function(query, keys, type = "FILTER", min = NULL, max = NULL, categories = NULL, lookupVariablesOverride = NULL) {
  if (typeof(keys) != "list") {
    keys <- list(keys)
  }

  if (is.function(lookupVariablesOverride)) {
    variablesToAdd <- lookupVariablesOverride(query, keys)
  } else {
    variablesToAdd <- lookupVariables(query, keys)
  }


  if(toupper(type) == "FILTER") {
    if (length(keys) != 1) {
      message("Filters must be added one at a time")
      return (query)
    }
    if (nrow(variablesToAdd) == 0) {
      message("Variables not found:")
      message(keys)
      return (query)
    }
    variableToAdd <- variablesToAdd[1,]
    if (variableToAdd$categorical) {
      if(typeof(categories) != "list") {
        message("Categorical variable filters must contain list of values to filter on")
      }
      query$categoryFilters[[keys[[1]]]] <- categories
    } else {
      filterValue <- list()
      if (is.numeric(min)) {
        filterValue$min <- min
      }
      if (is.numeric(max)) {
        filterValue$max <- max
      }
      if(length(filterValue) == 0) {
        message("Continuous variable filters must contain a numeric min or max")
        return (query)
      }
      query$numericFilters[[keys[[1]]]] <- filterValue
    }
    return (query)
  }


  if (nrow(variablesToAdd) != length(keys)) {
    message("Not all variables were valid. Only the following will be added:")
    print(variablesToAdd[,1])
  }
  if(toupper(type) == "SELECT") {
    query$fields <- append(query$fields, variablesToAdd[,1])
    return (query)
  }
  if(toupper(type) == "REQUIRE") {
    query$requiredFields <- append(query$requiredFields, variablesToAdd[,1])
    return (query)
  }
  if(toupper(type) == "ANYOF") {
    query$anyRecordOf <- append(query$anyRecordOf, variablesToAdd[,1])
    return (query)
  }
}

lookupVariables = function(query, keys) {
  return (query$session$dictionary[session$dictionary$name %in% keys, ])
}

getCategoryFilters = function(query) {
  keys = c(
    query$fields,
    query$crossCountFields,
    query$requiredFields,
    query$anyRecordOf,
    names(query$numericFilters),
    names(query$categoryFilters)
  )

  categoryFilters = query$categoryFilters
  if(length(query$variantInfoFilters$categoryVariantInfoFilters) == 0 && length(query$variantInfoFilters$numericVariantInfoFilters) == 0) {
    categoryFilters["\\_topmed_consents\\"] = NULL
  }
  if (length(keys[str_detect(keys, "\\DCC Harmonized data set")]) > 0) {
    categoryFilters["\\_harmonized_consent\\"] = NULL
  }
  return (categoryFilters)
}

generateQueryJSON = function(query, expectedResultType) {
  requestQuery = list(
    fields = query$fields,
    numericFilters = query$numericFilters,
    categoryFilters = getCategoryFilters(query),
    variantInfoFilters = query$variantInfoFilters,
    anyRecordOf = query$anyRecordOf,
    requiredFields = query$requiredFields
    #crossCountFields = query$crossCountFields
  )

  #requestPayload = list(query = requestQuery, resourceUUID = query$resourceId)
  requestPayload = list(query = requestQuery, resourceUUID = determineResource(query$session))
  requestPayload$query[['expectedResultType']] = expectedResultType

  queryJSON = jsonlite::toJSON(requestPayload, auto_unbox = TRUE)
  queryJSON <- gsub('\\[\\[\\]\\]','\\[\\]', queryJSON)
  queryJSON <- gsub('"fields":\\{\\}','"fields":\\[\\]', queryJSON)
  queryJSON <- gsub('"requiredFields":\\{\\}','"requiredFields":\\[\\]', queryJSON)
  queryJSON <- gsub('"numericFilters":\\[\\]','"numericFilters":\\{\\}', queryJSON)
  queryJSON <- gsub('"categoryFilters":\\[\\]','"categoryFilters":\\{\\}', queryJSON)
  queryJSON <- gsub('"categoryVariantInfoFilters":\\[\\]','"categoryVariantInfoFilters":\\{\\}', queryJSON)
  queryJSON <- gsub('"numericVariantInfoFilters":\\[\\]','"numericVariantInfoFilters":\\{\\}', queryJSON)

  return (queryJSON)
}

#' Executes a query
#'
#' @param query A query object
#' @param resultType The result type to query for:
#' "COUNT" queries only for the number of results
#' "DATA_FRAME" queries for the entire result set
#' @return The result of the query
#' @examples
#'
#'# count <- picsure::runQuery(query, "COUNT")
#'# results <- picsure::runQuery(query, "DATA_FRAME")
#'
#' @export
runQuery <- function(query, resultType) {
  if(toupper(resultType) == "COUNT") {
    return (getCount(query))
  }
  if(toupper(resultType) == "DATA_FRAME") {
    return (getResults(query))
  }
  print("Unrecognized resultType parameter. Valid values are: ('COUNT', 'DATA_FRAME'")
}

getCount = function(query) {
  queryJSON = generateQueryJSON(query, expectedResultType = 'COUNT')

  print(queryJSON)
  httpResults = postJSONRaw(query$session, "query/sync/", queryJSON)
  return (httpResults)
}

getResults = function(query) {
  queryJSON = generateQueryJSON(query, expectedResultType = 'DATAFRAME')

  print(queryJSON)
  response = postJSONRaw(query$session, "query/sync/", queryJSON)

  return(read.csv(text=response, sep=','))
}

determineResource = function(session) {
  return (session$currentResource)
}

parseQueryTemplate = function(query) {
  queryTemplateString = query$session$queryTemplate[[1]]
  if (is.null(queryTemplateString) && queryTemplateString != "null") {
    queryTemplate = jsonlite::fromJSON(queryTemplateString)
    query$categoryFilters = queryTemplate$categoryFilters
    query$fields = queryTemplate$fields
    query$variantInfoFilters = queryTemplate$variantInfoFilters
  }
  return (query)
}