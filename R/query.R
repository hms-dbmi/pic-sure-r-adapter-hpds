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

  query$resourceId = session$currentResource

  query$fields = list()
  query$requiredFields <- list()
  query$categoryFilters <- list()
  query$anyRecordOf <- list()
  query$numericFilters <- list()
  query$variantInfoFilters <- list()
  query$variantInfoFilters$numericVariantInfoFilters = list()
  query$variantInfoFilters$categoryVariantInfoFilters = list()

  query = parseQueryTemplate(query)

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
#'#'# query <- picsure::newQuery(session)
#'# query <- picsure::addClause(query, "\\demographics\\AGE\\", "FILTER", min =18, max = 35)
#'# query <- picsure::addClause(query, "\\demographics\\AGE\\", "FILTER", categories = list("Yes", "Maybe"))
#'# count <- picsure::getCount(query)
#'# results <- picsure::getResults(query)
#'
#'# query <- picsure::newQuery(session)
#'# query <- picsure::addClause(query, "\\demographics\\AGE\\", "SELECT")
#'# results <- picsure::getResults(query)
#'
#'# query <- picsure::newQuery(session)
#'# query <- picsure::addClause(query, "\\demographics\\AGE\\", "REQUIRE")
#'# results <- picsure::getResults(query)
#'
#'# query <- picsure::newQuery(session)
#'# query <- picsure::addClause(query, "\\demographics\\AGE\\", "ANYOF")
#'# results <- picsure::getResults(query)
#' @export
addClause <- function(query, keys, type = "FILTER", min = NULL, max = NULL, categories = NULL) {
  if (typeof(keys) != "list") {
    keys <- as.list(keys)
  }

  variablesToAdd <- lookupVariables(query, keys)

  if(toupper(type) == "FILTER") {
    if (length(keys) != 1) {
      message("Filters must be added one at a time")
      return (query)
    }
    if (nrow(variablesToAdd) == 0) {
      variablesToAdd <- lookupGenomicVariables(query, keys)
      if (nrow(variablesToAdd) == 0) {
        message("Variables not found:")
        message(keys)
        return (query)
      }
      # else, add genomic filter
      variableToAdd <- variablesToAdd[1,]
      if (variableToAdd$continuous) {
        filterValue <- list()
        if (is.numeric(min)) {
          if (min < variableToAdd$min || min > variableToAdd$max) {
            message(stringr::str_interp("Min value for ${variableToAdd$name} must be between ${variableToAdd$min} and ${variableToAdd$max}"))
            return (query)
          }
          filterValue$min <- min
        }
        if (is.numeric(max)) {
          if (max < variableToAdd$min || max > variableToAdd$max) {
            message(stringr::str_interp("Max value for ${variableToAdd$name} must be between ${variableToAdd$min} and ${variableToAdd$max}"))
            return (query)
          }
          filterValue$max <- max
        }
        if(length(filterValue) == 0) {
          message("Continuous variable filters must contain a numeric min or max")
          return (query)
        }
        query$variantInfoFilters$numericVariantInfoFilters[[keys[[1]]]] <- filterValue
      } else {
        if(typeof(categories) != "list") {
          message("Categorical variable filters must contain list of values to filter on")
          return (query)
        }
        query$variantInfoFilters$categoryVariantInfoFilters[[keys[[1]]]] <- categories
      }
      return (query)
    }


    variableToAdd <- variablesToAdd[1,]
    if (variableToAdd$categorical) {
      if(typeof(categories) != "list") {
        message("Categorical variable filters must contain list of values to filter on")
        return (query)
      }
      query$categoryFilters[[keys[[1]]]] <- categories
    } else {
      filterValue <- list()
      if (is.numeric(min)) {
        if (min < variableToAdd$min || min > variableToAdd$max) {
          message(stringr::str_interp("Min value for ${variableToAdd$name} must be between ${variableToAdd$min} and ${variableToAdd$max}"))
          return (query)
        }
        filterValue$min <- min
      }
      if (is.numeric(max)) {
        if (max < variableToAdd$min || max > variableToAdd$max) {
          message(stringr::str_interp("Max value for ${variableToAdd$name} must be between ${variableToAdd$min} and ${variableToAdd$max}"))
          return (query)
        }
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


  if (is.null(variablesToAdd) || nrow(variablesToAdd) == 0 ) {
    message("Variables not found:")
    message(keys)
    return (query)
  }
  if (nrow(variablesToAdd) != length(keys)) {
    message("Not all variables were valid. Only the following will be added:")
    message(variablesToAdd[,1])
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

#' Deletes all clauses from this query for a given key
#'
#' @return The updated query
#' @export
deleteClause <- function(query, key) {
  query$fields <- query$fields[!query$fields == key]
  query$requiredFields  <- query$requiredFields[!query$requiredFields == key]
  query$numericFilters[[key]] <- NULL
  query$categoryFilters[[key]] <- NULL
  query$variantInfoFilters$categoryVariantInfoFilters[[key]] <- NULL
  query$variantInfoFilters$numericVariantInfoFilters[[key]] <- NULL
  query$anyRecordOf <- query$anyRecordOf[!query$anyRecordOf == key]
  return (query)
}

#' Gets a variable from the session dictionary, if it exists
lookupVariables = function(query, keys) {
  return (query$session$dictionary[query$session$dictionary$name %in% keys, ])
}


#' Gets a genomic variable from the session dictionary, if it exists
lookupGenomicVariables <- function(query, keys) {
  return (query$session$genomicAnnotations[query$session$genomicAnnotations$name %in% keys, ])
}

#' Prints the JSON representation of a query object
#'
#' @param query A query object
#' @examples
#'
#'# query <- picsure::newQuery(session)
#'# query <- picsure::addClause(query, "\\demographics\\AGE\\", max = 10)
#'# picsure::showQuery(query)
#'
#' @export
showQuery <- function(query) {
  print(jsonlite::prettify(generateQueryJSON(query, ""), indent = 4))
}

#' Gets the category filters for a query
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
  # Topmed consents do not need to be included if there are no variant filters
  if(length(query$variantInfoFilters$categoryVariantInfoFilters) == 0 && length(query$variantInfoFilters$numericVariantInfoFilters) == 0) {
    categoryFilters["\\_topmed_consents\\"] = NULL
  }
  # Harmonized consents do not need to be included if there are no harmonized variables in the query
  if (length(keys[stringr::str_detect(keys, "\\DCC Harmonized data set")]) == 0) {
    categoryFilters["\\_harmonized_consent\\"] = NULL
  }
  return (categoryFilters)
}

#' Generate JSON representation of the query to be sent to the PIC-SURE api. This handles various quirks
#' between the required JSON schema and how jsonlite serializes R data types
generateQueryJSON = function(query, expectedResultType) {
  requestQuery = list(
    fields = query$fields,
    numericFilters = query$numericFilters,
    categoryFilters = getCategoryFilters(query),
    anyRecordOf = query$anyRecordOf,
    requiredFields = query$requiredFields
  )

  if (length(query$variantInfoFilters$numericVariantInfoFilters) > 0 || length(query$variantInfoFilters$categoryVariantInfoFilters) > 0) {
    requestQuery$variantInfoFilters <- list(query$variantInfoFilters)
  }

  requestPayload = list(query = requestQuery, resourceUUID = query$resourceId)
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
runQuery <- function(query, resultType = NULL) {
  if(is.null(resultType) || toupper(resultType) == "DATA_FRAME") {
    return (getResults(query))
  }
  if(toupper(resultType) == "COUNT") {
    return (getCount(query))
  }
  print("Unrecognized resultType parameter. Valid values are: ('COUNT', 'DATA_FRAME')")
}

#' Executes a query with a given queryUUID
#'
#' @param session The current session object
#' @param queryUUID The unique id of the query to run
#' @return The result of the query as a data frame
#' @examples
#'
#'# result <- picsure::getResultByQueryUUID(session, "e10882ae-542a-4e38-ae28-399e13ac38c1")
#'
#' @export
getResultByQueryUUID <- function(session, queryUUID) {
  queryJSON = "{}"
  response = postJSON(session, paste("query/", queryUUID, "/result", sep=""), queryJSON, responseDeserializer = NULL)
  return(read.csv(text=response, sep=',', check.names=FALSE))
}

#' Builds a query with the given query UUID
#'
#' @param session The current session object
#' @param queryUUID The query UUID as copied from PIC-SURE
#' @return A initialized query object that can be modified
#' @examples
#'
#' # query <- picsure::getQueryByUUID(session, "a2ba4b22-f85b-4388-a098-6c72cd55e9d3")
#'
#' @export
getQueryByUUID <- function(session, queryUUID) {
  response <- getJSON(session = session, paste("query/", queryUUID, "/metadata", sep=""), simplifyVector = FALSE)
  return(getQueryFromMetadata(session = session, queryJson = response$resultMetadata$queryJson$query))
}

getQueryFromMetadata = function(session, queryJson) {
  query <- newQuery(session = session)

  query$fields <- queryJson$fields
  query$requiredFields <- queryJson$requiredFields
  query$categoryFilters <- queryJson$categoryFilters
  query$anyRecordOf <- as.list(queryJson$anyRecordOf)
  query$numericFilters <- queryJson$numericFilters
  query$variantInfoFilters <- as.list(queryJson$variantInfoFilters[[1]])

  categoryVariantInfoFilters <- lapply(queryJson$variantInfoFilters[[1]]$categoryVariantInfoFilters, function(x) {
    if (is.data.frame(x)) {
      return(as.list(x[[1]]))
    } else if (is.list(x)) {
      return(x)
    } else if (is.character(x)) {
      return(list(x))
    }

  })

  cleaned_categoryVariantInfoFilters <- list()
  for (key in names(categoryVariantInfoFilters)) {
    value <- categoryVariantInfoFilters[[key]]

    if (is.list(value)) {
      non_empty_values <- lapply(value, function(x) if (length(x) > 0) x else NULL)
      non_empty_values <- non_empty_values[!sapply(non_empty_values, is.null)]
    }

    # We only keep the values that are non-empty.
    # We need to do this to ensure the json structure is produced correctly.
    if (!is.null(non_empty_values) && length(non_empty_values) > 0) {
      cleaned_categoryVariantInfoFilters[[key]] <- non_empty_values
    }
  }

  query$variantInfoFilters$categoryVariantInfoFilters <- cleaned_categoryVariantInfoFilters
  return (query)
}

getCount = function(query) {
  queryJSON = generateQueryJSON(query, expectedResultType = 'COUNT')
  httpResults = postJSON(query$session, "query/sync/", queryJSON, responseDeserializer = NULL)
  return (httpResults)
}

getResults = function(query) {
  queryJSON = generateQueryJSON(query, expectedResultType = 'DATAFRAME')
  response = postJSON(query$session, "query/sync/", queryJSON, responseDeserializer = NULL)
  return(read.csv(text=response, sep=',', check.names=FALSE))
}

#' Parses the query template and updates appropriate query fields. This may be incomplete,
#' but currently handles known query templates
parseQueryTemplate = function(query) {
  queryTemplateString = query$session$queryTemplate[[1]]
  if (!is.null(queryTemplateString) && queryTemplateString != "null") {
    queryTemplate = jsonlite::fromJSON(queryTemplateString, simplifyVector = FALSE, flatten = FALSE)
    if (!is.null(queryTemplate$categoryFilters) && length(queryTemplate$categoryFilters) > 0)
      query$categoryFilters = queryTemplate$categoryFilters
    if (!is.null(queryTemplate$fields) && length(queryTemplate$fields) > 0)
      query$fields = queryTemplate$fields
  }
  return (query)
}
