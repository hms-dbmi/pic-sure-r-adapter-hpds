NULL

newQuery <- function(connection) {
  query = list(connection = connection)

  query$resourceId <- determineQueryResource(connection)

  query$fields = list()
  query$requiredFields <- list()
  query$numericFilters <- list()
  query$categoryFilters <- list()
  #query$variantInfoFilters <- list()
  query$anyRecordOf <- list()
  query$requiredFields <- list()
  query$crossCountFields <- list()

  # TODO: load the default queryTemplate values. BDC only?
  # self$load(self$connection$profile_info$queryTemplate)
  return (query)
}

addFilter <- function(query, keys, method = "FILTER", min = NULL, max = NULL, categories = NULL) {
  if (typeof(keys) != "list") {
    keys <- list(keys)
  }
  variablesToAdd <- lookupVariables(query, keys)


  if(toupper(method) == "FILTER") {
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


  if(toupper(method) == "SELECT") {
    if (nrow(variablesToAdd) != length(keys)) {
      message("Not all variables were valid. Only the following will be added:")
      print(variablesToAdd[,1])
    }
    query$fields <- c(query$fields, variablesToAdd[,1])
    return (query)
  }
}

lookupVariables <- function(query, keys) {
  return (query$connection$dictionary[connection$dictionary$name %in% keys, ])
}

determineQueryResource = function(connection) {
  #todo
  return ("02e23f52-f354-4e8b-992c-d37c8b9ba140")
}

generateQueryJSON = function(query, expectedResultType) {
  requestQuery = list(
    fields = query$fields,
    #requiredFields = query$requiredFields,
    numericFilters = query$numericFilters,
    categoryFilters = query$categoryFilters,
    #query$variantInfoFilters,
    #anyRecordOf = query$anyRecordOf,
    requiredFields = query$requiredFields
    #crossCountFields = query$crossCountFields
  )
  requestPayload = list(query = requestQuery, resourceUUID = query$connection$resources$hpds)
  requestPayload$query[['expectedResultType']] = expectedResultType

  queryJSON = jsonlite::toJSON(requestPayload, auto_unbox = TRUE)
  # bugfix for jsonlite !!!! DO NOT REFACTOR BELOW 5 LINES AS R WILL MESS THINGS UP!
  queryJSON <- gsub('\\[\\[\\]\\]','\\[\\]', queryJSON)
  queryJSON <- gsub('"fields":\\{\\}','"fields":\\[\\]', queryJSON)
  queryJSON <- gsub('"requiredFields":\\{\\}','"requiredFields":\\[\\]', queryJSON)
  queryJSON <- gsub('"numericFilters":\\[\\]','"numericFilters":\\{\\}', queryJSON)
  queryJSON <- gsub('"categoryFilters":\\[\\]','"categoryFilters":\\{\\}', queryJSON)
  queryJSON <- gsub('"categoryVariantInfoFilters":\\[\\]','"categoryVariantInfoFilters":\\{\\}', queryJSON)
  queryJSON <- gsub('"numericVariantInfoFilters":\\[\\]','"numericVariantInfoFilters":\\{\\}', queryJSON)

  return (queryJSON)
}

getCount = function(query) {
  queryJSON = generateQueryJSON(query, expectedResultType = 'COUNT')

  print(queryJSON)
  httpResults = postJSONRaw(query$connection, "query/sync/", queryJSON)
  return (httpResults)
}

getResults = function(query) {
  queryJSON = generateQueryJSON(query, expectedResultType = 'DATAFRAME')

  print(queryJSON)
  response = postJSONRaw(query$connection, "query/sync/", queryJSON)

  return(read.csv(text=response, sep=','))
}
