NULL

newQuery <- function(connection) {
  query = list(connection = connection)

  query <- c(query, resourceId = determineQueryResource(connection))
  #todo
  query <- c(query, dictionary = searchPicsure(connection))

  query <- c(query, listSelect = list())
  query <- c(query, listFilter = list())
  query <- c(query, listAnyOf = list())
  query <- c(query, listRequire = list())
  query <- c(query, listCrossCounts = list())

  # TODO: load the default queryTemplate values. BDC only?
  # self$load(self$connection$profile_info$queryTemplate)
  return (query)
}

addFilter <- function(query, variablePath, method = "FILTER", min = NULL, max = NULL, categories = NULL) {
  if(toupper(method) == "FILTER") {

    return (query)
  }
}

determineQueryResource = function(connection) {
  #todo
  return ("02e23f52-f354-4e8b-992c-d37c8b9ba140")
}