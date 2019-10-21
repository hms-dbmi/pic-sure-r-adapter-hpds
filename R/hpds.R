#' Get the details of a PIC-SURE resource.
#'
#' @export
#' @param connection A PIC-SURE connection object.
#' @param resourceUUID The UUID identity of a Resource hosted via the PIC-SURE connection.
#' @param verbose Flag to display additional runtime information.
#' @return A string containing details of the requested Resource.
resource.details <- function(connection, resourceUUID, verbose=FALSE) {
  message("not finished")
  stop()
}


#' Get a new instance reference of a PIC-SURE resource.
#'
#' @export
#' @param connection A PIC-SURE connection object.
#' @param resourceUUID The UUID identity of a Resource hosted via the PIC-SURE connection.
#' @param verbose Flag to display additional runtime information.
#' @return An object which provides access to the requested Resource.
get.resource <- function(connection, resourceUUID, verbose=FALSE) {
  message("not finished")
  stop()
}


# ===== data dictionary functions =====


#' Search the data dictionary of a PIC-SURE resource.
#'
#' @export
#' @param resource A PIC-SURE resource object.
#' @param term A string to search for in the resource's data dictionary.
#' @param verbose Flag to display additional runtime information.
#' @return An object representing the search results.
find.in.dictionary <- function(resource, term, verbose=FALSE){
  if (class(resource) == "PicSure_Resource") {
    dictionaryObj = resource$dictionary()
    class(dictionaryObj) <- "Hpds_Dictionary"
    result = dictionaryObj$find(term)
    class(result) <- "Hpds_DictionaryResults"
    return(result)
  } else {
    message("Invalid resource was passed to find.in.dictionary() function")
    stop()
  }
}


#' Extract the number of results in the given data dictionary lookup.
#'
#' @export
#' @param dictionary.results A data dictionary search results object.
#' @param verbose Flag to display additional runtime information.
#' @return An integer of how many data dictionary entries were found in
extract.count <- function(dictionary.results, verbose=FALSE) {
  if (class(dictionary.results) == "Hpds_DictionaryResults") {
    result = dictionary.results$count()
    return(result)
  } else {
    message("Invalid dictionary results was passed to extract.count() function")
    stop()
  }
}


#' Extract the unique keys of all the results in a given data dictionary lookup.
#'
#' @export
#' @param dictionary.results A data dictionary search results object.
#' @param verbose Flag to display additional runtime information.
#' @return A list of unique keys for all the data dictionary search results.
extract.keys <- function(dictionary.results, verbose=FALSE) {
  message("not finished")
  stop()
}


#' Extract the unique keys of all the results in a given data dictionary lookup.
#'
#' @export
#' @param dictionary.results A dictionary results object.
#' @param verbose Flag to display additional runtime information.
#' @return A collection of result entries.
extract.entries <- function(dictionary.results, verbose=FALSE) {
  message("not finished")
  stop()
}


#' Extract all the results of a given data dictionary lookup in a data frame format.
#'
#' @export
#' @param dictionary.results A dictionary results object.
#' @param verbose Flag to display additional runtime information.
#' @return Results in a dataframe format.
extract.dataframe <- function(dictionary.results, verbose=FALSE) {
  message("not finished")
  stop()
}


# ===== query functions =====


#' Create a new query instance with no restrictions.
#'
#' @export
#' @param resource A resource object that the returned query object will execute against.
#' @param verbose Flag to display additional runtime information.
new.query <- function(resource, verbose=FALSE) {
  if (class(resource) == "Hpds_Resource") {
    result = resource$query()
    class(result) <- "Hpds_Query"
    return(result)
  } else {
    message("The resource given to new.query() is not a Hpds_Query typed object")
    stop()
  }
}


#' Run a query instance using any restrictions that have been added to it.
#'
#' @export
#' @param query A query instance object.
#' @param result.type A string specifying what type of results to return. Possible values: "count", "results", "dataframe" and "crosscount".
#' @param verbose Flag to display additional runtime information.
query.run <- function(query, result.type="dataframe", verbose=FALSE) {

  if (class(query) == "Hpds_Query") {
    result = switch(result.type,
                    "count" = query$getCount(),
                    "results" = query$getResults(),
                    "dataframe" = query$getResultsDataFrame(),
                    "crosscount" = query$getResultsCrossCounts()
    )
    return(result)
  } else {
    message("The resource given to query.run() is not a Hpds_Query typed object")
    stop()
  }
}


#' Add a "select" restriction to a query instance.
#'
#' @export
#' @param query A query instance object.
#' @param keys One or more keys to add to the given query object's select list.
#' @param verbose Flag to display additional runtime information.
query.select.add <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$select()$add(keys)
  } else {
    message("The query given to query.select.add() is not a Hpds_Query typed object")
    stop()
  }
}


#' Delete a "select" restriction from a query instance.
#'
#' @export
#' @param query A query instance object.
#' @param keys One or more keys to delete from the given query object's select list.
#' @param verbose Flag to display additional runtime information.
query.select.delete <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$select()$delete(keys)
  } else {
    message("The query given to query.select.delete() is not a Hpds_Query typed object")
    stop()
  }
}


#' Add a "cross-counts" restriction to a query instance.
#'
#' @export
#' @param query A query instance object.
#' @param keys One or more keys to add to the given query object's cross-count list.
#' @param verbose Flag to display additional runtime information.
query.crosscounts.add <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$crosscounts()$add(keys)
  } else {
    message("The query given to query.crosscounts.add() is not a Hpds_Query typed object")
    stop()
  }
}


#' Delete a "cross-counts" restriction from a query instance.
#'
#' @export
#' @param query A query instance object.
#' @param keys One or more keys to delete from the given query object's cross-count list.
#' @param verbose Flag to display additional runtime information.
query.crosscounts.delete <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$crosscounts()$delete(keys)
  } else {
    message("The query given to query.crosscounts.delete() is not a Hpds_Query typed object")
    stop()
  }
}


#' Add a "require" restriction to a query instance.
#'
#' @export
#' @param query A query instance object.
#' @param keys One or more keys to add to the given query object's require list.
#' @param verbose Flag to display additional runtime information.
query.require.add <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$require()$add(keys)
  } else {
    message("The query given to query.require.add() is not a Hpds_Query typed object")
    stop()
  }
}


#' Delete a "require" restriction from a query instance.
#'
#' @export
#' @param query A query instance object.
#' @param keys One or more keys to delete from the given query object's require list.
#' @param verbose Flag to display additional runtime information.
query.require.delete <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$require()$delete(keys)
  } else {
    message("The query given to query.require.delete() is not a Hpds_Query typed object")
    stop()
  }
}


#' Add an "AnyOf" restriction from a query instance.
#'
#' @export
#' @param query A query instance object.
#' @param keys One or more keys to add to the given query object's AnyOf list.
#' @param verbose Flag to display additional runtime information.
query.anyof.add <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$anyof()$add(keys)
  } else {
    message("The query given to query.anyof.add() is not a Hpds_Query typed object")
    stop()
  }
}


#' Delete an "AnyOf" restriction from a query instance.
#'
#' @export
#' @param query A query instance object.
#' @param keys One or more keys to delete from the given query object's AnyOf list.
#' @param verbose Flag to display additional runtime information.
query.anyof.delete <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$anyof()$delete(keys)
  } else {
    message("The query given to query.anyof.delete() is not a Hpds_Query typed object")
    stop()
  }
}


#' Add a "Filter" restriction to a query instance.
#'
#' @export
#' @param query A query instance object.
#' @param keys One or more keys to add to the given query object's filter list.
#' @param verbose Flag to display additional runtime information.
query.filter.add <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$filter()$add(keys)
  } else {
    message("The query given to query.filter.add() is not a Hpds_Query typed object")
    stop()
  }
}


#' Delete a "Filter" restriction from a query instance.
#'
#' @export
#' @param query A query instance object.
#' @param keys One or more keys to delete from the given query object's filter list.
#' @param verbose Flag to display additional runtime information.
query.filter.delete <- function(query, keys, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    query$filter()$delete(keys)
  } else {
    message("The query given to query.filter.delete() is not a Hpds_Query typed object")
    stop()
  }
}


#' Prints to screen the contents of a query instance's restriction parameters.
#'
#' @export
#' @param query A query instance object.
#' @param verbose Flag to display additional runtime information.
query.show <- function(query, verbose=FALSE) {
  if (class(query) == "Hpds_Query") {
    result = query$show()
    return(result)
  }
}
