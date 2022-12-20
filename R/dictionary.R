library(purrr)

#' Performs a search of variables in PIC-SURE for a given keyword
#'
#' @param session Current session
#' @param keyword Keyword to search for
#' @param resultType Optional parameter to specify result type: Valid values:
#' DATA_FRAME (default) which converts the response to a data frame
#' DICTIONARY which returns a list
#' VARIABLE_PATHS which returns the unique variable paths
#' @return A PIC-SURE session
#' @examples
#'
#' searchResults <- picsure::searchPicsure(session, "heart", resultType = "DATA_FRAME")
#'
#' @export
searchPicsure <- function(session, keyword = "", resultType = "DATA_FRAME") {
  searchQuery = jsonlite::toJSON(list(query = keyword), auto_unbox=TRUE)
  result <- postJSON(session, paste("search/", session$currentResource, sep=""), searchQuery)

  if(toupper(resultType) == "DICTIONARY")
    return (result$results)
  if (toupper(resultType) == "VARIABLE_PATHS")
    return (unique(result$name))
  if (toupper(resultType) == "DATA_FRAME")
    return (getDataFrame(result))

  return (result)
}

#' @export
getDataFrame <- function(results) {
  mappedResults <- results$results$phenotypes %>% map(mapPhenotypeResult)
  mappedResultsDF <- data.frame(do.call(rbind.data.frame, mappedResults))
  mappedInfoResults <- results$results$info %>% map(mapInfoResult)
  mappedInfoResultsDF <- data.frame(do.call(rbind.data.frame, mappedInfoResults))
  return (list(
    phenotypes = mappedResultsDF,
    info = mappedInfoResultsDF
  ))
}

mapPhenotypeResult = function(result) {
  if (result$categorical == TRUE) {
    result$categoryValues <- toString(result$categoryValues)
    result$min <- NA
    result$max <- NA
  } else {
    result$categoryValues <- NA
  }
  result$HpdsDataType <- "phenotypes"
  if (is.null(result[["description"]])) {
    result$description <- NA
  }
  for (name in names(result)) {
    if (is.list(result[[name]])) {
      result[[name]] <- NA
    }
  }
  return (result)
}


mapInfoResult = function(result) {
  if (!is.null(result$continuous)) {
    result$categorical = !result$continuous
  }
  if (result$categorical == TRUE) {
    result$categoryValues <- toString(result$categoryValues)
    result$min <- NA
    result$max <- NA
  } else {
    result$categoryValues <- NA
    if (is.null(result[["max"]])) {
      result$max <- NA
    }
    if (is.null(result[["min"]])) {
      result$min <- NA
    }
  }
  result$HpdsDataType <- "info"
  if (is.null(result[["description"]])) {
    result$description <- NA
  }
  for (name in names(result)) {
    if (is.list(result[[name]])) {
      result[[name]] <- NA
    }
  }
  return (result)
}
