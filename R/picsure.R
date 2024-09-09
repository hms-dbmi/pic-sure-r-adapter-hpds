#' @import jsonlite stringr httr urltools tidyverse
NULL

library(jsonlite)
library(stringr)
library(httr)
library(urltools)
library(tidyverse)


#' Creates a session to access a PIC-SURE instance
#'
#' @param url Url to a PIC-SURE instance
#' @param token A user token to access this instance. This can be found in the "User Profile" tab
#' on the PIC-SURE site
#' @param psama_url (Optional) URL to override the default PSAMA endpoint
#' @param initializeDictionary (Optional, for internal use only) Function to override dictionary initialization
#' @return An object which provides access to a PIC-SURE instance
#' @examples
#'
#'# session <- picsure::initializeSession(url="http://your.server/PIC-SURE/", token="your-security-token")
#'
#' @export
initializeSession <- function(url, token, psama_url=FALSE, initializeDictionary = NULL, defaultResource = NULL) {
  # Safely parse and set url_picsure
  url_df = urltools::url_parse(url)
  url_df$path <- stringr::str_trim(url_df$path)
  if (isFALSE(stringr::str_detect(url_df$path, "/$"))) {
    url_df$path <- paste(url_df$path, "/", sep="")
  }

  result <- list(url_picsure=urltools::url_compose(url_df))

  # Safely parse and set the url_psama
  temp_path = stringr::str_split(url_df$path, "/")
  url_len = length(temp_path)
  temp_path[[1]][[url_len]] = "psama"
  url_df$path = stringr::str_flatten(temp_path[[1]], collapse="/")
  if (isFALSE(psama_url)) {
    result <- c(result, url_psama=urltools::url_compose(url_df))
  }else{
    psama_url_df = urltools::url_parse(psama_url)
    psama_url_df$path <- stringr::str_trim(psama_url_df$path)
    if (isFALSE(stringr::str_detect(psama_url_df$path, "/$"))) {
      psama_url_df$path <- paste(psama_url_df$path, "/", sep="")
    }
    result <- c(result, url_psama = urltools::url_compose(psama_url_df))
  }
  result <- c(result, token = token)

  message("Loading resources...")
  # for some reason, these are returned with the ID as the key and the name as
  # the value, which is not useful
  resources = fetchResources(result)
  reversedResources = list()
  for (resourceName in names(resources)) {
    reversedResources[[resources[[resourceName]]]] <- resourceName
  }
  result$resources = reversedResources

  if (length(reversedResources) > 1) {
    if (!is.null(defaultResource)) {
      if (!is.null(reversedResources[[defaultResource]])) {
        result$currentResource = reversedResources[[defaultResource]]
      } else {
        print("Default resource not found, defaulting to:")
        print(reversedResources[1])
        result$currentResource = reversedResources[[1]]
      }
    } else {
      print("Multiple resources found, defaulting to:")
      print(reversedResources[1])
      result$currentResource = reversedResources[[1]]
    }
  } else {
    result$currentResource = reversedResources[[1]]
  }

  message("Loading user profile...")
  result$profile = getProfile(result)
  result$queryTemplate = getQueryTemplate(result)
  message("Loading PIC-SURE dictionary (this may take several minutes)...")

  if (is.function(initializeDictionary)) {
    searchResult <- initializeDictionary(result)
  } else {
    searchResult <- searchPicsure(result)
  }
  result$dictionary <- searchResult$phenotypes
  result$genomicAnnotations = searchResult$info

  message("Initialization complete.")
  return (result)
}


#' Returns the genomic annotations for this session, as a data frame
#'
#' @param session Current PIC-SURE session
#' @export
getGenomicAnnotations <- function(session) {
  return (session$genomicAnnotations)
}

#' Returns a list of resources for this session
#'
#' @param session Current PIC-SURE session
#' @export
getResources <- function(session) {
  return (session$resources)
}

getProfile <- function(session) {
  response = getJSON(session, "user/me", TRUE)
  return(response)
}

getQueryTemplate <- function(session) {
  response = getJSON(session, "user/me/queryTemplate", TRUE)
  return(response)
}

fetchResources <- function(session, resourceId = FALSE) {
  getJSON(session, "info/resources")
}

#' Make a POST request to the PIC-SURE service. Handles setting content type, and required headers. Throws an exception if there are
#' any errors, or if the response code is not 200.
#'
#' @param session The current PIC-SURE session
#' @param path A url path to POST to
#' @param body A string of JSON data to post to the service
#' @param responseDeserializer A function to deserialize the data returned. Default is deserializeJSON
#' @return a string containing the response from the request, or the result of responseDeserializer, if provided
postJSON <- function(session, path, body, responseDeserializer = deserializeJSON, acceptContentType = httr::accept_json()) {
  full_url = paste(session$url_picsure, path, sep="")
  response = httr::POST(full_url, body=body, httr::content_type_json(), acceptContentType, httr::add_headers(Authorization=paste('Bearer',session$token)))

  if (response$status_code != 200) {
    if (response$status_code == 401) {
      stop("ERROR: Bad security credentials.")
    } else {
      stop(paste("ERROR: Request failed: ", response$status_code, ""))
    }
  } else {
    responseText <- httr::content(response, type="text", encoding="UTF-8")
    if (is.function(responseDeserializer)) {
      return (responseDeserializer(responseText))
    }
    return (responseText)
  }
}

deserializeJSON = function(response) {
  response <- gsub("\\xef\\xbb\\xbf", "", response, useBytes = T) # strip BOM characters that are in the json data
  return(jsonlite::fromJSON(response, simplifyVector=FALSE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE))
}

#' Make a GET request to the PIC-SURE service. Handles setting content type, and required headers. Throws an exception if there are
#' any errors, or if the response code is not 200.
#'
#' @param session The current PIC-SURE session
#' @param path A url path to make a GET request to
#' @param simplifyVector If set to FALSE vectors will not be modified on parse
#' @return the result of the request, deserialized from JSON using jsonlite
getJSON = function(session, path, psama = FALSE, simplifyVector = TRUE) {
  urlstr = paste(ifelse(psama, session$url_psama, session$url_picsure), path, sep="")
  response = httr::GET(urlstr, httr::content_type_json(), httr::accept_json(), httr::add_headers(Authorization=paste('Bearer',session$token)))
  if (response$status_code != 200) {
    if (response$status_code == 401) {
      stop("ERROR: Bad security credentials.")
    } else {
      stop(paste("ERROR: Request failed: ", response$status_code, ""))
    }
  } else {
    return(jsonlite::fromJSON(httr::content(response, type="text", encoding = "UTF-8"), simplifyVector = simplifyVector))
  }
}
