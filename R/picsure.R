#' @import jsonlite stringr httr urltools tidyverse purrr
NULL

library(jsonlite)
library(stringr)
library(httr)
library(urltools)
library(purrr)
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
  # Safely parse and set url_picsure for this instance of the PicSureConnection
  url_df = urltools::url_parse(url)
  url_df$path <- stringr::str_trim(url_df$path)
  if (isFALSE(str_detect(url_df$path, "/$"))) {
    url_df$path <- paste(url_df$path, "/", sep="")
  }

  result <- list(url_picsure=urltools::url_compose(url_df))

  # Safely parse and set the url_psama for this instance of PicSureConnection
  temp_path = str_split(url_df$path, "/")
  url_len = length(temp_path)
  temp_path[[1]][[url_len]] = "psama"
  url_df$path = str_flatten(temp_path[[1]], collapse="/")
  if (isFALSE(psama_url)) {
    result <- c(result, url_psama=urltools::url_compose(url_df))
  }else{
    psama_url_df = urltools::url_parse(psama_url)
    psama_url_df$path <- stringr::str_trim(psama_url_df$path)
    if (isFALSE(str_detect(psama_url_df$path, "/$"))) {
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

getProfile <- function(connection) {
  response = getJSON(connection, "user/me", TRUE)
  return(response)
}

getQueryTemplate <- function(connection) {
  response = getJSON(connection, "user/me/queryTemplate", TRUE)
  return(response)
}

fetchResources <- function(session, resourceId = FALSE) {
  getJSON(session, "info/resources")
}

postJSON <- function(connection, path, body, responseDeserializer = deserializeJSON) {
  full_url = paste(connection$url_picsure, path, sep="")
  response = POST(full_url, body=body, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',connection$token)))

  if (response$status_code != 200) {
    if (response$status_code == 401) {
      stop("ERROR: Bad security credentials.")
    } else {
      stop(paste("ERROR: Request failed: ", response$status_code, ""))
    }
  } else {
    responseText <- content(response, type="text", encoding="UTF-8")
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


getJSON = function(connection, path, psama = FALSE) {
  urlstr = paste(ifelse(psama, connection$url_psama, connection$url_picsure), path, sep="")
  response = GET(urlstr, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',connection$token)))
  if (response$status_code != 200) {
    if (response$status_code == 401) {
      stop("ERROR: Bad security credentials.")
    } else {
      stop(paste("ERROR: Request failed: ", response$status_code, ""))
    }
  } else {
    return(jsonlite::fromJSON(content(response, type="text", encoding = "UTF-8")))
  }
}
