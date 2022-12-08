#' @import jsonlite stringr httr urltools
NULL

library(jsonlite)
library(stringr)
library(httr)
library(urltools)


getResources <- function(connection, resourceId = FALSE) {
  getJSON(connection, "info/resources")
}

#' Creates a session to access a PIC-SURE instance
#'
#' @param url Url to a PIC-SURE instance
#' @param token A user token to access this instance. This can be found in the "User Profile" tab
#' on the PIC-SURE site
#' @param psama_url (Optional) URL to override the default PSAMA endpoint
#' @return An object which provides access to a PIC-SURE instance
#' @examples
#'
#'# session <- picsure::initializeSession(url="http://your.server/PIC-SURE/", token="your-security-token")
#'
#' @export
initializeSession <- function(url, token, psama_url=FALSE, getDictionary = NULL) {
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
  # for some reason, these are returned with the ID as the key and the name as the value, which
  # is not useful
  resources = getResources(result)
  reversedResources = list()
  for (resourceName in names(resources)) {
    reversedResources[[resources[[resourceName]]]] <- resourceName
  }
  result$resources = reversedResources
  #todo: test this
  result$currentResource = reversedResources[[1]]

  message("Loading user profile...")
  result$profile = getProfile(result)
  result$queryTemplate = getQueryTemplate(result)
  message("Loading PIC-SURE dictionary (this may take several minutes)...")

  if (is.function(getDictionary)) {
    result$dictionary = getDictionary(result)
  } else {
    result$dictionary <- searchPicsure(result)
  }

  message("Loading genotypic annotations...")

  result$genotypeAnnotations = initializeGenotypeAnnotations(result)
  message("Initialization complete.")
  return (result)
}


#' @export
getGenotypeAnnotations <- function(session) {
  return (session$genotypeAnnotations)
}

initializeGenotypeAnnotations <- function(session) {
  result <- postJSON(session, paste("search/", session$resources$`auth-hpds`, sep = ""), "{\"query\":\"\"}")
  result <- result$results$info
  annotations = list()
  for (conceptName in names(result)) {
    concept = result[[conceptName]]
    annotations[[(length(annotations) + 1)]] = list(
      genomic_annotation = conceptName,
      description = concept$description,
      values = toString(concept$values),
      continuous = concept$continuous
    )
  }
  return(data.frame(do.call(rbind.data.frame, annotations)))
}

getProfile <- function(connection) {
  response = getJSON(connection, "user/me", TRUE)
  return(response)
}

getQueryTemplate <- function(connection) {
  response = getJSON(connection, "user/me/queryTemplate", TRUE)
  return(response)
}


postJSON <- function(connection, path, body, responseDeserializer = deserializeJSON) {
  full_url = paste(connection$url_picsure, path, sep="")
  response = POST(full_url, body=body, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',connection$token)))

  if (response$status_code != 200) {
    writeLines("HTTP response:")
    print(response$status_code)
    return(response)
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
  request = GET(urlstr, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',connection$token)))
  if (request$status_code != 200) {
    if (request$status_code == 401) {
      stop("ERROR: Bad security credentials.")
    } else {
      print(request)
      stop("ERROR: HTTP(S) Failed")
    }
  } else {
    return(jsonlite::fromJSON(content(request, type="text", encoding = "UTF-8")))
  }
}
