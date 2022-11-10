library(jsonlite)
library(stringr)
library(httr)
library(urltools)

getResources <- function(connection, resourceId = FALSE) {
  getJSON(connection, "info/resources")
}

connect <- function(url, token, psama_url=FALSE, getDictionary = NULL) {
  # Safely parse and set url_picsure for this instance of the PicSureConnection
  url_df = urltools::url_parse(url)
  url_df$path <- str_trim(url_df$path)
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
    psama_url_df$path <- str_trim(psama_url_df$path)
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
  message("Loading user profile...")
  result$profile = getProfile(result)
  result$queryTemplate = getQueryTemplate(result)
  message("Loading variables...")

  if (is.function(getDictionary)) {
    result$dictionary = getDictionary(result)
  } else {
    result$dictionary <- searchPicsure(result)
  }
  message("Initialization complete.")
  return (result)
}

getProfile <- function(connection) {
  response = getJSON(connection, "user/me", TRUE)
  return(response)
}

getQueryTemplate <- function(connection) {
  response = getJSON(connection, "user/me/queryTemplate", TRUE)
  return(response)
}


postJSON <- function(connection, path, body) {
  full_url = paste(connection$url_picsure, path, sep="")
  response = POST(full_url, body=body, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',connection$token)))

  if (response$status_code != 200) {
    writeLines("HTTP response:")
    print(response$status_code)
    return(response)
  } else {
    response <- gsub("\\xef\\xbb\\xbf", "", content(response, type="text", encoding="UTF-8"), useBytes = T) # strip BOM characters that are in the json data
    return(jsonlite::fromJSON(response, simplifyVector=FALSE, simplifyDataFrame=FALSE, simplifyMatrix=FALSE))
  }
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
