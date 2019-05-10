# PIC-SURE HPDS Adapter Library

#' export
Adapter <- R6::R6Class("PicSureHpdsAdapter",
                                  portable = FALSE,
                                  lock_objects = FALSE,
                                  public = list(
                                    initialize = function(PicSureConnection) {
                                      self$connection <- PicSureConnection
                                    },
                                    help = function() {
                                      cat("        [HELP] PicSureHpdsLib::Adapter$new(picsure_connection)\n")
                                      cat("            $version()                      gives version information for library\n")
                                      cat("            $list()                         lists available resources\n")
                                      cat("            $useResource(resource_uuid)     returns an object for selected resource\n")
                                      invisible(self)
                                    },
                                    version = function() {
                                      invisible(self)
                                    },
                                    list = function() {},
                                    useResource = function(resource_uuid) {
                                      return(PicSureHpdsResourceConnection$new(self$connection, resource_uuid))
                                    }
                                  )
)


#' export
PicSureHpdsResourceConnection <- R6::R6Class("PicSureHpdsResourceConnection",
                                  portable = FALSE,
                                  lock_objects = FALSE,
                                  public = list(
                                    initialize = function(connection, resource_uuid) {
                                      self$connection_reference <- connection
                                      self$resourceUUID <- resource_uuid
                                    },
                                    help = function() {
                                      cat("
        [HELP] PicSureHpdsLib::Adapter$useResource(resource_uuid)
            $dictionary()       Used to access data dictionary of the resource
            $query()            Used to query against data in the resource
            
        [ENVIRONMENT]")
                                    },
                                    dictionary = function() {
                                      return(PicSureHpdsDictionary$new(self))
                                    },
                                    query = function() {
                                      return(PicSureHpdsQuery$new(self))
                                    }
                                  )
)









#' export
BypassAdapter <- R6::R6Class("PicSureHpdsBypassAdapter",
                                        portable = FALSE,
                                        lock_objects = FALSE,
                                        public = list(
                                          initialize = function(url_arg, token_arg) {
                                            # trim and make sure URL ends in "/"
                                            endpoint <- str_trim(url_arg)
                                            if (str_detect(endpoint, "/$") == FALSE) {
                                              endpoint <- paste(endpoint, "/", sep="")
                                            }
                                            self$url <- endpoint
                                            self$token <- token_arg
                                            self$connection_reference <- PicSureHpdsLib::PicSureHpdsBypassConnection$new(self$url, self$token)
                                          },
                                          useResource = function(resource_uuid=FALSE) {
                                            temp <- PicSureHpdsLib::PicSureHpdsResourceConnection$new(self$connection_reference, resource_uuid)
                                            return(temp)
                                          },
                                          help = function() {
                                            cat("
        [HELP] PicSureHpdsLib::BypassAdapter$new(HPDS_Url, Security_Token)
            $version()                      gives version information for library
            $list()                         lists available resources
            $useResource(resource_uuid)     returns an object for selected resource
                                            ")
                                          }
                                        )
)

#' export
PicSureHpdsBypassConnection <- R6::R6Class("PicSureHpdsBypassConnection",
                                           portable = FALSE,
                                           lock_objects = FALSE,
                                           public = list(
                                             initialize = function(url_arg, token_arg) {
                                               self$url <- url_arg
                                               self$token <- token_arg
                                             },
                                             INTERNAL_api_obj = function() {
                                               return(PicSureHpdsBypassConnectionAPI$new(self$url, self$token))
                                             },
                                             help = function() {
                                               cat("
        [HELP] PicSureHpdsLib::PicSureHpdsBypassConnection(url, token)
            $help()                 Show help
                                               ")
                                               invisible(self)
                                             }
                                           )
)

                               
#' export
PicSureHpdsBypassConnectionAPI <- R6::R6Class("PicSureHpdsBypassConnectionAPI",
                                              portable = FALSE,
                                              lock_objects = FALSE,
                                              public = list(
                                                initialize = function(arg_url, arg_token) {
                                                  self$url <- arg_url
                                                  self$token <- arg_token
                                                },
                                                info = function(resource_uuid) {
                                                  writeLines(resource_uuid) 
                                                },
                                                search = function(resource_uuid, query=FALSE) { 
                                                  full_url = paste(self$url, "search", sep="")
                                                  if (query == FALSE) {
                                                    query <- list()
                                                    query$query <- ""
                                                    query = jsonlite::toJSON(query, auto_unbox=TRUE)
                                                  }
                                                  request = POST(full_url, body=query, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                                  if (request$status_code != 200) {
                                                    writeLines("ERROR: HTTP response was bad")
                                                    print(request)
                                                    return('{"results":{}, error":"True"}') 
                                                  } else {
                                                    return(content(request, "text"))
                                                  }
                                                },
                                                asynchQuery = function(resource_uuid, query) { writeLines(c(resource_uuid, query)) },
                                                synchQuery = function(resource_uuid, query) { 
                                                  full_url = paste(self$url, "query/sync/", resource_uuid, sep="")
                                                  if (query == FALSE) {
                                                    query <- list()
                                                    query$query <- ""
                                                    query = jsonlite::toJSON(query, auto_unbox=TRUE)
                                                  }
                                                  request = POST(full_url, body=query, content_type_json(), accept_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                                  if (request$status_code != 200) { 
                                                    writeLines("ERROR: HTTP response was bad")
                                                    print(request)
                                                    return('{"results":{}, error":"True"}') 
                                                  } else {
                                                    return(content(request, text))
                                                  }
                                                },
                                                queryStatus = function(resource_uuid, query_uuid) { writeLines(c(resource_uuid, query_uuid)) },
                                                queryResult = function(resource_uuid, query_uuid) { writeLines(c(resource_uuid, query_uuid)) },
                                                help = function() {
                                                  invisible(self)
                                                }
                                              )
)
