# PIC-SURE HPDS Adapter Library

#' export
Adapter <- R6::R6Class("PicSureHpdsAdapter",
                                  portable = FALSE,
                                  lock_objects = FALSE,
                                  public = list(
                                    initialize = function(PicSureConnection) {
                                      self$connection_reference <- PicSureConnection
                                    },
                                    help = function() {
                                      cat("        [HELP] PicSureHpdsLib::Adapter$new(picsure_connection)\n")
                                      cat("            $version()                      gives version information for library\n")
                                      cat("            $list()                         lists available resources\n")
                                      cat("            $useResource(resource_uuid)     returns an object for selected resource\n")
                                      invisible(self)
                                    },
                                    version = function() {
                                      cat(paste("PicSureHpdsLib Library (version ", packageVersion("PicSureHpdsLib"), ")\n", sep=""))
                                      cat(paste("URL: ", self$connection_reference$url, "\n", sep=""))
                                      invisible(self)
                                    },
                                    list = function() {
                                      self$connection_reference$list()
                                    },
                                    useResource = function(resource_uuid) {
                                      return(PicSureHpdsResourceConnection$new(self$connection_reference, resource_uuid))
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
                                      if (missing(resource_uuid)) {
                                        self$resourceUUID <- FALSE
                                      } else {
                                        self$resourceUUID <- resource_uuid
                                      }
                                    },
                                    help = function() {
                                      cat("        [HELP] PicSureHpdsLib::Adapter$useResource(resource_uuid)\n")
                                      cat("            $dictionary()       Used to access data dictionary of the resource\n")
                                      cat("            $query()            Used to query against data in the resource\n")
                                      cat("        [ENVIRONMENT]\n")
                                      cat(paste("            URL: \n", self$connection_reference$url, sep=""))
                                      cat(paste("  Resource UUID: \n", self$resourceUUID, sep=""))
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
                                        inherit = Adapter,
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
                                          useResource = function(resource_uuid) {
                                            if (missing(resource_uuid)) {
                                              temp <- PicSureHpdsLib::PicSureHpdsResourceConnection$new(self$connection_reference, FALSE)
                                            } else {
                                              temp <- PicSureHpdsLib::PicSureHpdsResourceConnection$new(self$connection_reference, resource_uuid)
                                            }
                                            return(temp)
                                          },
                                          list = function() {
                                            self$connection_reference$list()
                                          },
                                          help = function() {
                                            cat("        [HELP] PicSureHpdsLib::BypassAdapter$new(HPDS_Url, Security_Token)\n")
                                            cat("            $version()                      gives version information for library\n")
                                            cat("            $list()                         lists available resources\n")
                                            cat("            $useResource(resource_uuid)     returns an object for selected resource\n")
                                          }
                                        )
)


#' export
PicSureHpdsBypassConnection <- R6::R6Class("PicSureHpdsBypassConnection",
                                           portable = FALSE,
                                           lock_objects = FALSE,
                                           private = list(
                                           ),
                                           public = list(
                                             initialize = function(url_arg, token_arg) {
                                               self$url <- url_arg
                                               self$token <- token_arg
                                             },
                                             list = function() {
                                               res = self$getResources()
                                               if (nrow(res) > 0) {
                                                 print(paste(stringr::str_pad("+", 39, pad="-"), stringr::str_pad("+", 55, pad="-"), sep=""))
                                                 print(paste(stringr::str_pad("| Resource UUID", 38, pad=" "), stringr::str_pad("| Resource Name", 55, pad=" "), sep=""))
                                                 print(paste(stringr::str_pad("+", 39, pad="-"), stringr::str_pad("+", 55, pad="-"), sep=""))
                                                 for (idx in 1:nrow(res)) {
                                                   print(paste(stringr::str_pad(paste("|", res$uuid[[idx]], sep=""), 34, pad=" "), "| ", res$name[[idx]]))
                                                   print(paste("| Description: ", res$description[[idx]], sep=""))
                                                   print(paste(stringr::str_pad("+", 39, pad="-"), stringr::str_pad("+", 55, pad="-"), sep=""))
                                                 }
                                               }
                                             },
                                             INTERNAL_api_obj = function() {
                                               return(PicSureHpdsBypassConnectionAPI$new(self$url, self$token))
                                             },
                                             getResources = function() {
                                               full_url = paste(self$url, "info", sep="")
                                               request = POST(full_url, body="{}", content_type_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                               if (request$status_code != 200) { 
                                                 print("ERROR: HTTP response was bad")
                                                 print(request)
                                                 return(jsonlite::fromJSON('[]')) 
                                               } else {
                                                 results = content(request, "parsed")
                                                 ret = paste('[{"uuid":"', results[["id"]], '", "name":"', results[["name"]], '", "description":"[Resource accessed directly (bypassing PIC-SURE framework)]"}]')
                                                 return(jsonlite::fromJSON(ret))
                                               }
                                             },
                                             help = function() {
                                               cat("        [HELP] PicSureHpdsLib::PicSureHpdsBypassConnection(url, token)\n")
                                               cat("            $list()                 List resources\n")
                                               cat("            $help()                 Show help\n")
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
                                                  # trim and make sure URL ends in "/"
                                                  endpoint <- str_trim(arg_url)
                                                  if (str_detect(endpoint, "/$") == FALSE) {
                                                    endpoint <- paste(endpoint, "/", sep="")
                                                  }
                                                  self$url <- endpoint
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
                                                  full_url = paste(self$url, "query/sync/", sep="")
                                                  if (query == FALSE) {
                                                    query <- list()
                                                    query$query <- ""
                                                    query = jsonlite::toJSON(query, auto_unbox=TRUE)
                                                  }
                                                  request = POST(full_url, body=query, content_type_json(), add_headers(Authorization=paste('Bearer',self$token)))
                                                  if (request$status_code != 200) { 
                                                    writeLines("ERROR: HTTP response was bad")
                                                    print(request)
                                                    return('{"results":{}, error":"True"}') 
                                                  } else {
                                                    return(content(request, "text"))
                                                  }
                                                },
                                                queryStatus = function(resource_uuid, query_uuid) { writeLines(c(resource_uuid, query_uuid)) },
                                                queryResult = function(resource_uuid, query_uuid) { writeLines(c(resource_uuid, query_uuid)) },
                                                help = function() {
                                                  invisible(self)
                                                }
                                              )
)
