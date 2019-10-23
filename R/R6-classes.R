library(hash)


# ======================
#    ADAPTER CODE
# ======================


#' R6 class that allows access to the data dictionary and query services of a selected HPDS-hosted resources on a PIC-SURE network.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access the dictionary and query services via the objects it returns.
#' @format \code{\link{PicSureHpdsResourceConnection}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
#'   \item{\code{new(connection, resource_uuid)}}{This method is used to create new object of this class which uses the passed PicSureConnection object for communication with the PIC-SURE network along with a UUID to identify a HPDS-hosted resource.}
#'
#'   \item{\code{dictionary()}}{This method returns a \code{PicSureHpdsDictionary} object which is used to run lookups against a resources data dictionary.}
#'   \item{\code{query()}}{This method returns a new \code{PicSureHpdsQuery} object configured to run all commands against the previously specified HPDS-hosted resource.}}
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
                                               version = function() {
                                                 cat(paste("PicSureHpdsLib Library (version ", packageVersion("PicSureHpdsLib"), ")\n", sep=""))
                                                 cat(paste("URL: ", self$connection_reference$url, "\n", sep=""))
                                                 invisible(self)
                                               },
                                               dictionary = function() {
                                                 return(PicSureHpdsDictionary$new(self))
                                               },
                                               query = function() {
                                                 return(PicSureHpdsQuery$new(self))
                                               }
                                             )
)


#' R6 class that selects a HPDS-hosted resources directly without the use of the PIC-SURE Connection library.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource without using a PIC-SURE network.
#' @format \code{PicSureHpdsBypassAdapter} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
#'   \item{\code{new(url, token)}}{This method is used to create new object of this class which connects directly to a HPDS-hosted resource.}
#'
#'   \item{\code{list()}}{This method prints a list of UUIDs of all resources hosted by the currently connected PIC-SURE network.}
#'   \item{\code{useResource(resource_uuid)}}{This method returns a new \code{PicSureHpdsResourceConnection} object configured to connect the specified HPDS-hosted resource on the PIC-SURE Network identified by the given \code{resource_uuid}.}}
BypassAdapter <- R6::R6Class("PicSureHpdsBypassAdapter",
                             portable = FALSE,
                             lock_objects = FALSE,
                             inherit = Adapter,
                             public = list(
                               initialize = function(url_arg, token_arg = FALSE) {
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
                               }
                             )
)


#' R6 class that is a code-shim used by the PicSureBypassAdapter - DO NOT USE THIS OBJECT DIRECTLY!
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used by the \code{PicSureBypassAdapter} to access a HPDS-hosted resource without using a PIC-SURE network.
#' @format \code{\link{PicSureHpdsBypassConnection}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}}
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
                                             }
                                           )
)


#' R6 class that is a code-shim used by the PicSureBypassConnection - DO NOT USE THIS OBJECT DIRECTLY!
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import httr
#' @import jsonlite
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used by the \code{PicSureHpdsBypassConnection} to access a HPDS-hosted resource without using a PIC-SURE network.
#' @format \code{\link{PicSureHpdsBypassConnectionAPI}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}}
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
                                                    return('{"results":{}, "error":"True"}')
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
                                                    return('{"results":{}, "error":"True"}')
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


# ========================
#    DICTIONARY CODE
# ========================


#' R6 class that runs searches against a HPDS resource's data dictionary - DO NOT CREATE THIS OBJECT DIRECTLY!
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
#' @format \code{\link{PicSureHpdsDictionary}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
#'   \item{\code{new(refHpdsResourceConnection)}}{This method is used to create new object of this class. DO NOT CREATE THIS OBJECT DIRECTLY!}
#'
#'   \item{\code{find(term=FALSE)}}{This method returns a \code{PicSureHpdsDictionaryResult} object containing the results of the search on the HPDS resource's data dictionary.}}
PicSureHpdsDictionary <- R6::R6Class("PicSureHpdsDictionary",
                                     portable = FALSE,
                                     lock_objects = FALSE,
                                     public = list(
                                       initialize = function(refHpdsResourceConnection) {
                                         self$connection <- refHpdsResourceConnection
                                         self$resourceUUID <- refHpdsResourceConnection$resourceUUID
                                         self$INTERNAL_API_OBJ <- refHpdsResourceConnection$connection_reference$INTERNAL_api_obj()
                                       },
                                       find = function(term=FALSE) {
                                         query <- list()
                                         if (term == FALSE) {
                                           query$query <- ""
                                         } else {
                                           query$query <- toString(term)
                                         }
                                         results = self$INTERNAL_API_OBJ$search(self$resourceUUID, jsonlite::toJSON(query, auto_unbox=TRUE))
                                         return(PicSureHpdsDictionaryResult$new(results))
                                       }
                                     )
)


#' R6 class contain the results of a search against a HPDS resource's data dictionary - DO NOT CREATE THIS OBJECT DIRECTLY!
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
#' @format \code{\link{PicSureHpdsDictionaryResult}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
#'   \item{\code{new(results)}}{This method is used to create new object of this class. DO NOT CREATE THIS OBJECT DIRECTLY!}
#'
#'   \item{\code{count()}}{This method returns a integer of how many terms were returned by the data dictionary search.}
#'   \item{\code{keys()}}{This method returns a vector of strings holding the unique record keys of the terms discovered by the data dictionary search.}
#'   \item{\code{entries()}}{This method returns information about the terms discovered by the data dictionary search.}
#'   \item{\code{DataFrame()}}{This method returns a dataframe containing the keys and record information that was returned by the data dictionary search.}}
PicSureHpdsDictionaryResult <- R6::R6Class("PicSureHpdsDictionaryResult",
                                           portable = FALSE,
                                           lock_objects = FALSE,
                                           public = list(
                                             initialize = function(results) {
                                               self$results <- jsonlite::fromJSON(results)
                                               updated_list <- list()
                                               if (!is.null(self$results$results$phenotypes)) {
                                                 # dictionary results are segmented
                                                 new_results <- list()
                                                 for (idx1 in 1:length(self$results$results)) {
                                                   result_type = names(self$results$results[idx1])
                                                   if (length(self$results$results[[idx1]]) > 0) {
                                                     for (idx2 in 1:length(self$results$results[[idx1]])) {
                                                       self$results$results[[idx1]][[idx2]]$HpdsDataType <- result_type
                                                       idx3 <- self$results$results[[idx1]][[idx2]]$name
                                                       updated_list[[idx3]] <- self$results$results[[idx1]][[idx2]]
                                                     }
                                                   }
                                                 }
                                                 self$results$results <- updated_list
                                               }
                                             },
                                             count = function() {
                                               return(length(self$results[[1]]))
                                             },
                                             keys = function() {
                                               return(names(self$results[['results']]))
                                             },
                                             entries = function() {
                                               return(self$results[['results']])
                                             },
                                             DataFrame = function() {
                                               #get a list of all vector names
                                               vn <- list()
                                               for (idx1 in 1:length(self$results[['results']])) {
                                                 # for each record
                                                 tn <- names(self$results[['results']][[idx1]])
                                                 # for each record's attributes
                                                 for (idx2 in 1:length(tn)) {
                                                   if (is.null(vn[[tn[idx2]]])) {
                                                     # the column name is new, save for later
                                                     vn[[tn[idx2]]] <- 1
                                                   }
                                                 }
                                               }
                                               vn <- names(vn)

                                               # we genrate a list of all attributes of all records, then pivot into vectors
                                               df <- data.frame()
                                               for (idx1 in 1:length(vn)) {
                                                 df[1,idx1] <- NA
                                               }
                                               names(df) <- vn
                                               for (idx1 in 1:length(self$results[['results']])) {
                                                 # for each record
                                                 e <- self$results[['results']][[idx1]]
                                                 for (idx2 in 1:length(vn)) {
                                                   # for each global named-attribute
                                                   if (is.null(e[[vn[idx2]]])) {
                                                     # attribute is missing on this record, save as NA
                                                     df[idx1,idx2] <- NA
                                                   } else {
                                                     # attribute is set for this record, save
                                                     if (length(e[[vn[idx2]]]) > 1) {
                                                       df[idx1,idx2] <- paste(e[[vn[idx2]]], collapse=",")
                                                     } else {
                                                       df[idx1,idx2] <- e[[vn[idx2]]]
                                                     }
                                                   }
                                                 }
                                               }
                                               return(df)
                                             }
                                           )
)


# ===================
#     QUERY CODE
# ===================


#' R6 class used to build a multi-use query to search against a HPDS resource's data - DO NOT CREATE THIS OBJECT DIRECTLY!
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @import stringr
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
#' @format \code{\link{PicSureHpdsQuery}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
#'   \item{\code{new(connection)}}{This method is used to create new object of this class. DO NOT CREATE THIS OBJECT DIRECTLY!}
#'
#'   \item{\code{show()}}{This method displays a list of all settings specified for the query.}
#'   \item{\code{select()}}{This method accesses a reference to a query parameter list.}
#'   \item{\code{crosscounts()}}{This method accesses a reference to a query parameter list.}
#'   \item{\code{require()}}{This method accesses a reference to a query parameter list.}
#'   \item{\code{anyof()}}{This method accesses a reference to a query parameter list.}
#'   \item{\code{filter()}}{This method accesses a reference to a query parameter list.}
#'   \item{\code{getCount()}}{This method returns a count of how many records are discovered by the query.}
#'   \item{\code{getResults()}}{This method returns the records discovered by the query.}
#'   \item{\code{getResultsDataFrame()}}{This method returns the discovered records in a dataframe format.}
#'   \item{\code{getRunDetails()}}{This method returns information the performance of the query.}}
PicSureHpdsQuery <- R6::R6Class("PicSureHpdsQuery",
                                portable = FALSE,
                                lock_objects = FALSE,
                                private = list(),
                                public = list(
                                  initialize = function(connection) {
                                    self$listSelect = HpdsAttribListKeys$new(help_text='')
                                    self$listCrossCounts = HpdsAttribListKeys$new(help_text='')
                                    self$listRequire = HpdsAttribListKeys$new(help_text='')
                                    self$listAnyOf = HpdsAttribListKeys$new(help_text='')
                                    self$listFilter = HpdsAttribListKeyValues$new(help_text='')
                                    self$connection <- connection
                                    self$resourceUUID <- connection$resourceUUID

                                    #                                    if (missing(resource_uuid)) {
                                    #                                      self$resourceUUID <- FALSE
                                    #                                    } else {
                                    #                                      self$resourceUUID <- resource_uuid
                                    #                                    }
                                    self$INTERNAL_API_OBJ <- connection$connection_reference$INTERNAL_api_obj()
                                    self$performance <- c(FALSE, 0, 0, 0, 0)
                                    names(self$performance) <- c("running","tmr_start","tmr_query","tmr_recv","tmr_proc")
                                  },
                                  show = function() {
                                    queryJSON = self$buildQuery("DATAFRAME")
                                    # bugfix for jsonlite
                                    queryJSON <- jsonlite::toJSON(queryJSON, auto_unbox = TRUE)
                                    queryJSON <- str_replace_all(queryJSON, '"numericFilters":\\[\\]','"numericFilters":\\{\\}')
                                    queryJSON <- str_replace_all(queryJSON, '"categoryFilters":\\[\\]','"categoryFilters":\\{\\}')
                                    print(jsonlite::prettify(queryJSON))
                                  },
                                  select = function() {
                                    return(self$listSelect)
                                  },
                                  crosscounts = function() {
                                    return(self$listCrossCounts)
                                  },
                                  require = function() {
                                    return(self$listRequire)
                                  },
                                  anyof = function() {
                                    return(self$listAnyOf)
                                  },
                                  filter = function() {
                                    return(self$listFilter)
                                  },
                                  getCount = function(asAsync = FALSE, timeout=30) {
                                    self$performance['running'] <- TRUE
                                    self$performance['tmr_start'] <- Sys.time()
                                    queryJSON = self$buildQuery("COUNT")
                                    # bugfix for jsonlite
                                    queryJSON <- jsonlite::toJSON(queryJSON, auto_unbox = TRUE)
                                    queryJSON <- str_replace_all(queryJSON, '"numericFilters":\\[\\]','"numericFilters":\\{\\}')
                                    queryJSON <- str_replace_all(queryJSON, '"categoryFilters":\\[\\]','"categoryFilters":\\{\\}')
                                    self$performance['tmr_query'] <- Sys.time()
                                    httpResults = self$INTERNAL_API_OBJ$synchQuery(self$resourceUUID, queryJSON)
                                    self$performance['tmr_recv'] <- Sys.time()
                                    ret = as.integer(httpResults)
                                    self$performance['tmr_proc'] <- Sys.time()
                                    self$performance['running'] <- FALSE
                                    return(ret)
                                  },
                                  getResults = function(asAsync = FALSE, timeout=30) {
                                    self$performance['running'] <- TRUE
                                    self$performance['tmr_start'] <- Sys.time()
                                    queryJSON = self$buildQuery("DATAFRAME")
                                    # bugfix for jsonlite
                                    queryJSON <- jsonlite::toJSON(queryJSON, auto_unbox = TRUE)
                                    queryJSON <- str_replace_all(queryJSON, '"numericFilters":\\[\\]','"numericFilters":\\{\\}')
                                    queryJSON <- str_replace_all(queryJSON, '"categoryFilters":\\[\\]','"categoryFilters":\\{\\}')
                                    self$performance['tmr_query'] <- Sys.time()
                                    httpResults = self$INTERNAL_API_OBJ$synchQuery(self$resourceUUID, queryJSON)
                                    self$performance['tmr_recv'] <- Sys.time()
                                    ret = read.csv(text=httpResults)
                                    self$performance['tmr_proc'] <- Sys.time()
                                    self$performance['running'] <- FALSE
                                    return(ret)
                                  },
                                  getResultsDataFrame = function(asAsync = FALSE, timeout=30) {
                                    self$performance['running'] <- TRUE
                                    self$performance['tmr_start'] <- Sys.time()
                                    queryJSON = self$buildQuery("DATAFRAME")
                                    # bugfix for jsonlite
                                    queryJSON <- jsonlite::toJSON(queryJSON, auto_unbox = TRUE)
                                    queryJSON <- str_replace_all(queryJSON, '"numericFilters":\\[\\]','"numericFilters":\\{\\}')
                                    queryJSON <- str_replace_all(queryJSON, '"categoryFilters":\\[\\]','"categoryFilters":\\{\\}')
                                    self$performance['tmr_query'] <- Sys.time()
                                    httpResults = self$INTERNAL_API_OBJ$synchQuery(self$resourceUUID, queryJSON)
                                    self$performance['tmr_recv'] <- Sys.time()
                                    ret = read.csv(text=httpResults)
                                    self$performance['tmr_proc'] <- Sys.time()
                                    self$performance['running'] <- FALSE
                                    return(ret)
                                  },
                                  getRunDetails = function() {
                                    print('This function returns None or details about the last run of the query')
                                    if (self$performance['tmr_start'] > 0) {
                                      if (self$performance['running'] == TRUE) {
                                        print('Query is RUNNING...')
                                      } else {
                                        print('Query is FINISHED...')
                                      }
                                      if (self$performance['tmr_query'] < self$performance['tmr_start']) {
                                        print("   Query Build: --- ms")
                                        print(" Query Execute: --- ms")
                                        print("Process Result: --- ms")
                                      } else {
                                        t = str((self$performance['tmr_query'] - self$performance['tmr_start'])*1000)
                                        print(paste("   Query Build: ", t, " ms", sep=""))
                                        if (self$performance['tmr_recv'] < self$performance['tmr_query']) {
                                          print(" Query Execute: --- ms")
                                          print("Process Result: --- ms")
                                        } else {
                                          t = str((self$performance['tmr_recv'] - self$performance['tmr_query'])*1000)
                                          print(paste(" Query Execute: ", t, " ms", sep=""))
                                          if (self$performance['tmr_proc'] < self$performance['tmr_recv']) {
                                            print("Process Result: --- ms")
                                          } else {
                                            t = str((self$performance['tmr_proc'] - self$performance['tmr_recv'])*1000)
                                            print(paste("Process Result: ", t, " ms", sep=''))
                                            t = str((self$performance['tmr_proc'] - self$performance['tmr_start'])*1000)
                                            print(paste("____Total Time: ", t, " ms", sep=''))
                                          }
                                        }
                                      }
                                    }
                                  },
                                  getQueryCommand = function() {},
                                  buildQuery = function(resultType="COUNT") {
                                    ret <- jsonlite::fromJSON('{
                                                              "query": {
                                                              "fields":[],
                                                              "requiredFields":[],
                                                              "numericFilters":{},
                                                              "categoryFilters":{}
                                                              }
                                    }')
                                    ret$query$fields <- self$listSelect$getQueryValues()
                                    ret$query$requiredFields <- self$listRequire$getQueryValues()
                                    temp <- self$listFilter$getQueryValues()
                                    ret$query$numericFilters <- temp$numericFilters
                                    ret$query$categoryFilters <- temp$categoryFilters
                                    if (self$resourceUUID != FALSE) {
                                      ret[['resourceUUID']] <- self$resourceUUID
                                    }
                                    ret$query[['expectedResultType']] <- resultType
                                    return(ret)
                                  }
                                )
)


#' R6 class used as base class for all query parameter lists - DO NOT CREATE THIS OBJECT DIRECTLY!
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @import hash
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
#' @format \code{\link{HpdsAttribList}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
#'   \item{\code{new(connection)}}{This method is used to create new object of this class. DO NOT CREATE THIS OBJECT DIRECTLY!}
#'
#'   \item{\code{add()}}{This method adds one or more entries to the query parameter list.}
#'   \item{\code{delete(keys)}}{This method deletes one or more entries from the query parameter list.}
#'   \item{\code{clear()}}{This method clears all entries from the query parameter list.}
#'   \item{\code{show()}}{This method displays the entries of the query parameter list.}
#'   \item{\code{getQueryValues()}}{This is an internally used method that returns the entries for use by the parent query object.}}
HpdsAttribList <- R6::R6Class("HpdsAttribList",
                              portable = FALSE,
                              lock_objects = FALSE,
                              public = list(
                                initialize = function(inst_list=FALSE, help_text=FALSE) {
                                  self$helpstr <- ""
                                  self$data <- hash()
                                  if (help_text != FALSE) {
                                    self$helpstr <- help_text
                                  }
                                },
                                add = function(keys=FALSE, ...) {
                                  args = list(...)
                                  if (typeof(keys) == "logical") {
                                    if (keys == FALSE) {
                                      cat("ERROR: No key specified!")
                                      return(FALSE)
                                    }
                                  }
                                  if (typeof(keys) != "list") {
                                    keys <- list(keys)
                                  }
                                  for (key in keys) {
                                    if (has.key(key, self$data) == TRUE) {
                                      print('ERROR: cannot add, key already exists')
                                      print(key)
                                    } else {
                                      #setting the key depending on input arguments
                                      if (length(args) == 0) {
                                        entry <- list()
                                        entry["type"] <- "exists"
                                        .set(self$data, key, entry)
                                      } else {
                                        if (length(args) == 1 & typeof(args[[1]]) == "list") {
                                          # handle categorical filter
                                          entry <- list()
                                          entry["type"] <- "categorical"
                                          entry["values"] <- args[1]
                                        } else {
                                          if (length(args) == 1 & (is.null(args[["min"]]) & is.null(args[["max"]]))) {
                                            # handle single value
                                            entry <- list()
                                            entry["type"] <- "value"
                                            entry["value"] <- args[[1]]
                                          } else {
                                            # handle minmax
                                            entry<- list()
                                            entry["type"] <- "minmax"
                                            if (!is.null(args[["min"]])) {
                                              entry["min"] <- args[["min"]]
                                            }
                                            if (!is.null(args[["max"]])) {
                                              entry["max"] <- args[["max"]]
                                            }
                                            # handle unnamed value(s)
                                            if (is.null(names(args[[1]])) & typeof(args[[1]]) == "double") {
                                              entry["min"] <- args[[1]]
                                            }
                                            if (length(args) > 1) {
                                              if (is.null(names(args[[2]])) & typeof(args[[2]]) == "double") {
                                                entry["max"] <- args[[2]]
                                              }
                                            }
                                          }
                                        }
                                      }
                                      .set(self$data, key, entry)
                                    }
                                  }
                                  invisible(self)
                                },
                                delete = function(keys, ...) {
                                  args <- list(...)
                                  if (typeof(keys) != "list") {
                                    keys <- list(keys)
                                  }
                                  for (key in keys) {
                                    if (has.key(key, self$data) == FALSE) {
                                      print('ERROR: the specified key does not exist')
                                    } else {
                                      # TODO: implement deleting a single category value from a key
                                      if (length(args) == 0) {
                                        del(key, self$data)
                                      } else {
                                        temp <- get(key, self$data)
                                        if (temp$type == "categorical") {
                                          for (x in seq(length(temp$values), 1)) {
                                            if (temp$values[[x]] == args[[1]]) {
                                              temp$values[[x]] <- NULL
                                            }
                                          }
                                          # replace the data in the hash array
                                          del(key, self$data)
                                          .set(self$data, key, temp)
                                        }
                                      }
                                    }
                                  }
                                  invisible(self)
                                },
                                show = function() {
                                  return(jsonlite::prettify(jsonlite::toJSON(self$getQueryValues(), auto_unbox = TRUE)))
                                },
                                clear = function() {
                                  self$data <- hash()
                                  invisible(self)
                                },
                                getQueryValues = function() {
                                  return(jsonlite::toJSON(self$data, auto_unbox = TRUE))
                                }
                              )
)


#' R6 class used to store/manipulate key-only query parameter lists - DO NOT CREATE THIS OBJECT DIRECTLY!
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @import hash
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
#' @format \code{\link{HpdsAttribListKeys}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
#'   \item{\code{new(inst_list, help_text)}}{This method is used to create new object of this class. DO NOT CREATE THIS OBJECT DIRECTLY!}
#'
#'   \item{\code{add()}}{This method adds one or more entries to the query parameter list.}
#'   \item{\code{delete(keys)}}{This method deletes one or more entries from the query parameter list.}
#'   \item{\code{clear()}}{This method clears all entries from the query parameter list.}
#'   \item{\code{show()}}{This method displays the entries of the query parameter list.}
#'   \item{\code{getQueryValues()}}{This is an internally used method that returns the entries for use by the parent query object.}}
HpdsAttribListKeys <- R6::R6Class("HpdsAttribListKeys",
                                  portable = FALSE,
                                  lock_objects = FALSE,
                                  inherit = HpdsAttribList,
                                  private = list(
                                  ),
                                  public = list(
                                    initialize = function(inst_list=FALSE, help_text=FALSE) {
                                      self$data <- hash()
                                      if (help_text != FALSE) {
                                        super$initialize(help_text=help_text)
                                      }
                                    },
                                    add = function(key = FALSE, ...) {
                                      #setting the key as exists filter
                                      super$add(key)
                                      invisible(self)
                                    },
                                    delete = function(key = FALSE, ...) {
                                      super$delete(key)
                                      invisible(self)
                                    },
                                    getQueryValues = function() {
                                      data <- as.list(self$data)
                                      ret <- list()
                                      for (key in names(data)) {
                                        if (data[[key]]$type == "exists") {
                                          l <- length(ret) + 1
                                          ret[[l]] <- key
                                        }
                                      }
                                      return(ret)
                                    }
                                  )
)

#' R6 class used to store/manipulate Key+Value query parameter lists - DO NOT CREATE THIS OBJECT DIRECTLY!
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import jsonlite
#' @import hash
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} used to access a HPDS-hosted resource's data dictionary.
#' @format \code{\link{HpdsAttribListKeyValues}} object.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://github.com/hms-dbmi/pic-sure-r-adapter-hpds}
#'   \item{\code{new(inst_list, help_text)}}{This method is used to create new object of this class. DO NOT CREATE THIS OBJECT DIRECTLY!}
#'
#'   \item{\code{add()}}{This method adds one or more entries to the query parameter list.}
#'   \item{\code{delete(keys)}}{This method deletes one or more entries from the query parameter list.}
#'   \item{\code{clear()}}{This method clears all entries from the query parameter list.}
#'   \item{\code{show()}}{This method displays the entries of the query parameter list.}
#'   \item{\code{getQueryValues()}}{This is an internally used method that returns the entries for use by the parent query object.}}
HpdsAttribListKeyValues <- R6::R6Class("HpdsAttribListKeyValues",
                                       portable = FALSE,
                                       lock_objects = FALSE,
                                       inherit = HpdsAttribList,
                                       private = list(),
                                       public = list(
                                         initialize = function(inst_list=FALSE, help_text=FALSE) {
                                           self$data <- hash()
                                           if (help_text != FALSE) {
                                             super$initialize(help_text=help_text)
                                           }
                                         },
                                         add = function(key=FALSE, ...) {
                                           #setting the key as exists filter
                                           super$add(key, ...)
                                           invisible(self)
                                         },
                                         delete = function(key=FALSE, ...) {
                                           super$delete(key, ...)
                                           invisible(self)
                                         },
                                         getQueryValues = function() {
                                           data <- as.list(self$data)
                                           ret <- list(numericFilters=list(), categoryFilters=list())
                                           for (key in names(data)) {
                                             rec <- data[[key]]
                                             if (rec$type == "minmax") {
                                               t = list()
                                               if (!is.null(rec$min)) {
                                                 t$min <- rec$min
                                               }
                                               if (!is.null(rec$max)) {
                                                 t$max <- rec$max
                                               }
                                               ret$numericFilters[[key]] <- t
                                             } else if (rec$type == "categorical") {
                                               ret$categoryFilters[[key]] <- rec$values
                                             } else if (rec$type == "value") {
                                               if (typeof(rec$value) == "character") {
                                                 ret$categoryFilters[[key]] <- list(rec$value)
                                               } else {
                                                 ret$numericFilters[[key]] <- rec$value
                                               }
                                             }
                                           }
                                           return(ret)
                                         }
                                       )
)
