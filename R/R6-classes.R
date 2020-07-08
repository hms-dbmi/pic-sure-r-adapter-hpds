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
                                                 self$profile_info = jsonlite::fromJSON("{}")
                                                 if (missing(resource_uuid)) {
                                                   if (length(self$connection_reference$self$resource_uuids) > 1) {
                                                     print(self$connection_reference$self$resource_uuids)
                                                     stop("ERROR: You must specify a valid Resource UUID")
                                                   } else {
                                                     self$resourceUUID <- self$connection_reference$resource_uuids[[1]]
                                                   }
                                                 } else {
                                                   if (resource_uuid %in% self$connection_reference$resource_uuids) {
                                                     self$resourceUUID <- resource_uuid
                                                   } else {
                                                     stop("ERROR: You must specify a valid Resource UUID")
                                                   }
                                                 }

                                                 # cache the profile information on startup
                                                 api = connection$INTERNAL_api_obj()
                                                 self$profile_info = jsonlite::fromJSON(api$profile())

                                               },
                                               version = function() {
                                                 cat(paste("PicSureHpdsLib Library (version ", packageVersion("PicSureHpdsLib"), ")\n", sep=""))
                                                 cat(paste("URL: ", self$connection_reference$url, "\n", sep=""))
                                                 invisible(self)
                                               },
                                               dictionary = function() {
                                                 return(PicSureHpdsDictionary$new(self))
                                               },
                                               showConsents = function() {
                                                 template = jsonlite::fromJSON(self$profile_info$queryTemplate);
                                                 consent_filters = template$categoryFilters$`\\_Consents\\Short Study Accession with Consent Code\\`
                                                 return(consent_filters);
                                               },
                                               setConsents = function(consents = c()){
                                                 template = list()
                                                 template$categoryFilters = list();
                                                 template$categoryFilters$`\\_Consents\\Short Study Accession with Consent Code\\` = consents;
                                                 self$profile_info$queryTemplate = jsonlite::toJSON(template);
                                               },
                                               query = function(loadQuery=NA) {
                                                 if (is.na(loadQuery)) {
                                                   return(PicSureHpdsQuery$new(self))
                                                 } else {
                                                   return(PicSureHpdsQuery$new(self, loadQuery=loadQuery))
                                                 }
                                               },
                                               retrieveQueryResults = function(query_uuid = NA) {
                                                 query_text = paste('{"resourceUUID":"', self$resourceUUID, '", "resourceCredentials":{}, "query":', self$profile_info$queryTemplate ,' }', sep="")
                                                 api = self$connection_reference$INTERNAL_api_obj()
                                                 repeat {
                                                   status = jsonlite::fromJSON(api$queryStatus(resource_uuid=self$resourceUUID, query_uuid=query_uuid, query_body=query_text))
                                                   if (status$status == "AVAILABLE") {
                                                     break
                                                   } else {
                                                     if (status$status == "ERROR") {
                                                       print("An error occured retrieving this query! For more information please check the server logs.")
                                                       return(NA)
                                                     } else {
                                                       Sys.sleep(1)
                                                     }
                                                   }
                                                 }
                                                 return(api$queryResult(resource_uuid = self$resourceUUID, query_uuid = query_uuid))
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
#                             inherit = picsure::Adapter,
                             public = list(
                               initialize = function(url_arg, token_arg = FALSE) {
                                 # trim and make sure URL ends in "/"
                                 endpoint <- str_trim(url_arg)
                                 if (str_detect(endpoint, "/$") == FALSE) {
                                   endpoint <- paste(endpoint, "/", sep="")
                                 }
                                 self$url <- endpoint
                                 self$token <- token_arg
                                 self$connection_reference <- hpds::PicSureHpdsBypassConnection$new(self$url, self$token)
                               },
                               useResource = function(resource_uuid) {
                                 if (missing(resource_uuid)) {
                                   if (length(self$connection_reference$self$resource_uuids) > 1) {
                                     print(self$connection_reference$self$resource_uuids)
                                   } else {
                                     temp <- hpds::PicSureHpdsResourceConnection$new(self$connection_reference, FALSE)
                                   }
                                 } else {
                                   if (resource_uuid %in% self$connection_reference$self$resource_uuids) {
                                     temp <- hpds::PicSureHpdsResourceConnection$new(self$connection_reference, resource_uuid)
                                   } else {
                                     stop("ERROR: You must specify a valid Resource UUID")
                                   }
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
                                                 results = content(request, "parsed", encoding = "UTF-8")
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
                                                  if (isFALSE(query)) {
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
                                                    return(content(request, "text", encoding = "UTF-8"))
                                                  }
                                                },
                                                asynchQuery = function(resource_uuid, query) { writeLines(c(resource_uuid, query)) },
                                                synchQuery = function(resource_uuid, query) {
                                                  full_url = paste(self$url, "query/sync/", sep="")
                                                  if (isFALSE(query)) {
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
                                                    return(content(request, "text", encoding = "UTF-8"))
                                                  }
                                                },
                                                queryStatus = function(resource_uuid, query_uuid) { writeLines(c(resource_uuid, query_uuid)) },
                                                queryResult = function(resource_uuid, query_uuid) { writeLines(c(resource_uuid, query_uuid)) }
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
                                       find = function(term=FALSE, showAll=FALSE) {
                                         query <- list()
                                         if (isFALSE(term)) {
                                           query$query <- ""
                                         } else {
                                           query$query <- toString(term)
                                         }
                                         results = self$INTERNAL_API_OBJ$search(self$resourceUUID, jsonlite::toJSON(query, auto_unbox=TRUE))
                                         # filter to query scope if needed
                                         if (showAll != FALSE) {
                                           return(PicSureHpdsDictionaryResult$new(results))
                                         } else {
                                           if (exists("self$connection$profile_info$queryScopes")) {
                                             return(PicSureHpdsDictionaryResult$new(results, self$connection$profile_info$queryScopes))
                                           } else {
                                             return(PicSureHpdsDictionaryResult$new(results))
                                           }
                                         }
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
                                             initialize = function(results, filter.list = FALSE) {
                                               self$results <- jsonlite::fromJSON(results)
                                               updated_list <- list()
                                               if (!is.null(self$results$results$phenotypes)) {
                                                 new_results <- list()
                                                 # dictionary results are segmented (phenotype, info, etc)
                                                 for (idx1 in 1:length(self$results$results)) {
                                                   result_type = names(self$results$results[idx1])
                                                   if (length(self$results$results[[idx1]]) > 0) {
                                                     for (idx2 in 1:length(self$results$results[[idx1]])) {
                                                       self$results$results[[idx1]][[idx2]]$HpdsDataType <- result_type
                                                       idx3 <- names(self$results$results[[idx1]][idx2])[[1]]
                                                       if (filter.list != FALSE && length(filter.list) > 0) {
                                                         for (matchidx in 1:length(filter.list)) {
                                                           pos = regexpr(idx3, filter.list[[matchidx]])
                                                           if (pos > -1 && pos < 3) {
                                                             updated_list[[idx3]] <- self$results$results[[idx1]][[idx2]]
                                                             break
                                                           }
                                                         }
                                                       } else {
                                                         updated_list[[idx3]] <- self$results$results[[idx1]][[idx2]]
                                                       }
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
                                               df <- data.frame()
                                               #get a list of all vector names
                                               vn <- list()
                                               if (length(self$results[['results']]) > 0) {
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
                                                 # make sure we have a column called "name"
                                                 if (!"name" %in% vn) {
                                                   vn[["name"]] <- 1
                                                 }
                                                 vn <- names(vn)
                                                 # we genrate a list of all attributes of all records, then pivot into vectors
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
                                                       # unless the missing record is "name"
                                                       if (vn[idx2] == "name") {
                                                         df[idx1,idx2] <- names(self$results[['results']])[[idx1]]
                                                       } else {
                                                         df[idx1,idx2] <- NA
                                                       }
                                                     } else {
                                                       # attribute is set for this record, save
                                                       if (typeof(e[[vn[idx2]]]) == "list" || length(e[[vn[idx2]]]) > 1) {
                                                         df[idx1,idx2] <- paste(e[[vn[idx2]]], collapse=",")
                                                       } else {
                                                         if (is.null(e[[vn[idx2]]])) {
                                                           # Set to NA unless the missing record is "name"
                                                           if (vn[idx2] == "name") {
                                                             df[idx1,idx2] <- names(self$results[['results']])[[idx1]]
                                                           } else {
                                                             df[idx1,idx2] <- NA
                                                           }
                                                         } else {
                                                           df[idx1,idx2] <- e[[vn[idx2]]]
                                                         }
                                                       }
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
                                    self$connection <- connection
                                    self$resourceUUID <- connection$resourceUUID
                                    self$INTERNAL_API_OBJ <- connection$connection_reference$INTERNAL_api_obj()

                                    self$listSelect = HpdsAttribListKeys$new(help_text='',
                                                                             resource_uuid=self$resourceUUID,
                                                                             api_obj=self$INTERNAL_API_OBJ,
                                                                             allow_variants=FALSE)
                                    self$listCrossCounts = HpdsAttribListKeys$new(help_text='',
                                                                                  resource_uuid=self$resourceUUID,
                                                                                  api_obj=self$INTERNAL_API_OBJ)
                                    self$listRequire = HpdsAttribListKeys$new(help_text='',
                                                                              resource_uuid=self$resourceUUID,
                                                                              api_obj=self$INTERNAL_API_OBJ)
                                    self$listAnyOf = HpdsAttribListKeys$new(help_text='',
                                                                            resource_uuid=self$resourceUUID,
                                                                            api_obj=self$INTERNAL_API_OBJ)
                                    self$listFilter = HpdsAttribListKeyValues$new(help_text='',
                                                                                  resource_uuid=self$resourceUUID,
                                                                                  api_obj=self$INTERNAL_API_OBJ)
                                    self$performance <- c(FALSE, 0, 0, 0, 0)
                                    names(self$performance) <- c("running","tmr_start","tmr_query","tmr_recv","tmr_proc")
                                    # load the default queryTemplate values
                                    self$load(self$connection$profile_info$queryTemplate)
                                  },
                                  show = function() {
                                    queryJSON = self$buildQuery("DATAFRAME")
                                    queryJSON = jsonlite::toJSON(queryJSON, auto_unbox = TRUE)
                                    # bugfix for jsonlite !!!! DO NOT REFACTOR BELOW 5 LINES AS R WILL MESS THINGS UP!
                                    queryJSON <- gsub('\\[\\[\\]\\]','\\[\\]', queryJSON)
                                    queryJSON <- gsub('"numericFilters":\\[\\]','"numericFilters":\\{\\}', queryJSON)
                                    queryJSON <- gsub('"categoryFilters":\\[\\]','"categoryFilters":\\{\\}', queryJSON)
                                    queryJSON <- gsub('"categoryVariantInfoFilters":\\[\\]','"categoryVariantInfoFilters":\\{\\}', queryJSON)
                                    queryJSON <- gsub('"numericVariantInfoFilters":\\[\\]','"numericVariantInfoFilters":\\{\\}', queryJSON)
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
                                    queryJSON = jsonlite::toJSON(queryJSON, auto_unbox = TRUE)
                                    # bugfix for jsonlite !!!! DO NOT REFACTOR BELOW 5 LINES AS R WILL MESS THINGS UP!
                                    queryJSON <- gsub('\\[\\[\\]\\]','\\[\\]', queryJSON)
                                    queryJSON <- gsub('"numericFilters":\\[\\]','"numericFilters":\\{\\}', queryJSON)
                                    queryJSON <- gsub('"categoryFilters":\\[\\]','"categoryFilters":\\{\\}', queryJSON)
                                    queryJSON <- gsub('"categoryVariantInfoFilters":\\[\\]','"categoryVariantInfoFilters":\\{\\}', queryJSON)
                                    queryJSON <- gsub('"numericVariantInfoFilters":\\[\\]','"numericVariantInfoFilters":\\{\\}', queryJSON)
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
                                    queryJSON = jsonlite::toJSON(queryJSON, auto_unbox = TRUE)
                                    # bugfix for jsonlite !!!! DO NOT REFACTOR BELOW 5 LINES AS R WILL MESS THINGS UP!
                                    queryJSON <- gsub('\\[\\[\\]\\]','\\[\\]', queryJSON)
                                    queryJSON <- gsub('"numericFilters":\\[\\]','"numericFilters":\\{\\}', queryJSON)
                                    queryJSON <- gsub('"categoryFilters":\\[\\]','"categoryFilters":\\{\\}', queryJSON)
                                    queryJSON <- gsub('"categoryVariantInfoFilters":\\[\\]','"categoryVariantInfoFilters":\\{\\}', queryJSON)
                                    queryJSON <- gsub('"numericVariantInfoFilters":\\[\\]','"numericVariantInfoFilters":\\{\\}', queryJSON)
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
                                    queryJSON = jsonlite::toJSON(queryJSON, auto_unbox = TRUE)
                                    # bugfix for jsonlite !!!! DO NOT REFACTOR BELOW 5 LINES AS R WILL MESS THINGS UP!
                                    queryJSON <- gsub('\\[\\[\\]\\]','\\[\\]', queryJSON)
                                    queryJSON <- gsub('"numericFilters":\\[\\]','"numericFilters":\\{\\}', queryJSON)
                                    queryJSON <- gsub('"categoryFilters":\\[\\]','"categoryFilters":\\{\\}', queryJSON)
                                    queryJSON <- gsub('"categoryVariantInfoFilters":\\[\\]','"categoryVariantInfoFilters":\\{\\}', queryJSON)
                                    queryJSON <- gsub('"numericVariantInfoFilters":\\[\\]','"numericVariantInfoFilters":\\{\\}', queryJSON)
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
                                        t = as.character((self$performance['tmr_query'] - self$performance['tmr_start'])*1000)
                                        print(paste("   Query Build: ", t, " ms", sep=""))
                                        if (self$performance['tmr_recv'] < self$performance['tmr_query']) {
                                          print(" Query Execute: --- ms")
                                          print("Process Result: --- ms")
                                        } else {
                                          t = as.character((self$performance['tmr_recv'] - self$performance['tmr_query'])*1000)
                                          print(paste(" Query Execute: ", t, " ms", sep=""))
                                          if (self$performance['tmr_proc'] < self$performance['tmr_recv']) {
                                            print("Process Result: --- ms")
                                          } else {
                                            t = as.character((self$performance['tmr_proc'] - self$performance['tmr_recv'])*1000)
                                            print(paste("Process Result: ", t, " ms", sep=''))
                                            t = as.character((self$performance['tmr_proc'] - self$performance['tmr_start'])*1000)
                                            print(paste("____Total Time: ", t, " ms", sep=''))
                                          }
                                        }
                                      }
                                    }
                                  },
                                  save = function(resultType="COUNT") {
                                    return(jsonlite::toJSON(self$buildQuery(resultType)))
                                  },
                                  load = function(queryStr="{}", merge=TRUE) {
                                    queryObj = jsonlite::fromJSON(queryStr)

                                    if (isTRUE(any("query" %in% names(queryObj)))) {
                                      load_node = queryObj[["query"]]
                                    } else {
                                      load_node = queryObj
                                    }

                                    # clear  the current criteria if we are not merging
                                    if (merge != TRUE) {
                                      self$listSelect$clear()
                                      self$listCrossCounts$clear()
                                      self$listRequire$clear()
                                      self$listAnyOf$clear()
                                      self$listFilter$clear()
                                    }

                                    # ___ handle key-only fields ___
                                    if (isTRUE(any("fields" %in% names(load_node)))) {
                                      self$listSelect$load(load_node[["fields"]])
                                    }

                                    if (isTRUE(any("crossCountFields" %in% names(load_node)))) {
                                      self$listCrossCounts$load(load_node[["crossCountFields"]])
                                    }

                                    if (isTRUE(any("requiredFields" %in% names(load_node)))) {
                                      self$listRequire$load(load_node[["requiredFields"]])
                                    }

                                    if (isTRUE(any("anyRecordOf" %in% names(load_node)))) {
                                      self$listAnyOf$load(load_node[["anyRecordOf"]])
                                    }


                                    # ___ handle various filters ___
                                    if (isTRUE(any("numericFilters" %in% names(load_node)))) {
                                      filter_numeric = load_node[["numericFilters"]]
                                    } else {
                                      filter_numeric = list()
                                    }
                                    if (isTRUE(any("categoryFilters" %in% names(load_node)))) {
                                      filter_categorical = load_node[["categoryFilters"]]
                                    } else {
                                      filter_categorical = list()
                                    }
                                    if (isTRUE(any("anyRecordOf" %in% names(load_node)))) {
                                      filter_variant = load_node[["variantInfoFilters"]]
                                    } else {
                                      filter_variant = list()
                                    }
                                    self$listFilter$load(
                                      filter_numeric,
                                      filter_categorical,
                                      filter_variant
                                    )
                                    return(self)
                                  },
                                  buildQuery = function(resultType="COUNT") {
                                    ret <- jsonlite::fromJSON('{"query": {
                                                              "fields":[],
                                                              "crossCountFields":[],
                                                              "requiredFields":[],
                                                              "anyRecordOf": [],
                                                              "numericFilters":{},
                                                              "categoryFilters":{},
                                                              "variantInfoFilters": []
                                                              }
                                    }')
                                    ret$query$fields = self$listSelect$getQueryValues()
                                    ret$query$crossCountFields = self$listCrossCounts$getQueryValues()
                                    ret$query$requiredFields = self$listRequire$getQueryValues()
                                    ret$query$anyRecordOf = self$listAnyOf$getQueryValues()
                                    temp = self$listFilter$getQueryValues()
                                    ret$query$numericFilters = temp$numericFilters
                                    ret$query$categoryFilters = temp$categoryFilters
                                    # Hack to make jsonlite work correctly for variant info filters
                                    ret$query$variantInfoFilters = list(temp$variantInfoFilters)

                                    if (!(isFALSE(self$resourceUUID))) {
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
#' @import stringr
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
                                initialize = function(inst_list=FALSE, help_text="", allow_variants=TRUE, resource_uuid=FALSE, api_obj=FALSE) {
                                  self$helpstr <- ""
                                  if (!(isFALSE(help_text))) {
                                    self$helpstr <- help_text
                                  }
                                  if (isFALSE(inst_list)) {
                                    self$data <- hash()
                                  } else {
                                    self$data <- inst_list
                                  }
                                  self$variants_enabled <- isTRUE(allow_variants)
                                  self$resource_uuid <- resource_uuid
                                  self$api_obj <- api_obj
                                  self$dictionary_cache = NA
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

                                  new_keys = list()
                                  for (key in keys) {
                                    if (has.key(key, self$data) == TRUE) {
                                      # filter out keys that already exist
                                      print(paste('ERROR: cannot add, key already exists', key, sep=": "))
                                    } else {
                                      # do not lookup the key if it is in VariantSpec format
                                      if (self$is_VariantSpec(key)) {
                                        # add variant spec entry
                                        if (self$variants_enabled == TRUE) {
                                          entry <- list()
                                          entry["type"] <- "categorical"
                                          entry["HpdsDataType"] <- "HpdsVariantSpec"
                                          # handle categorical filter
                                          if (typeof(args[[1]]) != "list") {
                                            entry["values"] <- list(args[[1]])
                                          } else {
                                            entry["values"] <- args[1]
                                          }
                                          .set(self$data, key, entry)
                                        } else {
                                          # variant spec is not allowed
                                          print(paste('ERROR: cannot add key, it is of type HpdsVariantSpec', key, sep=": "))
                                        }
                                      } else {
                                        # add to the list of keys to lookup
                                        new_keys = c(new_keys, key)
                                      }
                                    }
                                  }


                                  # TODO: Lookup keys and only add keys that have matches in the data dictionary
                                  keys = new_keys
                                  for (key in keys) {
                                    add_key = FALSE
                                    variant_key = FALSE
                                    key_typename = "NA"
                                    is_categorical = FALSE
                                    valid_categories = list()
                                    if (self$is_VariantSpec(key)) {
                                      # add variant spec entry
                                      variant_key = TRUE
                                      key_typename = "HpdsVariantSpec"
                                      if (self$variants_enabled == TRUE) {
                                        add_key = TRUE
                                      } else {
                                        # variant spec is not allowed
                                        print(paste('ERROR: cannot add key, it is of type HpdsVariantSpec', key, sep=": "))
                                      }
                                    } else {
                                      add_key = TRUE
                                    }

                                    # has the dictionary already been cached?
                                    if (length(self$dictionary_cache) == 1 && is.na(self$dictionary_cache)) {
                                      # pull down the full dictionary and cache it
                                      query <- {}
                                      query$query <- ""
                                      results <- self$api_obj$search(resource_uuid=self$resource_uuid, jsonlite::toJSON(query, auto_unbox=TRUE))
                                      self$dictionary_cache <- jsonlite::fromJSON(results)
                                      if (!is.null(self$dictionary_cache$error)) {
                                        self$dictionary_cache$results = list()
                                      print("ERROR: Could not cache data dictionary")
                                      }
                                    }

                                    add_key = FALSE
                                    # loop though the result types
                                    for (typename in names(self$dictionary_cache$results)) {
                                      if (!is.null(self$dictionary_cache$results[[typename]][[key]])) {
                                        # the key exists in the data dictionary, insert it
                                        add_key = TRUE
                                        key_typename = typename
                                        # save categorical info
                                        if (is.null(self$dictionary_cache$results[[typename]][[key]]$categorical)) {
                                          is_categorical = !self$dictionary_cache$results[[typename]][[key]]$continuous
                                        } else {
                                          is_categorical = self$dictionary_cache$results[[typename]][[key]]$categorical
                                        }
                                        if (is.null(self$dictionary_cache$results[[typename]][[key]]$categoryValues)) {
                                          valid_categories = self$dictionary_cache$results[[typename]][[key]]$values
                                        } else {
                                          valid_categories = self$dictionary_cache$results[[typename]][[key]]$categoryValues
                                        }
                                        break
                                      }
                                    }

                                    # add the key if it was found in the data dictionary
                                    if (isFALSE(add_key)) {
                                      print(paste('ERROR: cannot add, key does not exist in resource', key, sep=": "))
                                    } else {
                                      # add key depending on input arguments
                                      if (length(args) == 0) {
                                        entry <- list()
                                        entry["type"] <- "exists"
                                        .set(self$data, key, entry)
                                      } else {
                                        if (isTRUE(is_categorical)) {
                                          # handle categorical filter
                                          if (typeof(args[[1]]) != "list") {
                                            args[[1]] = list(args[[1]])
                                          }
                                          # check that all passed values are valid
                                          for (catval in args[[1]]) {
                                            if (!(catval %in% valid_categories)) {
                                              print('ERROR: cannot add, invalid category specified for key', key, sep=": ")
                                              add_key = FALSE
                                              break
                                            }
                                          }
                                          # add entry if passed above check
                                          if (isTRUE(add_key)) {
                                            entry <- list()
                                            entry["type"] <- "categorical"
                                            entry["values"] <- args[1]
                                            add_key = FALSE
                                          }
                                        } else {
                                          # see if user specified a categorical list on non-categorical key
                                          if (length(args) == 1 & typeof(args[[1]]) == "list") {
                                            print('ERROR: cannot add key, it does not take categorical values', key, sep=": ")
                                            add_key = FALSE
                                          }
                                        }
                                        if (isTRUE(add_key)) {
                                          if (length(args) == 1 & (is.null(args[["min"]]) & is.null(args[["max"]]))) {
                                            # handle single value
                                            entry <- list()
                                            entry["type"] <- "value"
                                            entry["value"] <- args[[1]]
                                          } else {
                                            # handle minmax
                                            if (key_typename == "HpdsVariantSpec") {
                                              print(paste('ERROR: cannot add key, HpdsVariantSpec cannot filter a range', key, sep=": "))
                                            } else {
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
                                      }
                                      entry["HpdsDataType"] = key_typename
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
                                  return(self$data)
                                },
                                normalize_VariantSpec = function(teststr) {
                                  return(paste(str_split(teststr,"[:_/]"), sep=","))
                                },
                                is_VariantSpec = function(teststr) {
                                  norm_str = self$normalize_VariantSpec(teststr)
                                  # is the string of a variant spec type
                                  is_variant = FALSE
                                  if (str_detect(norm_str, 'rs[0-9]+$')) {
                                    is_variant = TRUE
                                  }
                                  if (str_detect(norm_str, '[0-9]+,[0-9\\.]+,.*')) {
                                    is_variant = TRUE
                                  }
                                  return(is_variant)
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
                                    initialize = function(inst_list=FALSE, help_text="", allow_variants=TRUE, resource_uuid=FALSE, api_obj=FALSE) {
                                      super$initialize(inst_list=inst_list, help_text=help_text, allow_variants=allow_variants, resource_uuid=resource_uuid, api_obj=api_obj)
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
                                        if (data[[key]]$type == "exists" || data[[key]]$type == "HpdsVariantSpec") {
                                          l <- length(ret) + 1
                                          ret[[l]] <- key
                                        }
                                      }
                                      return(ret)
                                    },
                                    load = function(keys) {
                                      for (key in keys) {
                                        entry <- list()
                                        entry["type"] = "exists"
                                        .set(self$data, key, entry)
                                      }
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
                                         initialize = function(inst_list=FALSE, help_text="", allow_variants=TRUE, resource_uuid=FALSE, api_obj=FALSE) {
                                           super$initialize(inst_list=inst_list, help_text=help_text, allow_variants=allow_variants, resource_uuid=resource_uuid, api_obj=api_obj)
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
                                         load = function(numericFilters=list(), categoryFilters=list(), variantInfoFilters=list()) {
                                           for (key in names(numericFilters)) {
                                             rec = list()
                                             rec["type"] = "minmax"
                                             if (!is.null(numericFilters[[key]]$min)) {
                                               rec["min"] <- numericFilters[[key]]$min
                                             }
                                             if (!is.null(numericFilters[[key]]$max)) {
                                               rec["max"] <- numericFilters[[key]]$max
                                             }
                                             rec["HpdsDataType"] = "unknown"
                                             .set(self$data, key, rec)
                                           }

                                           for (key in names(categoryFilters)) {
                                             rec = list()
                                             rec["type"] = "categorical"
                                             rec["values"] = list(categoryFilters[[key]])
                                             rec$HpdsDataType = "unknown"
                                             .set(self$data, key, rec)

                                           }

                                           for (key in names(variantInfoFilters$categoryVariantInfoFilters)) {
                                             rec = list()
                                             rec$HpdsDataType = "info"
                                             rec$type = "categorical"
                                             rec$values = as.list(variantInfoFilters$categoryVariantInfoFilters[[key]]$values)
                                             rec$HpdsDataType = "unknown"
                                             .set(self$data, key, rec)
                                           }


                                           for (key in names(variantInfoFilters$numericVariantInfoFilters)) {
                                             rec = list()
                                             rec$HpdsDataType = "info"
                                             rec$type = "minmax"
                                             if (!is.null(variantInfoFilters$numericVariantInfoFilters[[key]]$min)) {
                                               rec$min <- variantInfoFilters$numericVariantInfoFilters[[key]]$min
                                             }
                                             if (!is.null(variantInfoFilters$numericVariantInfoFilters[[key]]$max)) {
                                               rec$max <- variantInfoFilters$numericVariantInfoFilters[[key]]$max
                                             }
                                             .set(self$data, key, rec)
                                           }

                                         },
                                         getQueryValues = function() {
                                           ret <- list(numericFilters=list(), categoryFilters=list(), variantInfoFilters=list())
                                           ret_variant_numeric = list()
                                           ret_variant_category = list()
                                           for (key in names(self$data)) {
                                             rec <- self$data[[key]]
                                             if (rec$type == "minmax") {
                                               t = list()
                                               if (!is.null(rec$min)) {
                                                 t$min <- rec$min
                                               }
                                               if (!is.null(rec$max)) {
                                                 t$max <- rec$max
                                               }
                                               if (rec$HpdsDataType == "info") {
                                                 ret_variant_numeric[[key]] <- t
                                               } else {
                                                 ret$numericFilters[[key]] <- t
                                               }
                                             } else if (rec$type == "categorical" || rec$type == "HpdsVariantSpec") {
                                               if (rec$HpdsDataType == "info") {
                                                 ret_variant_category[[key]] <- as.list(rec$values)
                                               } else {
                                                 ret$categoryFilters[[key]] <- as.list(rec$values)
                                               }
                                             } else if (rec$type == "value") {
                                               if (typeof(rec$value) == "character") {
                                                 if (rec$HpdsDataType == "info") {
                                                   ret_variant_category[[key]] <- list(rec$value)
                                                 } else {
                                                   ret$categoryFilters[[key]] <- list(rec$value)
                                                 }
                                               } else {
                                                 t = list()
                                                 t$min <- rec$value
                                                 t$max <- rec$value
                                                 if (rec$HpdsDataType == "info") {
                                                   ret_variant_numeric[[key]] <- t
                                                 } else {
                                                   ret$numericFilters[[key]] <- t
                                                 }
                                               }
                                             }
                                           }
                                           # add any variant info filters
                                           ret$variantInfoFilters = c(ret$variantInfoFilters, list(categoryVariantInfoFilters=ret_variant_category, numericVariantInfoFilters=ret_variant_numeric))

                                           return(ret)
                                         }
                                       )
)
