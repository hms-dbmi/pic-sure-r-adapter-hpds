# PIC-SURE HPDS Dictonary(ish) Objects
library(jsonlite)

#' export
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
                                          },
                                          help = function() {
                                            cat("        [HELP] PicSureHpdsLib::Adapter$new(connection)$useResource(uuid)$dictionary()\n")
                                            cat("            $find()                 Lists all data dictionary entries\n")
                                            cat("            $find(search_string)    Lists matching data dictionary entries\n")
                                          }
                                        )
)


#' export
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
                                       help = function() {
                                         cat("        [HELP] PicSureHpdsLib$Adapter(connection)$useResource(uuid)$dictionary()$find(term)\n")
                                         cat("            $count()        Returns the number of entries in the dictionary that match the given term\n")
                                         cat("            $keys()         Return the keys of the matching entries\n")
                                         cat("            $entries()      Return a list of matching dictionary entries\n")
                                         cat("            $DataFrame()    Return the entries in a DataFrame-compatible format\n\n")
                                         cat("        [Examples]\n")
                                         cat("            results = PicSureHpdsLib$Adapter(connection)$useResource(uuid)$dictionary()$find('asthma')\n")
                                         cat("            df = results$DataFrame()\n")
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
