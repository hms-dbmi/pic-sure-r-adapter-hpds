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
                                            cat("
        [HELP] PicSureHpdsLib::Adapter$new(connection)$useResource(uuid)$dictionary()
            $find()                 Lists all data dictionary entries
            $find(search_string)    Lists matching data dictionary entries
                                            ")
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
                                       },
                                       help = function() {
                                         cat("
        [HELP] PicSureHpdsLib$Adapter(connection)$useResource(uuid)$dictionary()$find(term)
            $count()        Returns the number of entries in the dictionary that match the given term
            $keys()         Return the keys of the matching entries
            $entries()      Return a list of matching dictionary entries
            $DataFrame()    Return the entries in a DataFrame-compatible format
             
        [Examples]
            results = PicSureHpdsLib$Adapter(connection)$useResource(uuid)$dictionary()$find('asthma')
            df = results$DataFrame()
        ")
                                       },
                                       count = function() {
                                         return(length(self$results))
                                       },
                                       keys = function() {
                                         return(names(self$results[['results']]))
                                       },
                                       entries = function() {
                                         return(self$results[['results']])
                                       },
                                       DataFrame = function() {
                                         return(data.frame(self$results[['results']],row.names=self$keys()))
                                       }
                                     )
)
