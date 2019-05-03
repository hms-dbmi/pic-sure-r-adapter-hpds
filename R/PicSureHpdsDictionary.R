# PIC-SURE HPDS Dictonary(ish) Objects


#' export
PicSureHpdsDictionary <- R6::R6Class("PicSureHpdsDictionary",
                                        portable = FALSE,
                                        lock_objects = FALSE,
                                        public = list(
                                          initialize = function(refHpdsResourceConnection) {
                                            self$connection <- refHpdsResourceConnection
                                            self$resourceUUID <- refHpdsResourceConnection$resourceUUID
                                          },
                                          find = function(term=FALSE) {},
                                          help = function() {}
                                        )
)


#' export
PicSureHpdsDictionaryResult <- R6::R6Class("PicSureHpdsDictionaryResult",
                                     portable = FALSE,
                                     lock_objects = FALSE,
                                     public = list(
                                       initialize = function(results) {
                                         self$results <- results
                                       },
                                       help = function() {},
                                       count = function() {},
                                       keys = function() {},
                                       entries = function() {},
                                       DataFrame = function() {}
                                     )
)
