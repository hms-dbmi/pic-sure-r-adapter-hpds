# PIC-SURE HPDS Adapter Library

#' export
PicSureHpdsAdapter <- R6::R6Class("PicSureHpdsAdapter",
                                  portable = FALSE,
                                  lock_objects = FALSE,
                                  public = list(
                                    initialize = function(PicSureConnection) {
                                      self$connection <- PicSureConnection
                                    },
                                    help = function() {},
                                    version = function() {},
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
                                      self$connection <- connection
                                      self$resourceUUID <- resource_uuid
                                    },
                                    help = function() {},
                                    dictionary = function() {
                                      return()
                                    },
                                    query = function() {
                                      return()
                                    }
                                  )
)









#' export
PicSureHpdsBypassAdapter <- R6::R6Class("PicSureHpdsBypassAdapter",
                                  portable = FALSE,
                                  lock_objects = FALSE,
                                  public = list(
                                    initialize = function(connectUrl, token) {},
                                    version = function() {},
                                    list = function() {},
                                    useResource = function() {},
                                    help = function() {},
                                  )
)

#' export
PicSureHpdsBypassConnection <- R6::R6Class("PicSureHpdsBypassConnection",
                                             portable = FALSE,
                                             lock_objects = FALSE,
                                             public = list(
                                               initialize = function() {},
                                               help = function() {},
                                             )
)

                               
#' export
PicSureHpdsBypassConnectionAPI <- R6::R6Class("PicSureHpdsBypassConnectionAPI",
                                           portable = FALSE,
                                           lock_objects = FALSE,
                                           public = list(
                                             initialize = function() {},
                                             help = function() {},
                                           )
)
