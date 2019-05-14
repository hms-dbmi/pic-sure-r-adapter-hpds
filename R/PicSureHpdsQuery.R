# PIC-SURE HPDS Query(ish) Objects

#' export
PicSureHpdsQuery <- R6::R6Class("PicSureHpdsQuery",
                                portable = FALSE,
                                lock_objects = FALSE,
                                private = list(),
                                public = list(
                                  initialize = function(connection) {
                                    self$listSelect = HpdsAttribListKeys$new(
                                        help_text='
          select()$
             add("key")            add a single column to be returned in results
             delete("key")         delete a single column from the list of columns to return
             show()                lists all current columns that will be returned in results
             clear()               clears all values from the select list'
                                    )
                                    self$listRequire = HpdsAttribListKeys$new(
                                      help_text='
          require()$
            add("key")            add a single column that must exist within each results record
            delete("key")         delete a single column from the list of columns to that results records must have
            show()                lists all current columns that results records must have
            clear()               clears all values from the require list'
                                    )
                                    self$listFilter = HpdsAttribListKeyValues$new(
                                      help_text='
            filter()$
              add("key", value)                  - or -
              add("key", "value")               filter to records with KEY column that equals VALUE
              add("key", ["value1", "value2"])  filter to records with KEY column equalling one value within the given list
              add("key", start, end)            filter to records with KEY column value between START and END (inclusive)
                                                    start -or- end may be set to None to filter by a max or min value
              delete("key")                     delete a filter from the list of filters
              show()                            lists all current filters that results records must satisfy
              clear()                           clears all values from the filters list'
                                    )
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
                                  help = function() {
                                    cat("        $select()   list of data fields to return from resource for each record\n")
                                    cat("        $require()  list of data fields that must be present in all returned records\n")
                                    cat("        $filter()   list of data fields and conditions that returned records satisfy\n")
                                    cat("           [ Filter keys exert an AND relationship on returned records      ]\n")
                                    cat("           [ Categorical values have an OR relationship on their key        ]\n")
                                    cat("           [ Numerical Ranges are inclusive of their start and end points   ]\n\n")
                                    cat("        $getCount()             returns a count indicating the number of matching numbers\n")
                                    cat("        $getResults()           returns a CSV-like string containing the matching records\n")
                                    cat("        $getResultsDataFrame()  returns a pandas DataFrame containing the matching records\n")
                                    cat("        $getRunDetails()        returns details about the last run of the query\n")
                                    cat("        $getQueryCommand()      returns the JSON-formatted query request\n")
                                    cat("        $show()                 lists all current query parameters\n\n")
                                    cat("        * $getCount(), $getResults(), and $getResultsDataFrame() functions can\n")
                                    cat("        also accept options that run queries differently which might help with\n")
                                    cat("        connection timeouts. Example: myQry$getResults(async=True, timeout=60)\n")
                                  },
                                  show = function() {},
                                  select = function() {
                                    return(self$listSelect)
                                  },
                                  require = function() {
                                    return(self$listRequire)
                                  },
                                  filter = function() {
                                    return(self$listFilter)
                                  },
                                  getCount = function(asAsync = FALSE, timeout=30) {
                                    queryJSON = self$buildQuery("COUNT")
                                    # bugfix for jsonlite
                                    queryJSON <- jsonlite::toJSON(queryJSON, auto_unbox = TRUE)
                                    queryJSON <- str_replace_all(queryJSON, '"numericFilters":\\[\\]','"numericFilters":\\{\\}')
                                    queryJSON <- str_replace_all(queryJSON, '"categoryFilters":\\[\\]','"categoryFilters":\\{\\}')
                                    httpResults = self$INTERNAL_API_OBJ$synchQuery(self$resourceUUID, queryJSON)
                                    return(as.integer(httpResults))
                                  },
                                  getResults = function(asAsync = FALSE, timeout=30) {
                                    queryJSON = self$buildQuery("DATAFRAME")
                                    # bugfix for jsonlite
                                    queryJSON <- jsonlite::toJSON(queryJSON, auto_unbox = TRUE)
                                    queryJSON <- str_replace_all(queryJSON, '"numericFilters":\\[\\]','"numericFilters":\\{\\}')
                                    queryJSON <- str_replace_all(queryJSON, '"categoryFilters":\\[\\]','"categoryFilters":\\{\\}')
                                    httpResults = self$INTERNAL_API_OBJ$synchQuery(self$resourceUUID, queryJSON)
                                    return(read.csv(text=httpResults))
                                  },
                                  getResultsDataFrame = function(asAsync = FALSE, timeout=30) {
                                    queryJSON = self$buildQuery("DATAFRAME")
                                    # bugfix for jsonlite
                                    queryJSON <- jsonlite::toJSON(queryJSON, auto_unbox = TRUE)
                                    queryJSON <- str_replace_all(queryJSON, '"numericFilters":\\[\\]','"numericFilters":\\{\\}')
                                    queryJSON <- str_replace_all(queryJSON, '"categoryFilters":\\[\\]','"categoryFilters":\\{\\}')
                                    httpResults = self$INTERNAL_API_OBJ$synchQuery(self$resourceUUID, queryJSON)
                                    return(read.csv(text=httpResults))
                                  },
                                  getRunDetails = function() {},
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


#' export
HpdsAttribList <- R6::R6Class("HpdsAttribList",
                                portable = FALSE,
                                lock_objects = FALSE,
                                public = list(
                                  initialize = function(inst_list=FALSE, help_text=FALSE) {
                                    self$helpstr <- " [Help] valid commands are:
        |    $add(): add a value
        | $delete(): delete a value
        |   $show(): lists all current values
        |  $clear(): clears all values from list
        |   $help(): this command...
        "
                                    self$data <- hash()  
                                    if (help_text != FALSE) {
                                      self$helpstr <- help_text
                                    }
                                  },
                                  add = function(key, ...) {
                                    if (key==FALSE) {
                                      cat("ERROR: No key specified!")
                                      return(FALSE)  
                                    }
                                    if (has.key(key, self$data) == TRUE) {
                                      print('ERROR: cannot add, key already exists')
                                      print(key)
                                    } else {
                                      #setting the key depending on input arguments
                                      args = list(...)
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
                                    invisible(self)
                                  },
                                  delete = function(key, ...) {
                                    args <- list(...)
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
                                    invisible(self)
                                  },
                                  show = function() {
                                    return(jsonlite::prettify(jsonlite::toJSON(self$getQueryValues(), auto_unbox = TRUE)))
                                  },
                                  clear = function() {
                                    self$data <- hash()
#                                    cat("cleared list")
                                    invisible(self)
                                  },
                                  help = function() {
                                    cat(self$helpstr)
                                  },
                                  getQueryValues = function() {
                                    return(jsonlite::toJSON(self$data, auto_unbox = TRUE))
                                  }
                                )
)
                                  

#' export
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
                                add = function(key, ...) {
                                  if (key==FALSE) {
                                    cat("ERROR: No key specified!")
                                  } else {
                                    #setting the key as exists filter
                                    super$add(key)
                                  }
                                  invisible(self)
                                },
                                delete = function(key, ...) {
                                  super$delete(key)
                                  invisible(self)
                                },
                                help = function() {
                                  super$help()
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

#' export
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
                                    add = function(key, ...) {
                                      #setting the key as exists filter
                                      super$add(key, ...)
                                      invisible(self)
                                    },
                                    delete = function(key, ...) {
                                      super$delete(key, ...)
                                      invisible(self)
                                    },
                                    help = function() {
                                      super$help()
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
