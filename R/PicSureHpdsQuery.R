# PIC-SURE HPDS Query(ish) Objects

#' export
PicSureHpdsQuery <- R6::R6Class("PicSureHpdsQuery",
                                portable = FALSE,
                                lock_objects = FALSE,
                                public = list(
                                  initialize = function(connection, resource_uuid) {
                                    self$connection <- connection
                                    self$resourceUUID <- resource_uuid
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
                                    return()
                                  },
                                  require = function() {
                                    return()
                                  },
                                  filter = function() {
                                    return()
                                  },
                                  getCount = function(asAsync = FALSE, timeout=30) {},
                                  getResults = function(asAsync = FALSE, timeout=30) {},
                                  getResultsDataFrame = function(asAsync = FALSE, timeout=30) {},
                                  getRunDetails = function() {},
                                  getQueryCommand = function() {},
                                  buildQuery = function(resultType="COUNT") {}
                                )
)


#' export
HpdsAttribList <- R6::R6Class("HpdsAttribList",
                                portable = FALSE,
                                lock_objects = FALSE,
                                public = list(
                                  initialize = function(inst_list=FALSE, help_text=FALSE) {
                                    self$helpstr = " [Help] valid commands are:
        |    add(): add a value
        |  delete(): delete a value
        |   show(): lists all current values
        |  clear(): clears all values from list
        |   help(): this command...
        "
                                    self$data = list()
                                    if (help_text != FALSE) {
                                      self$helpstr = help_text
                                    }
                                  },
                                  add = function() {},
                                  delete = function() {},
                                  show = function() {},
                                  clear = function() {},
                                  help = function() {}
                                )
)
                                  
                                  