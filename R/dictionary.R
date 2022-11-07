search <- function(connection, keyword, limit = 10000) {
  searchQuery = jsonlite::toJSON(list(query = keyword), auto_unbox=TRUE)
  result <- postJSON(connection, "search/bf751312-419f-413e-936b-87975011fe18", searchQuery)
  #return (data.frame(do.call(rbind.data.frame, result)))
  return (result)
}

searchPicsure <- function(connection, keyword = "", resultType = "DATA_FRAME") {
  searchQuery = jsonlite::toJSON(list(query = keyword), auto_unbox=TRUE)
  result <- postJSON(connection, "search/bf751312-419f-413e-936b-87975011fe18", searchQuery)

  if(toupper(resultType) == "DICTIONARY")
    return (result$results)
  if (toupper(resultType) == "VARIABLE_PATHS")
    return (unique(result$paths))
  if (toupper(resultType) == "DATA_FRAME")
    return (getDataFrame(result))
}

getDataFrame <- function(results) {
  output_df <- data.frame()
  # process JSON objects into dataframe
  for (idx1 in 1:length(results$results)) {
    result_type <- names(results$results[idx1])
    temp_list <- unname(results$results[[idx1]])
    temp_keys <- names(results$results[[idx1]])
    if (length(temp_list) > 0) {
      if (result_type == "phenotypes") {
        temp_categoricals <- list()
        for (idx2 in 1:length(temp_list)) {
          if (temp_list[[idx2]][["categorical"]] == TRUE) {
            temp_list[[idx2]][["min"]] <- NA
            temp_list[[idx2]][["max"]] <- NA
            if (length(temp_list[[idx2]][["categoryValues"]]) > 0) {
              temp_categoricals[[idx2]] <- temp_list[[idx2]][["categoryValues"]]
            } else {
              temp_categoricals[[idx2]] <- NA
            }

          } else {
            temp_categoricals[[idx2]] <- NA
          }
          temp_list[[idx2]][["categoryValues"]] <- NULL
        }
        temp_df <- data.frame(do.call(rbind.data.frame, temp_list))
        temp_df$HpdsDataType <- result_type
        temp_df$categoryValues <- temp_categoricals
        temp_df$description <- NA
        # normalize categorical/continuous vars (only categorical)
        # temp_df$continuous <- !temp_df$categorical
      } else {
        temp_values <- list()
        for (idx2 in 1:length(temp_list)) {
          temp_var = temp_list[[idx2]][["values"]]
          if (length(temp_var) == 0) {
            temp_values[[idx2]] <- NA
          } else {
            temp_values[[idx2]] <- temp_var
          }
          temp_list[[idx2]][["values"]] <- NULL
        }
        temp_df <- data.frame(do.call(rbind.data.frame, temp_list))
        temp_df$name <- temp_keys # populate the name field for "info" data records
        temp_df$min <- NA
        # normalize categorical/continuous vars (only categorical)
        temp_df$categorical <- !temp_df$continuous
        temp_df$continuous <- NULL
        temp_df$patientCount <- NA
        temp_df$observationCount <- NA
        temp_df$max <- NA
        temp_df$HpdsDataType <- result_type
        temp_df$categoryValues <- temp_values
      }
      output_df <- rbind(output_df, temp_df)
    }
  }

  # filter based on queryScopes
  # if (isTRUE(useQueryScopes)) {
  #   if (length(connection$queryScopes) > 0) {
  #     # get the genomic info records
  #     cumulative <- output_df$HpdsDataType != "phenotypes"
  #     for (matchidx in 1:length(connection$queryScopes)) {
  #       cumulative <- cumulative | str_detect(output_df$name, fixed(connection$queryScopes[[matchidx]]))
  #     }
  #     output_df <- output_df[cumulative, ]
  #   }
  # }
  return(output_df)
}