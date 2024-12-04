source("../../R/bdc.R", chdir = TRUE)
library(testthat)

test_that("projectAndFilterResults handles nulls and in-scope results correctly", {
  # Mock input data
  mockResults <- list(
    list(result = list(
      metadata = list(
        columnmeta_HPDS_PATH = "\\phs000001\\unit_test\\test_categorical_variable\\",
        derived_var_id = "var1",
        derived_var_name = NULL,  # Should be converted to ""
        derived_var_description = NULL,  # Should be converted to ""
        columnmeta_data_type = "categorical",
        derived_group_id = "group1",
        derived_group_name = NULL,  # Should be converted to ""
        derived_group_description = "desc1",
        derived_study_id = "study1",
        derived_study_description = NULL,  # Should be converted to ""
        is_stigmatized = NULL,  # Should be converted to ""
        columnmeta_min = NULL,
        columnmeta_max = NULL
      ),
      is_categorical = TRUE,
      values = NULL  # Should be converted to ""
    )),
    list(result = list(
      metadata = list(
        columnmeta_HPDS_PATH = "\\phs000001\\unit_test\\test_continuous_variable\\",
        derived_var_id = "var2",
        derived_var_name = "Test Continuous Variable",
        derived_var_description = "A test continuous variable",
        columnmeta_data_type = "continuous",
        derived_group_id = "group2",
        derived_group_name = "Test Group",
        derived_group_description = "A description for group 2",
        derived_study_id = "study2",
        derived_study_description = "Study 2 Description",
        is_stigmatized = "false",
        columnmeta_min = "1.1",
        columnmeta_max = "42"
      ),
      is_categorical = FALSE,
      values = ""  # Should remain unchanged
    ))
  )

  mockScopes <- c("phs000001")
  mockShowAll <- TRUE

  # Run the function
  result <- projectAndFilterResults(mockResults, mockScopes, mockShowAll)

  expect_true(length(result$results) > 0, info = "No results returned; check input data or filtering logic.")

  # Verify results structure
  expect_true(is.list(result))
  expect_named(result, c("results", "paths"))

  # Verify paths
  expect_equal(result$paths, c("\\phs000001\\unit_test\\test_categorical_variable\\", "\\phs000001\\unit_test\\test_continuous_variable\\"))

  expect_equal(result$results[[1]]$var_name, "")  # NULL replaced with ""
  expect_equal(result$results[[1]]$min, NA)  # Categorical, so NA
  expect_equal(result$results[[1]]$categorical, TRUE)

  expect_equal(result$results[[2]]$name, "\\phs000001\\unit_test\\test_continuous_variable\\")
  expect_equal(result$results[[2]]$min, 1.1)  # Converted to numeric
  expect_equal(result$results[[2]]$max, 42)  # Converted to numeric
  expect_equal(result$results[[2]]$categorical, FALSE)
})
