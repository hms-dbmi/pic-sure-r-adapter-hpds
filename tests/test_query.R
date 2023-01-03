source("../R/query.R", chdir = TRUE)
library(testthat)

test_that("parseQueryTemplate full query template", {
  mockSession <- list(
    currentResource = 'abc123',
    queryTemplate = "{\"categoryFilters\":{\"\\\\_consents\\\\\":[\"phs001345.c1\",\"phs000956.c2\",\"phs000287.c3\"],\"\\\\_topmed_consents\\\\\":[\"phs000993.c1\",\"phs001368.c2\",\"phs000993.c2\",\"phs001368.c1\",\"phs000964.c3\",\"phs000964.c4\",\"phs000988.c1\",\"phs000964.c1\",\"phs000964.c2\",\"phs001368.c4\",\"phs001211.c2\",\"phs001211.c1\",\"phs001416.c1\",\"phs001024.c1\",\"phs001416.c2\"],\"\\\\_harmonized_consent\\\\\":[\"phs001238.c1\",\"phs000956.c2\",\"phs001013.c1\"]},\"numericFilters\":{},\"requiredFields\":[],\"fields\":[\"\\\\_Topmed Study Accession with Subject ID\\\\\",\"\\\\_Parent Study Accession with Subject ID\\\\\"],\"variantInfoFilters\":[{\"categoryVariantInfoFilters\":{},\"numericVariantInfoFilters\":{}}],\"expectedResultType\":[\"COUNT\"]}"
  )
  mockQuery <- newQuery(mockSession)

  query = parseQueryTemplate(mockQuery)

  expect_equal(query$fields, list("\\_Topmed Study Accession with Subject ID\\", "\\_Parent Study Accession with Subject ID\\"))
  expect_type(query$categoryFilters$`\\_consents\\`, "list")
  expect_type(query$categoryFilters$`\\_topmed_consents\\`, "list")
  expect_type(query$categoryFilters$`\\_harmonized_consent\\`, "list")
  expect_equal(query$numericFilters, list())
  expect_equal(query$requiredFields, list())
})


test_that("parseQueryTemplate empty query template", {
  mockSession <- list(
    currentResource = 'abc123',
    queryTemplate = "{}"
  )
  mockQuery <- newQuery(mockSession)

  query = parseQueryTemplate(mockQuery)
  expect_equal(query$fields, list())
  expect_equal(query$categoryFilters, list())
  expect_equal(query$numericFilters, list())
  expect_equal(query$requiredFields, list())
})

test_that("parseQueryTemplate null query template", {
  mockSession <- list(
    currentResource = 'abc123',
    queryTemplate = "{}"
  )
  mockQuery <- newQuery(mockSession)

  query = parseQueryTemplate(mockQuery)
  expect_equal(query$fields, list())
  expect_equal(query$categoryFilters, list())
  expect_equal(query$numericFilters, list())
  expect_equal(query$requiredFields, list())
})
