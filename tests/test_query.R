source("../R/query.R", chdir = TRUE)
library(testthat)

mockDictionary <- list(
  list(
    name = "\\phs000001\\unit_test\\test_categorical_variable\\",
    min = NA,
    max = NA,
    categorical = TRUE,
    values = "Yes,No,Maybe"
  ),
  list(
    name = "\\phs000001\\unit_test\\test_continuous_variable\\",
    min = 1.1,
    max = 42,
    categorical = FALSE,
    values = ""
  )
)
mockDictionaryDF <- data.frame(do.call(rbind.data.frame, mockDictionary))
mockGenomicAnnotationsDF <- data.frame(do.call(rbind.data.frame, list()))
mockSession <- list(
  currentResource = 'abc123',
  queryTemplate = "{\"categoryFilters\":{\"\\\\_consents\\\\\":[\"phs001345.c1\",\"phs000956.c2\",\"phs000287.c3\"],\"\\\\_topmed_consents\\\\\":[\"phs000993.c1\",\"phs001368.c2\",\"phs000993.c2\",\"phs001368.c1\",\"phs000964.c3\",\"phs000964.c4\",\"phs000988.c1\",\"phs000964.c1\",\"phs000964.c2\",\"phs001368.c4\",\"phs001211.c2\",\"phs001211.c1\",\"phs001416.c1\",\"phs001024.c1\",\"phs001416.c2\"],\"\\\\_harmonized_consent\\\\\":[\"phs001238.c1\",\"phs000956.c2\",\"phs001013.c1\"]},\"numericFilters\":{},\"requiredFields\":[],\"fields\":[\"\\\\_Topmed Study Accession with Subject ID\\\\\",\"\\\\_Parent Study Accession with Subject ID\\\\\"],\"variantInfoFilters\":[{\"categoryVariantInfoFilters\":{},\"numericVariantInfoFilters\":{}}],\"expectedResultType\":[\"COUNT\"]}",
  dictionary = mockDictionaryDF,
  genomicAnnotations = mockGenomicAnnotationsDF
)
mockQuery <- newQuery(mockSession)

test_that("parseQueryTemplate parses full query template", {
  query = parseQueryTemplate(mockQuery)

  expect_equal(query$fields, list("\\_Topmed Study Accession with Subject ID\\", "\\_Parent Study Accession with Subject ID\\"))
  expect_type(query$categoryFilters$`\\_consents\\`, "list")
  expect_type(query$categoryFilters$`\\_topmed_consents\\`, "list")
  expect_type(query$categoryFilters$`\\_harmonized_consent\\`, "list")
  expect_equal(query$numericFilters, list())
  expect_equal(query$requiredFields, list())
})

test_that("parseQueryTemplate parses empty query template", {
  mockSession = list(
    currentResource = 'abc123',
    queryTemplate = "{}"
  )
  mockQuery = newQuery(mockSession)

  query = parseQueryTemplate(mockQuery)
  expect_equal(query$fields, list())
  expect_equal(query$categoryFilters, list())
  expect_equal(query$numericFilters, list())
  expect_equal(query$requiredFields, list())
})

test_that("parseQueryTemplate parses null query template", {
  mockSession = list(
    currentResource = 'abc123',
    queryTemplate = "{}"
  )
  mockQuery = newQuery(mockSession)

  query = parseQueryTemplate(mockQuery)
  expect_equal(query$fields, list())
  expect_equal(query$categoryFilters, list())
  expect_equal(query$numericFilters, list())
  expect_equal(query$requiredFields, list())
})


test_that("addClause() adds valid continuous variable filter", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_continuous_variable\\", type = "FILTER", min = 1.1, max = 20)
  expect_equal(length(mockQuery$numericFilters), 1)
})
test_that("addClause() adds valid continuous variable filter min only", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_continuous_variable\\", type = "FILTER", min = 3)
  expect_equal(length(mockQuery$numericFilters), 1)
})
test_that("addClause() adds valid continuous variable filter max only", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_continuous_variable\\", type = "FILTER", max = 20)
  expect_equal(length(mockQuery$numericFilters), 1)
})
test_that("addClause() does not add continuous variable filter without min or max", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_continuous_variable\\", type = "FILTER")
  expect_equal(length(mockQuery$numericFilters), 0)
})
test_that("addClause() does not add continuous variable filter with invalid min", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_continuous_variable\\", type = "FILTER", min = -1)
  expect_equal(length(mockQuery$numericFilters), 0)

  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_continuous_variable\\", type = "FILTER", min = 43)
  expect_equal(length(mockQuery$numericFilters), 0)
})
test_that("addClause() does not add continuous variable filter with invalid max", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_continuous_variable\\", type = "FILTER", max = -1)
  expect_equal(length(mockQuery$numericFilters), 0)

  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_continuous_variable\\", type = "FILTER", max = 43)
  expect_equal(length(mockQuery$numericFilters), 0)
})

test_that("addClause() adds valid categorical variable filter", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_categorical_variable\\", type = "FILTER", categories = list("Yes", "No"))
  expect_equal(length(mockQuery$categoryFilters[["\\phs000001\\unit_test\\test_categorical_variable\\"]]), 2)
})
test_that("addClause() does not add categorical variable filter without category value", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_categorical_variable\\", type = "FILTER")
  expect_equal(length(mockQuery$categoryFilters[["\\phs000001\\unit_test\\test_categorical_variable\\"]]), 0)
})
test_that("addClause() does not add multiple variable filter", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, list("\\phs000001\\unit_test\\test_categorical_variable\\", "\\phs000001\\unit_test\\test_continuous_variable\\"), type = "FILTER", categories = list("Yes", "No"))
  expect_equal(length(mockQuery$categoryFilters[["\\phs000001\\unit_test\\test_categorical_variable\\"]]), 0)
})

test_that("addClause() does not add invalid filter", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_not_existing_variable\\", type = "FILTER", categories = list("Yes", "No"))
  expect_equal(length(mockQuery$categoryFilters[["\\phs000001\\unit_test\\test_not_existing_variable\\"]]), 0)
})


test_that("addClause() adds valid select clause", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_categorical_variable\\", type = "SELECT")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% mockQuery$fields)
})
test_that("addClause() adds valid require clause", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_categorical_variable\\", type = "REQUIRE")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% mockQuery$requiredFields)
})
test_that("addClause() adds valid anyof clause", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_categorical_variable\\", type = "ANYOF")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% mockQuery$anyRecordOf)
})

test_that("addClause() adds multiple valid select clause", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, list("\\phs000001\\unit_test\\test_categorical_variable\\",  "\\phs000001\\unit_test\\test_continuous_variable\\"), type = "SELECT")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% mockQuery$fields)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% mockQuery$fields)
})
test_that("addClause() adds only valid select clause", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, list("\\phs000001\\unit_test\\test_categorical_variable\\",  "\\phs000001\\unit_test\\test_invalid_variable\\"), type = "SELECT")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% mockQuery$fields)
  expect_false("\\phs000001\\unit_test\\test_invalid_variable\\" %in% mockQuery$fields)
})

test_that("deleteClause() deletes select clause", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, list("\\phs000001\\unit_test\\test_categorical_variable\\",  "\\phs000001\\unit_test\\test_continuous_variable\\"), type = "SELECT")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% mockQuery$fields)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% mockQuery$fields)
  mockQuery = deleteClause(mockQuery, "\\phs000001\\unit_test\\test_categorical_variable\\")
  expect_false("\\phs000001\\unit_test\\test_categorical_variable\\" %in% mockQuery$fields)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% mockQuery$fields)
})

test_that("deleteClause() deletes require clause", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, list("\\phs000001\\unit_test\\test_categorical_variable\\",  "\\phs000001\\unit_test\\test_continuous_variable\\"), type = "REQUIRE")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% mockQuery$requiredFields)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% mockQuery$requiredFields)
  mockQuery = deleteClause(mockQuery, "\\phs000001\\unit_test\\test_categorical_variable\\")
  expect_false("\\phs000001\\unit_test\\test_categorical_variable\\" %in% mockQuery$requiredFields)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% mockQuery$requiredFields)
})

test_that("deleteClause() deletes anyof clause", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, list("\\phs000001\\unit_test\\test_categorical_variable\\",  "\\phs000001\\unit_test\\test_continuous_variable\\"), type = "ANYOF")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% mockQuery$anyRecordOf)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% mockQuery$anyRecordOf)
  mockQuery = deleteClause(mockQuery, "\\phs000001\\unit_test\\test_categorical_variable\\")
  expect_false("\\phs000001\\unit_test\\test_categorical_variable\\" %in% mockQuery$anyRecordOf)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% mockQuery$anyRecordOf)
})


test_that("deleteClause() deletes filter clause", {
  mockQuery = newQuery(mockSession)
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_categorical_variable\\", type = "FILTER", categories = list("Yes", "No"))
  mockQuery = addClause(mockQuery, "\\phs000001\\unit_test\\test_continuous_variable\\", type = "FILTER", min = 3)
  expect_equal(length(mockQuery$numericFilters), 1)
  expect_equal(length(mockQuery$categoryFilters[["\\phs000001\\unit_test\\test_categorical_variable\\"]]), 2)
  mockQuery = deleteClause(mockQuery, "\\phs000001\\unit_test\\test_categorical_variable\\")
  expect_equal(length(mockQuery$numericFilters), 1)
  expect_null(mockQuery$categoryFilters[["\\phs000001\\unit_test\\test_categorical_variable\\"]])
})
