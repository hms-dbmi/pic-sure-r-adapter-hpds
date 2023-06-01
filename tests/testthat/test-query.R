source("../../R/query.R", chdir = TRUE)
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
    min = "1.1",
    max = "42",
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

mockQueryMetaData <- list(
  status = "AVAILABLE",
  resourceID = "02e23f52-f354-4e8b-992c-d37c8b9ba140",
  resourceStatus = NULL,
  picsureResultId = "e6d2e680-bec1-4ded-85b6-791a7d8e4441",
  resourceResultId = "87e91691-7f1e-5606-97c0-a3e6acefea16",
  resultMetadata = list(
    queryResultMetadata = "[B@8132f1",
    queryJson = list(
      resourceCredentials = list(
        BEARER_TOKEN = NULL
      ),
      query = list(
        categoryFilters = list(
          `\\_consents\\` = list("phs001001.c1", "phs001001.c2"),
          `\\_topmed_consents\\` = list("phs001215.c0", "phs001217.c1")
        ),
        numericFilters = list(
          `\\phs000209\\pht001121\\phv00087080\\bmifc\\` = list(
            min = "20",
            max = "60"
          )
        ),
        requiredFields = list("\\phs000209\\pht001121\\phv00087119\\asthmaf\\"),
        fields = list("\\_Topmed Study Accession with Subject ID\\", "\\_Parent Study Accession with Subject ID\\"),
        variantInfoFilters = list(
          list(
            categoryVariantInfoFilters = list(
              Gene_with_variant = list("IL1R1"),
              Variant_consequence_calculated = list("splice_acceptor_variant", "splice_donor_variant", "stop_gained"),
              Variant_frequency_as_text = list("Rare", "Common")
            ),
            numericVariantInfoFilters = list()
          )
        ),
        expectedResultType = "DATAFRAME"
      ),
      resourceUUID = "02e23f52-f354-4e8b-992c-d37c8b9ba140"
    )
  )
)

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

test_that("getQueryFromMetadata correctly initializes a query", {
  # Mock session and queryJson

mockQueryJson <- list(
  fields = list("\\_Topmed Study Accession with Subject ID\\", "\\_Parent Study Accession with Subject ID\\"),
  requiredFields = list("\\phs000209\\pht001121\\phv00087119\\asthmaf\\"),
  categoryFilters = list("\\_consents\\" = list("phs001001.c1", "phs001001.c2", "phs001345.c1", "phs002694.c1"),
                         "\\_topmed_consents\\" = list("phs001215.c0", "phs001217.c1", "phs001217.c0", "phs001345.c1", "phs001215.c1")),
  anyRecordOf = list(),
  numericFilters = list("\\phs000209\\pht001121\\phv00087080\\bmifc\\" = list(min = "20", max = "60")),
  variantInfoFilters = list(list(
      categoryVariantInfoFilters = list(
        Gene_with_variant = list("IL1R1"),
        Variant_consequence_calculated = list("splice_acceptor_variant", "splice_donor_variant", "stop_gained"),
        Variant_frequency_as_text = list("Rare", "Common")
      ),
      numericVariantInfoFilters = list()
    ))
  )

  result <- getQueryFromMetadata(mockSession, mockQueryJson)

  mockQueryJson$variantInfoFilters <- list(
    categoryVariantInfoFilters = list(
      Gene_with_variant = list("IL1R1"),
      Variant_consequence_calculated = list("splice_acceptor_variant", "splice_donor_variant", "stop_gained"),
      Variant_frequency_as_text = list("Rare", "Common")
    ),
    numericVariantInfoFilters = list()
  )

  expect_equal(result$fields, mockQueryJson$fields)
  expect_equal(result$requiredFields, mockQueryJson$requiredFields)
  expect_equal(result$categoryFilters, mockQueryJson$categoryFilters)
  expect_equal(result$anyRecordOf, mockQueryJson$anyRecordOf)
  expect_equal(result$numericFilters, mockQueryJson$numericFilters)
  expect_equal(result$variantInfoFilters, mockQueryJson$variantInfoFilters)
})

test_that("getQueryFromMetadata() formats parsed object", {
  result <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)
  print(names(result))

  expectedNames <- c("session", "fields", "requiredFields", "categoryFilters",
    "anyRecordOf", "numericFilters", "variantInfoFilters", "resourceId")

  actualNames <- names(result)

  expect_true(setequal(sort(actualNames), sort(expectedNames)))

  # The result should be a list
  expect_true(is.list(result))

  # Check that the values in each list component match the values in the input
  expect_equal(result$fields, mockQueryMetaData$resultMetadata$queryJson$query$fields)
  expect_equal(result$requiredFields, mockQueryMetaData$resultMetadata$queryJson$query$requiredFields)
  expect_equal(result$categoryFilters, mockQueryMetaData$resultMetadata$queryJson$query$categoryFilters)
  expect_equal(result$anyRecordOf, as.list(mockQueryMetaData$resultMetadata$queryJson$query$anyRecordOf))
  expect_equal(result$numericFilters, mockQueryMetaData$resultMetadata$queryJson$query$numericFilters)
  expect_equal(result$variantInfoFilters$categoryVariantInfoFilters,
               mockQueryMetaData$resultMetadata$queryJson$query$variantInfoFilters[[1]]$categoryVariantInfoFilters)
  expect_equal(result$variantInfoFilters$numericVariantInfoFilters,
               mockQueryMetaData$resultMetadata$queryJson$query$variantInfoFilters[[1]]$numericVariantInfoFilters)
})

test_that("getQueryFromMetadata() returns expected object properties", {
  result <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)

  expectedNames <- c("session", "fields", "requiredFields", "categoryFilters",
                     "anyRecordOf", "numericFilters", "variantInfoFilters", "resourceId")
  actualNames <- names(result)

  # Confirm all expected names exist in the object. We sort because order isn't guarenteed
  expect_true(setequal(sort(actualNames), sort(expectedNames)))
})

test_that("getQueryFromMetadata() handles NULL categoryVariantInfoFilters", {
    modifiedMockQuery <- mockQueryMetaData$resultMetadata$queryJson$query
    modifiedMockQuery$variantInfoFilters[[1]]$categoryVariantInfoFilters$Gene_with_variant <- list(NULL)

    result <- getQueryFromMetadata(mockSession, modifiedMockQuery)

    expect_null(result$variantInfoFilters$categoryVariantInfoFilters$Gene_with_variant)
})

test_that("addClause() adds valid continuous variable filter to query object produced by getQueryFromMetadata()", {
  query <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)
  query = addClause(query, "\\phs000001\\unit_test\\test_continuous_variable\\", type = "FILTER", min = 1.1, max = 20)
  expect_equal(length(query$numericFilters), 2)
})

test_that("addClause() adds valid continuous variable filter min only to query object produced by getQueryFromMetadata()", {
  query <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)
  query = addClause(query, "\\phs000001\\unit_test\\test_continuous_variable\\", type = "FILTER", min = 3)
  expect_equal(length(query$numericFilters), 2)
})

test_that("addClause() adds valid categorical variable filter to query object produced by getQueryFromMetadata()", {
  mockQueryMetaData$resultMetadata$queryJson$query$categoryFilters <- list()
  query <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)
  query = addClause(query, "\\phs000001\\unit_test\\test_categorical_variable\\", type = "FILTER", categories = list("Yes", "No"))
  expect_equal(length(query$categoryFilters[["\\phs000001\\unit_test\\test_categorical_variable\\"]]), 2)
})

test_that("addClause() adds valid select clause to query object produced by getQueryFromMetadata()", {
  query <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)
  query = addClause(query, "\\phs000001\\unit_test\\test_categorical_variable\\", type = "SELECT")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% query$fields)
})

test_that("addClause() adds valid require clause to query object produced by getQueryFromMetadata()", {
  query <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)
  query = addClause(query, "\\phs000001\\unit_test\\test_categorical_variable\\", type = "REQUIRE")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% query$requiredFields)
})

test_that("addClause() adds valid anyof clause to query object produced by getQueryFromMetadata()", {
  query <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)
  query = addClause(query, "\\phs000001\\unit_test\\test_categorical_variable\\", type = "ANYOF")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% query$anyRecordOf)
})

test_that("addClause() adds multiple valid select clause to query object produced by getQueryFromMetadata()", {
  query <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)
  query = addClause(query, list("\\phs000001\\unit_test\\test_categorical_variable\\",  "\\phs000001\\unit_test\\test_continuous_variable\\"), type = "SELECT")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% query$fields)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% query$fields)
})
test_that("deleteClause() deletes select clause to query object produced by getQueryFromMetadata()", {
  query <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)
  query = addClause(query, list("\\phs000001\\unit_test\\test_categorical_variable\\",  "\\phs000001\\unit_test\\test_continuous_variable\\"), type = "SELECT")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% query$fields)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% query$fields)
  query = deleteClause(query, "\\phs000001\\unit_test\\test_categorical_variable\\")
  expect_false("\\phs000001\\unit_test\\test_categorical_variable\\" %in% query$fields)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% query$fields)
})

test_that("deleteClause() deletes require clause to query object produced by getQueryFromMetadata()", {
  query <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)
  query = addClause(query, list("\\phs000001\\unit_test\\test_categorical_variable\\",  "\\phs000001\\unit_test\\test_continuous_variable\\"), type = "REQUIRE")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% query$requiredFields)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% query$requiredFields)
  query = deleteClause(query, "\\phs000001\\unit_test\\test_categorical_variable\\")
  expect_false("\\phs000001\\unit_test\\test_categorical_variable\\" %in% query$requiredFields)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% query$requiredFields)
})

test_that("deleteClause() deletes anyof clause to query object produced by getQueryFromMetadata()", {
  query <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)
  query = addClause(query, list("\\phs000001\\unit_test\\test_categorical_variable\\",  "\\phs000001\\unit_test\\test_continuous_variable\\"), type = "ANYOF")
  expect_true("\\phs000001\\unit_test\\test_categorical_variable\\" %in% query$anyRecordOf)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% query$anyRecordOf)
  query = deleteClause(query, "\\phs000001\\unit_test\\test_categorical_variable\\")
  expect_false("\\phs000001\\unit_test\\test_categorical_variable\\" %in% query$anyRecordOf)
  expect_true("\\phs000001\\unit_test\\test_continuous_variable\\" %in% query$anyRecordOf)
})


test_that("deleteClause() deletes filter clause to query object produced by getQueryFromMetadata()", {
  query <- getQueryFromMetadata(mockSession, mockQueryMetaData$resultMetadata$queryJson$query)
  query = addClause(query, "\\phs000001\\unit_test\\test_categorical_variable\\", type = "FILTER", categories = list("Yes", "No"))
  query = addClause(query, "\\phs000001\\unit_test\\test_continuous_variable\\", type = "FILTER", min = 3)
  expect_equal(length(query$numericFilters), 2)
  expect_equal(length(query$categoryFilters[["\\phs000001\\unit_test\\test_categorical_variable\\"]]), 2)
  query = deleteClause(query, "\\phs000001\\unit_test\\test_categorical_variable\\")
  expect_equal(length(query$numericFilters), 2)
  expect_null(query$categoryFilters[["\\phs000001\\unit_test\\test_categorical_variable\\"]])
})




