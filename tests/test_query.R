source("../R/query.R", chdir = TRUE)
library(testthat)

mockSession <- list(
  currentResource = 'abc123',
  queryTemplate = "{\"categoryFilters\":{\"\\\\_consents\\\\\":[\"phs001345.c1\",\"phs000956.c2\",\"phs000946.c1\",\"phs001368.c2\",\"phs001402.c1\",\"phs001368.c1\",\"phs001143.c1\",\"phs000988.c1\",\"phs001252.c1\",\"phs000286.c4\",\"phs000286.c3\",\"phs000810.c2\",\"phs000810.c1\",\"phs000286.c2\",\"phs000286.c1\",\"phs000200.c1\",\"phs000951.c2\",\"phs000951.c1\",\"phs001237.c1\",\"phs001237.c2\",\"phs000007.c2\",\"phs000007.c1\",\"phs000993.c1\",\"phs000179.c1\",\"phs000179.c2\",\"phs000993.c2\",\"phs000285.c2\",\"phs000285.c1\",\"phs001368.c4\",\"phs000209.c1\",\"phs000209.c2\",\"phs001238.c1\",\"phs000954.c1\",\"phs001013.c1\",\"phs001013.c2\",\"phs002299.c1\",\"phs000280.c1\",\"phs000284.c1\",\"phs002385.c1\",\"phs000280.c2\",\"phs000974.c1\",\"phs000200.c2\",\"phs000974.c2\",\"phs000964.c3\",\"phs001359.c1\",\"phs000964.c4\",\"phs000964.c1\",\"phs000964.c2\",\"phs000287.c1\",\"phs001211.c2\",\"phs000287.c2\",\"phs001211.c1\",\"phs000287.c3\",\"phs000287.c4\",\"phs001416.c1\",\"phs001024.c1\",\"phs001416.c2\"],\"\\\\_topmed_consents\\\\\":[\"phs000951.c2\",\"phs000974.c1\",\"phs000951.c1\",\"phs001345.c1\",\"phs001237.c1\",\"phs001237.c2\",\"phs000974.c2\",\"phs000954.c1\",\"phs000946.c1\",\"phs000993.c1\",\"phs001368.c2\",\"phs000993.c2\",\"phs001368.c1\",\"phs000964.c3\",\"phs000964.c4\",\"phs000988.c1\",\"phs000964.c1\",\"phs000964.c2\",\"phs001368.c4\",\"phs001211.c2\",\"phs001211.c1\",\"phs001416.c1\",\"phs001024.c1\",\"phs001416.c2\"],\"\\\\_harmonized_consent\\\\\":[\"phs001238.c1\",\"phs000956.c2\",\"phs001013.c1\",\"phs001013.c2\",\"phs000988.c1\",\"phs000280.c1\",\"phs000286.c4\",\"phs000286.c3\",\"phs000810.c2\",\"phs000286.c2\",\"phs000810.c1\",\"phs000284.c1\",\"phs000286.c1\",\"phs000200.c1\",\"phs000280.c2\",\"phs000200.c2\",\"phs000007.c2\",\"phs000007.c1\",\"phs000179.c1\",\"phs000179.c2\",\"phs000285.c2\",\"phs000285.c1\",\"phs000287.c1\",\"phs000287.c2\",\"phs000287.c3\",\"phs000287.c4\",\"phs000209.c1\",\"phs000209.c2\"]},\"numericFilters\":{},\"requiredFields\":[],\"fields\":[\"\\\\_Topmed Study Accession with Subject ID\\\\\",\"\\\\_Parent Study Accession with Subject ID\\\\\"],\"variantInfoFilters\":[{\"categoryVariantInfoFilters\":{},\"numericVariantInfoFilters\":{}}],\"expectedResultType\":[\"COUNT\"]}"
)

mockQuery <- newQuery(mockSession)

test_that("1 equals 1", {
  expect_equal(1, 1)
})

test_that("parseQueryTemplate", {
  query = parseQueryTemplate(mockQuery)
  expect_equal(query$fields, list("\\_Topmed Study Accession with Subject ID\\", "\\_Parent Study Accession with Subject ID\\"))
})
