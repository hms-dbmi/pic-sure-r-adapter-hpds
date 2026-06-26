# Public enum constants mirroring the Python adapter's enums.
#
# Each member is an S3 list with class c(<subclass>, "picsure_enum_member").
# Subclasses (picsure_phenotypic_filter_type, picsure_group_operator,
# picsure_query_type, picsure_platform) reserve room for per-enum dispatch;
# today only picsure_platform uses it (richer print).
#
# Member objects are accepted as inputs by buildClause(), buildClauseGroup(),
# runQuery(), and connect() in addition to the existing case-insensitive
# string inputs.

#' @keywords internal
.enum_member <- function(name, value, enum_name, ..., subclass = NULL) {
  structure(
    c(list(name = name, value = value), list(...)),
    enum_name = enum_name,
    class     = c(subclass, "picsure_enum_member")
  )
}

#' @export
format.picsure_enum_member <- function(x, ...) {
  enum_name <- attr(x, "enum_name")
  if (is.null(enum_name)) enum_name <- "Enum"
  sprintf("<%s.%s>", enum_name, x$name)
}

#' @export
print.picsure_enum_member <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

#' @export
as.character.picsure_enum_member <- function(x, ...) x$name

#' Phenotypic filter clause types.
#'
#' Pass a member to [`buildClause()`][picsure::buildClause]'s `type`
#' argument. Mirrors Python's `picsure.PhenotypicFilterType`.
#'
#' To include concept paths in query output without filtering, use
#' [`buildQuery()`][picsure::buildQuery]'s `includeConcepts` argument — output
#' columns are no longer a clause type.
#'
#' @format A list of `picsure_enum_member` objects:
#' \describe{
#'   \item{`FILTER`}{Filter by categorical values or numeric range.}
#'   \item{`ANYRECORD`}{Match records where the concept path *or any
#'     descendant* has a value (wire: `ANY_RECORD_OF`).}
#'   \item{`REQUIRE`}{Require the concept path to have a non-null value
#'     (wire: `REQUIRED`).}
#' }
#' @examples
#' \dontrun{
#' picsure::buildClause(
#'   "\\phs1\\pht1\\phv1\\sex\\",
#'   type = picsure::PhenotypicFilterType$FILTER,
#'   categories = "male"
#' )
#' }
#' @export
PhenotypicFilterType <- list(
  FILTER    = .enum_member("FILTER",    "filter",    enum_name = "PhenotypicFilterType", subclass = "picsure_phenotypic_filter_type"),
  ANYRECORD = .enum_member("ANYRECORD", "anyrecord", enum_name = "PhenotypicFilterType", subclass = "picsure_phenotypic_filter_type"),
  REQUIRE   = .enum_member("REQUIRE",   "require",   enum_name = "PhenotypicFilterType", subclass = "picsure_phenotypic_filter_type")
)

#' Logical operators for combining clauses in a group.
#'
#' Pass a member to [`buildClauseGroup()`][picsure::buildClauseGroup]'s
#' `operator` argument. Mirrors Python's `picsure.GroupOperator`.
#'
#' @format A list of `picsure_enum_member` objects:
#' \describe{
#'   \item{`AND`}{All clauses must match.}
#'   \item{`OR`}{At least one clause must match.}
#' }
#' @examples
#' \dontrun{
#' c1 <- picsure::buildClause("\\phs1\\sex\\",
#'                              type = picsure::PhenotypicFilterType$FILTER,
#'                              categories = "male")
#' c2 <- picsure::buildClause("\\phs1\\copd\\",
#'                              type = picsure::PhenotypicFilterType$FILTER,
#'                              categories = "Yes")
#' picsure::buildClauseGroup(
#'   list(c1, c2),
#'   operator = picsure::GroupOperator$AND
#' )
#' }
#' @export
GroupOperator <- list(
  AND = .enum_member("AND", "AND", enum_name = "GroupOperator", subclass = "picsure_group_operator"),
  OR  = .enum_member("OR",  "OR",  enum_name = "GroupOperator", subclass = "picsure_group_operator")
)

#' Query result types for `runQuery()`.
#'
#' Pass a member to [`runQuery()`][picsure::runQuery]'s `type` argument.
#' Mirrors Python's `picsure.QueryType`.
#'
#' @format A list of `picsure_enum_member` objects:
#' \describe{
#'   \item{`COUNT`}{Returns a `CountResult` with the matching-participant
#'     count (or NULL on small-cohort obfuscation).}
#'   \item{`PARTICIPANT`}{Returns a data.frame with one row per
#'     matching participant.}
#'   \item{`TIMESTAMP`}{Returns a data.frame of participant-level
#'     timestamps for longitudinal concepts.}
#'   \item{`CROSS_COUNT`}{Returns a list of `CountResult`s keyed by
#'     concept path.}
#'   \item{`VARIANT_COUNT`}{Returns a `CountResult` for the number of distinct
#'     matching variants (preserving obfuscation, like `COUNT`).}
#'   \item{`VARIANT_LIST`}{Returns a character vector of variant spec strings.}
#'   \item{`VCF_EXCERPT`}{Returns a data.frame, one row per variant, with
#'     per-patient genotype columns.}
#'   \item{`AGGREGATE_VCF_EXCERPT`}{Like `VCF_EXCERPT` without patient columns.}
#' }
#' @examples
#' \dontrun{
#' picsure::runQuery(session, query, type = picsure::QueryType$COUNT)
#' }
#' @export
QueryType <- list(
  COUNT                 = .enum_member("COUNT",                 "count",                 enum_name = "QueryType", subclass = "picsure_query_type"),
  PARTICIPANT           = .enum_member("PARTICIPANT",           "participant",           enum_name = "QueryType", subclass = "picsure_query_type"),
  TIMESTAMP             = .enum_member("TIMESTAMP",             "timestamp",             enum_name = "QueryType", subclass = "picsure_query_type"),
  CROSS_COUNT           = .enum_member("CROSS_COUNT",           "cross_count",           enum_name = "QueryType", subclass = "picsure_query_type"),
  VARIANT_COUNT         = .enum_member("VARIANT_COUNT",         "variant_count",         enum_name = "QueryType", subclass = "picsure_query_type"),
  VARIANT_LIST          = .enum_member("VARIANT_LIST",          "variant_list",          enum_name = "QueryType", subclass = "picsure_query_type"),
  VCF_EXCERPT           = .enum_member("VCF_EXCERPT",           "vcf_excerpt",           enum_name = "QueryType", subclass = "picsure_query_type"),
  AGGREGATE_VCF_EXCERPT = .enum_member("AGGREGATE_VCF_EXCERPT", "aggregate_vcf_excerpt", enum_name = "QueryType", subclass = "picsure_query_type")
)

#' Variant population-frequency buckets.
#'
#' Pass a member to [`buildGenomicFilter()`][picsure::buildGenomicFilter]'s
#' `values` argument for the `"Variant_frequency_as_text"` key. Mirrors
#' Python's `picsure.VariantFrequency`.
#'
#' @format A list of `picsure_enum_member` objects:
#' \describe{
#'   \item{`RARE`}{Rare variants.}
#'   \item{`COMMON`}{Common variants.}
#'   \item{`NOVEL`}{Novel variants.}
#' }
#' @examples
#' \dontrun{
#' picsure::buildGenomicFilter(
#'   "Variant_frequency_as_text",
#'   values = picsure::VariantFrequency$RARE
#' )
#' }
#' @export
VariantFrequency <- list(
  RARE   = .enum_member("RARE",   "Rare",   enum_name = "VariantFrequency", subclass = "picsure_variant_frequency"),
  COMMON = .enum_member("COMMON", "Common", enum_name = "VariantFrequency", subclass = "picsure_variant_frequency"),
  NOVEL  = .enum_member("NOVEL",  "Novel",  enum_name = "VariantFrequency", subclass = "picsure_variant_frequency")
)

#' Known PIC-SURE deployment platforms.
#'
#' Pass a member to [`connect()`][picsure::connect]'s `platform`
#' argument. Mirrors Python's `picsure.Platform`. Each member exposes
#' the connection URL, default resource UUID, label, and policy flags.
#'
#' @format A list of `picsure_enum_member` (subclass `picsure_platform`)
#' objects:
#' \describe{
#'   \item{`BDC_AUTHORIZED`}{BDC production, authenticated.}
#'   \item{`BDC_OPEN`}{BDC production, open.}
#'   \item{`BDC_DEV_AUTHORIZED`}{BDC dev, authenticated.}
#'   \item{`BDC_DEV_OPEN`}{BDC dev, open.}
#'   \item{`BDC_PREDEV_AUTHORIZED`}{BDC predev, authenticated.}
#'   \item{`BDC_PREDEV_OPEN`}{BDC predev, open.}
#'   \item{`NHANES_AUTHORIZED`}{NHANES, authenticated.}
#'   \item{`NHANES_OPEN`}{NHANES, open.}
#' }
#' @examples
#' \dontrun{
#' picsure::connect(platform = picsure::Platform$BDC_OPEN, token = "")
#' }
#' @export
Platform <- local({
  # value list mirrors Python's `.value` (a PlatformConfig dataclass);
  # flat fields mirror Python's @property accessors. Both shapes are
  # exposed so R users can read either way and Python docs translate
  # 1:1.
  mk <- function(name, url, resource_uuid, label, include_consents, requires_auth) {
    .enum_member(
      name             = name,
      value            = list(
        url              = url,
        resource_uuid    = resource_uuid,
        label            = label,
        include_consents = include_consents,
        requires_auth    = requires_auth
      ),
      url              = url,
      resource_uuid    = resource_uuid,
      label            = label,
      include_consents = include_consents,
      requires_auth    = requires_auth,
      enum_name        = "Platform",
      subclass         = "picsure_platform"
    )
  }
  list(
    BDC_AUTHORIZED        = mk("BDC_AUTHORIZED",        "https://picsure.biodatacatalyst.nhlbi.nih.gov",        "02e23f52-f354-4e8b-992c-d37c8b9ba140", "BDC Authorized",    TRUE,  TRUE),
    BDC_OPEN              = mk("BDC_OPEN",              "https://picsure.biodatacatalyst.nhlbi.nih.gov",        "ac004461-1b47-4832-80e2-22a4aecabe39", "BDC Open",          FALSE, FALSE),
    BDC_DEV_AUTHORIZED    = mk("BDC_DEV_AUTHORIZED",    "https://dev.picsure.biodatacatalyst.nhlbi.nih.gov",    "02e23f52-f354-4e8b-992c-d37c8b9ba140", "BDC Authorized",    TRUE,  TRUE),
    BDC_DEV_OPEN          = mk("BDC_DEV_OPEN",          "https://dev.picsure.biodatacatalyst.nhlbi.nih.gov",    "ac004461-1b47-4832-80e2-22a4aecabe39", "BDC Open",          FALSE, FALSE),
    BDC_PREDEV_AUTHORIZED = mk("BDC_PREDEV_AUTHORIZED", "https://predev.picsure.biodatacatalyst.nhlbi.nih.gov", "02e23f52-f354-4e8b-992c-d37c8b9ba140", "BDC Authorized",    TRUE,  TRUE),
    BDC_PREDEV_OPEN       = mk("BDC_PREDEV_OPEN",       "https://predev.picsure.biodatacatalyst.nhlbi.nih.gov", "ac004461-1b47-4832-80e2-22a4aecabe39", "BDC Open",          FALSE, FALSE),
    NHANES_AUTHORIZED     = mk("NHANES_AUTHORIZED",     "https://nhanes.hms.harvard.edu/",                      "ded89b08-faa9-435c-b7c4-55b81922ee5f", "Nhanes Authorized", FALSE, TRUE),
    NHANES_OPEN           = mk("NHANES_OPEN",           "https://nhanes.hms.harvard.edu/",                      "ded89b08-faa9-435c-b7c4-55b81922ee5f", "Nhanes Open",       FALSE, FALSE)
  )
})

#' @export
print.picsure_platform <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  cat("  url:              ", x$url,              "\n", sep = "")
  cat("  resource_uuid:    ", x$resource_uuid,    "\n", sep = "")
  cat("  label:            ", x$label,            "\n", sep = "")
  cat("  include_consents: ", x$include_consents, "\n", sep = "")
  cat("  requires_auth:    ", x$requires_auth,    "\n", sep = "")
  invisible(x)
}
