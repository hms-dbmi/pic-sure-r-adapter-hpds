# Public enum constants mirroring the Python adapter's enums.
#
# Each member is an S3 list with class c(<subclass>, "picsure_enum_member").
# Subclasses (picsure_clause_type, picsure_group_operator, picsure_query_type,
# picsure_platform) reserve room for per-enum dispatch; today only
# picsure_platform uses it (richer print).
#
# Member objects are accepted as inputs by createClause(), buildClauseGroup(),
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

#' Filter clause types.
#'
#' Pass a member to [`createClause()`][picsure::createClause]'s `type`
#' argument. Mirrors Python's `picsure.ClauseType`.
#'
#' @format A list of `picsure_enum_member` objects:
#' \describe{
#'   \item{`FILTER`}{Filter by categorical values or numeric range.}
#'   \item{`ANYRECORD`}{Match records where the concept path *or any
#'     descendant* has a value (wire: `ANY_RECORD_OF`).}
#'   \item{`SELECT`}{Include the concept path(s) in query output.}
#'   \item{`REQUIRE`}{Require the concept path to have a non-null value
#'     (wire: `REQUIRED`).}
#' }
#' @examples
#' \dontrun{
#' picsure::createClause(
#'   "\\phs1\\pht1\\phv1\\sex\\",
#'   type = picsure::ClauseType$FILTER,
#'   categories = "male"
#' )
#' }
#' @export
ClauseType <- list(
  FILTER    = .enum_member("FILTER",    "filter",    enum_name = "ClauseType", subclass = "picsure_clause_type"),
  ANYRECORD = .enum_member("ANYRECORD", "anyrecord", enum_name = "ClauseType", subclass = "picsure_clause_type"),
  SELECT    = .enum_member("SELECT",    "select",    enum_name = "ClauseType", subclass = "picsure_clause_type"),
  REQUIRE   = .enum_member("REQUIRE",   "require",   enum_name = "ClauseType", subclass = "picsure_clause_type")
)

#' Logical operators for combining clauses in a group.
#'
#' Pass a member to [`buildClauseGroup()`][picsure::buildClauseGroup]'s
#' `root` argument. Mirrors Python's `picsure.GroupOperator`.
#'
#' @format A list of `picsure_enum_member` objects:
#' \describe{
#'   \item{`AND`}{All clauses must match.}
#'   \item{`OR`}{At least one clause must match.}
#' }
#' @examples
#' \dontrun{
#' picsure::buildClauseGroup(
#'   list(c1, c2),
#'   root = picsure::GroupOperator$AND
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
#' }
#' @examples
#' \dontrun{
#' picsure::runQuery(session, query, type = picsure::QueryType$COUNT)
#' }
#' @export
QueryType <- list(
  COUNT       = .enum_member("COUNT",       "count",       enum_name = "QueryType", subclass = "picsure_query_type"),
  PARTICIPANT = .enum_member("PARTICIPANT", "participant", enum_name = "QueryType", subclass = "picsure_query_type"),
  TIMESTAMP   = .enum_member("TIMESTAMP",   "timestamp",   enum_name = "QueryType", subclass = "picsure_query_type"),
  CROSS_COUNT = .enum_member("CROSS_COUNT", "cross_count", enum_name = "QueryType", subclass = "picsure_query_type")
)
