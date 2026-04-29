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
