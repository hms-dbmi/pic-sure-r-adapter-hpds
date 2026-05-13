# Package index

## Connection

- [`connect()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/connect.md)
  : Connect to a PIC-SURE instance.
- [`platforms()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/platforms.md)
  : List available PIC-SURE platforms.

## Search

- [`dictionarySearch()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/dictionarySearch.md)
  : Search the PIC-SURE data dictionary.
- [`facets()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/facets.md)
  : Build a FacetSet for narrowing search results.
- [`addFacet()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/addFacet.md)
  : Add an entry to a FacetSet.
- [`removeFacet()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/removeFacet.md)
  : Remove an entry from a FacetSet.

## Query construction

- [`createClause()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/createClause.md)
  : Create a single query clause.
- [`buildClauseGroup()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/buildClauseGroup.md)
  : Combine clauses (and nested groups) under an AND or OR operator.

## Query execution

- [`runQuery()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/runQuery.md)
  : Execute a query against a PIC-SURE session.
- [`runQueryByID()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/runQueryByID.md)
  : Load a saved PIC-SURE query by ID and execute it in one call.
- [`loadQueryByID()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/loadQueryByID.md)
  : Load a previously-saved PIC-SURE query by its query ID.

## Export

- [`exportAsPFB()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/exportAsPFB.md)
  : Export query results to a PFB file.
- [`exportCSV()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/exportCSV.md)
  : Write a participant data frame to a CSV file.
- [`exportTSV()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/exportTSV.md)
  : Write a participant data frame to a TSV file.

## Errors

- [`picsureError()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/picsureError.md)
  : Construct a picsureError condition.

## Enums

Public enum constants mirroring the Python adapter’s enums. Pass a
member to the wrapper functions instead of a string for autocomplete and
parity with the Python API.

- [`ClauseType`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/ClauseType.md)
  : Filter clause types.
- [`GroupOperator`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/GroupOperator.md)
  : Logical operators for combining clauses in a group.
- [`QueryType`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/QueryType.md)
  : Query result types for \`runQuery()\`.
- [`Platform`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/Platform.md)
  : Known PIC-SURE deployment platforms.
