# Quick and dirty script file to test functionality (run manually)

library(picsure)
library(hpds)


# Connect to the PIC-SURE network
myendpoint <- "https://copdgene-dev.hms.harvard.edu/picsure/"
mytoken <- "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJnb29nbGUtb2F1dGgyfDEwMTY1ODI0NDM2OTk1NjM3ODQzOSIsInVzZXJfaWQiOiJnb29nbGUtb2F1dGgyfDEwMTY1ODI0NDM2OTk1NjM3ODQzOSIsIm5hbWUiOiJOaWNrIEJlbmlrIiwiZXhwIjoxNTcxODc1OTI3LCJpYXQiOjE1NzE4NzIzMjcsImVtYWlsIjoibmJlbmlrQGdtYWlsLmNvbSJ9.Jq0IU_-FLTwCiYRT7ABDhnJdF-W0GGVnTUfYMu8W2Ts"
myconn <- picsure::connect(url=myendpoint, token=mytoken)


# List the resources and connect to the first one (assumes that the resource will be HPDS-based)
lstResources <- picsure::list.resources(myconn)
myResourceUUID <- lstResources[[1]]
myres <- hpds::get.resource(connection=myconn, resourceUUID=myResourceUUID)


# Query the data dictionary for "asthma"
dictResults <- hpds::find.in.dictionary(resource=myres, term="asthma")


# test the various dictionary results functions
hpds::extract.count(dictionary.results=dictResults)
hpds::extract.keys(dictionary.results=dictResults)
hpds::extract.entries(dictionary.results=dictResults)
hpds::extract.dataframe(dictionary.results=dictResults)


# create a blank query
myquery <- hpds::new.query(resource=myres)


# SELECT: test add and delete
hpds::query.select.add(query=myquery, keys="select-test-key")
hpds::query.show(query=myquery)
hpds::query.select.delete(query=myquery, keys="select-test-key")
hpds::query.show(query=myquery)


# REQUIRE: test add and delete
hpds::query.require.add(query=myquery, keys="require-test-key")
hpds::query.show(query=myquery)
hpds::query.require.delete(query=myquery, keys="require-test-key")
hpds::query.show(query=myquery)


# ANYOF: test add and delete
hpds::query.anyof.add(query=myquery, keys="anyof-test-key")
hpds::query.show(query=myquery)
hpds::query.anyof.delete(query=myquery, keys="anyof-test-key")
hpds::query.show(query=myquery)


# CROSSCOUNTS: test add and delete
hpds::query.crosscounts.add(query=myquery, keys="crosscounts-test-key")
hpds::query.show(query=myquery)
hpds::query.crosscounts.delete(query=myquery, keys="crosscounts-test-key")
hpds::query.show(query=myquery)


# FILTER: test add and delete for various formats
# --- filter by key name !!! THIS DOES NOTHING !!!
#hpds::query.filter.add(query=myquery, keys="filter-test-key")
#hpds::query.show(query=myquery)
#hpds::query.filter.delete(query=myquery, keys="filter-test-key")
#hpds::query.show(query=myquery)
# --- filter by category
hpds::query.filter.add(query=myquery, keys="filter-test-key", c("categoryName"))
hpds::query.show(query=myquery)
hpds::query.filter.delete(query=myquery, keys="filter-test-key")
hpds::query.show(query=myquery)
# --- filter by single value
hpds::query.filter.add(query=myquery, keys="filter-test-key", "42")
hpds::query.show(query=myquery)
hpds::query.filter.delete(query=myquery, keys="filter-test-key")
hpds::query.show(query=myquery)
# --- filter by range
hpds::query.filter.add(query=myquery, keys="filter-test-key", min="0", max="100")
hpds::query.show(query=myquery)
hpds::query.filter.delete(query=myquery, keys="filter-test-key")
hpds::query.show(query=myquery)




