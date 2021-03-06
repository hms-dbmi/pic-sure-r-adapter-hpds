# Quick and dirty script file to test functionality (run manually)

library(picsure)
library(hpds)

# Connect to the PIC-SURE network
myendpoint <- "https://copdgene-dev.hms.harvard.edu/picsure/"
mytoken <- "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJnb29nbGUtb2F1dGgyfDEwMTY1ODI0NDM2OTk1NjM3ODQzOSIsInVzZXJfaWQiOiJnb29nbGUtb2F1dGgyfDEwMTY1ODI0NDM2OTk1NjM3ODQzOSIsIm5hbWUiOiJOaWNrIEJlbmlrIiwiZXhwIjoxNTczMjMxMjkwLCJpYXQiOjE1NzMyMjc2OTAsImVtYWlsIjoibmJlbmlrQGdtYWlsLmNvbSJ9.9A_qGFyU50IW5o7aw6-PJMy4VTr8XE789h98dif8XJk"
myconn <- picsure::connect(url=myendpoint, token=mytoken)

# List the resources and connect to the first one (assumes that the resource will be HPDS-based)
lstResources <- picsure::list.resources(myconn)
myResourceUUID <- lstResources[[1]]
myres <- hpds::get.resource(connection=myconn, resourceUUID=myResourceUUID)

# ===== LOCAL CONNECTION (via BypassAdapter) =====
myconn <- hpds::BypassAdapter$new(url="http://localhost:8080/PIC-SURE")
myres <- myconn$useResource();
class(myres) <- "Hpds_Resource"


# create a blank query
myquery <- hpds::new.query(resource=myres)


# Query the data dictionary for "asthma"
dictResults <- hpds::find.in.dictionary(resource=myres, term="asthma")


# test the various dictionary results functions
hpds::extract.count(dictionary.results=dictResults)
hpds::extract.keys(dictionary.results=dictResults)
hpds::extract.entries(dictionary.results=dictResults)
hpds::extract.dataframe(dictionary.results=dictResults)




# SELECT: test add and delete
keySelect = "\\03 Clinical data\\Respiratory disease form\\04 Respiratory Conditions\\01 Asthma\\01 Have you ever had asthma\\"
hpds::query.select.add(query=myquery, keys=keySelect)
hpds::query.show(query=myquery)
hpds::query.select.delete(query=myquery, keys=keySelect)
hpds::query.show(query=myquery)


# REQUIRE: test add and delete
keyRequire = "\\03 Clinical data\\Respiratory disease form\\04 Respiratory Conditions\\01 Asthma\\01 Have you ever had asthma\\"
hpds::query.require.add(query=myquery, keys=keyRequire)
hpds::query.show(query=myquery)
hpds::query.require.delete(query=myquery, keys=keyRequire)
hpds::query.show(query=myquery)


# ANYOF: test add and delete
keyAnyof = "\\03 Clinical data\\Respiratory disease form\\04 Respiratory Conditions\\01 Asthma\\01 Have you ever had asthma\\"
hpds::query.anyof.add(query=myquery, keys=keyAnyof)
hpds::query.show(query=myquery)
hpds::query.anyof.delete(query=myquery, keys=keyAnyof)
hpds::query.show(query=myquery)


# CROSSCOUNTS: test add and delete
keyCrossCount = "\\03 Clinical data\\Respiratory disease form\\04 Respiratory Conditions\\01 Asthma\\01 Have you ever had asthma\\"
hpds::query.crosscounts.add(query=myquery, keys=keyCrossCount)
hpds::query.show(query=myquery)
hpds::query.crosscounts.delete(query=myquery, keys=keyCrossCount)
hpds::query.show(query=myquery)


# FILTER: test add and delete for various formats
# --- filter by key name !!! THIS DOES NOTHING !!!
#hpds::query.filter.add(query=myquery, keys="filter-test-key")
#hpds::query.show(query=myquery)
#hpds::query.filter.delete(query=myquery, keys="filter-test-key")
#hpds::query.show(query=myquery)
# --- filter by category
keyFilterCategory = "\\03 Clinical data\\Respiratory disease form\\04 Respiratory Conditions\\01 Asthma\\01 Have you ever had asthma\\"
keyFilterCategoryValue = "Yes"
hpds::query.filter.add(query=myquery, keys=keyFilterCategory, c(keyFilterCategoryValue))
hpds::query.show(query=myquery)
hpds::query.filter.delete(query=myquery, keys=keyFilterCategory)
hpds::query.show(query=myquery)
# --- filter by single value
keyFilterValue = "\\03 Clinical data\\Respiratory disease form\\04 Respiratory Conditions\\01 Asthma\\05a If you no longer have asthma at what age did it stop\\"
keyFilterValueValue = 35
hpds::query.filter.add(query=myquery, keys=keyFilterValue, keyFilterValueValue)
hpds::query.show(query=myquery)
hpds::query.filter.delete(query=myquery, keys=keyFilterValue)
hpds::query.show(query=myquery)
# --- filter by range
keyFilterRange = "\\03 Clinical data\\Respiratory disease form\\04 Respiratory Conditions\\01 Asthma\\05a If you no longer have asthma at what age did it stop\\"
keyFilterRangeMinValue = 30
keyFilterRangeMaxValue = 40
hpds::query.filter.add(query=myquery, keys=keyFilterRange, min=keyFilterRangeMinValue, max=keyFilterRangeMaxValue)
hpds::query.show(query=myquery)
hpds::query.filter.delete(query=myquery, keys=keyFilterRange)
hpds::query.show(query=myquery)


# Again but now running count queries
keyFilterCategory = "\\03 Clinical data\\Respiratory disease form\\04 Respiratory Conditions\\01 Asthma\\01 Have you ever had asthma\\"
keyFilterCategoryValue = "Yes"
hpds::query.filter.add(query=myquery, keys=keyFilterCategory, c(keyFilterCategoryValue))
hpds::query.show(query=myquery)
hpds::query.run(query=myquery, result.type="count")
hpds::query.filter.delete(query=myquery, keys=keyFilterCategory)
# --- filter by single value
keyFilterValue = "\\03 Clinical data\\Respiratory disease form\\04 Respiratory Conditions\\01 Asthma\\05a If you no longer have asthma at what age did it stop\\"
keyFilterValueValue = 35
hpds::query.filter.add(query=myquery, keys=keyFilterValue, keyFilterValueValue)
hpds::query.show(query=myquery)
hpds::query.run(query=myquery, result.type="count")
hpds::query.filter.delete(query=myquery, keys=keyFilterValue)
# --- filter by range
keyFilterRange = "\\01 Demographics\\Age at enrollment\\"
keyFilterRangeMinValue = 40
keyFilterRangeMaxValue = 50
hpds::query.filter.add(query=myquery, keys=keyFilterRange, min=keyFilterRangeMinValue, max=keyFilterRangeMaxValue)
hpds::query.show(query=myquery)
hpds::query.run(query=myquery, result.type="count")
hpds::query.filter.delete(query=myquery, keys=keyFilterRange)



# ========== VariantInfo queries ==========
# test categorical Variant Info query
keyFilterCategory = "TSD"
keyFilterCategoryValue = "GCTAAAGGGATTTT"
hpds::query.filter.add(query=myquery, keys=keyFilterCategory, c(keyFilterCategoryValue))
hpds::query.show(query=myquery)
hpds::query.run(query=myquery, result.type="count")
hpds::query.filter.delete(query=myquery, keys=keyFilterCategory)

# test numeric Variant Info query
keyFilterRange = "AF"
keyFilterRangeMinValue = 0
keyFilterRangeMaxValue = 0.5
hpds::query.filter.add(query=myquery, keys=keyFilterRange, min=keyFilterRangeMinValue, max=keyFilterRangeMaxValue)
hpds::query.show(query=myquery)
hpds::query.run(query=myquery, result.type="count")
hpds::query.filter.delete(query=myquery, keys=keyFilterRange)


# ========== VariantSpec queries ==========
# SELECT: test that VariantSpec cannot be entered (since it cannot be retrieved)
keySelect = "14,20103168,C,T"
hpds::query.select.add(query=myquery, keys=keySelect)
hpds::query.show(query=myquery)
# REQUIRE: test add and delete
keyRequire = "14,20103168,C,T"
hpds::query.require.add(query=myquery, keys=keyRequire)
hpds::query.show(query=myquery)
hpds::query.require.delete(query=myquery, keys=keyRequire)
hpds::query.show(query=myquery)
# ANYOF: test add and delete
keyAnyof = "14,20103168,C,T"
hpds::query.anyof.add(query=myquery, keys=keyAnyof)
hpds::query.show(query=myquery)
hpds::query.anyof.delete(query=myquery, keys=keyAnyof)
hpds::query.show(query=myquery)
# CROSSCOUNTS: test add and delete
keyCrossCount = "14,20103168,C,T"
hpds::query.crosscounts.add(query=myquery, keys=keyCrossCount)
hpds::query.show(query=myquery)
hpds::query.crosscounts.delete(query=myquery, keys=keyCrossCount)
hpds::query.show(query=myquery)
# FILTER: test add and delete
hpds::query.filter.add(query=myquery, keys="14,20103168,C,T", c("0/1","1/1","1/0"))
hpds::query.show(query=myquery)
hpds::query.run(query=myquery, result.type="count")
hpds::query.filter.delete(query=myquery, keys="14,20103168,C,T")






