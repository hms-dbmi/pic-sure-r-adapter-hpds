# PIC-SURE HPDS R Client

R client library to run queries against a PIC-SURE High Performance Data Store (HPDS) resource.

--- 

**Features**

- Connect to a PIC-SURE resource through R using your personal access token
- Create and modify queries in R to generate cohorts at the variable level
- Run queries in R to pull aggregate counts and patient / participant level data
- Load queries generated in the PIC-SURE UI into an R environment

---

**Dependencies**

R6, httr, jsonlite, stringr, hash, purrr

---

**Examples**

See the [Access-to-Data-using-PIC-SURE-API](https://github.com/hms-dbmi/Access-to-Data-using-PIC-SURE-API) repository for example notebooks demonstrating how to use the R client libraries to run queries in PIC-SURE.

---

**Functionality**

See the [package pdf documentation](https://github.com/hms-dbmi/pic-sure-r-adapter-hpds/blob/readme/picsure_1.1.1.pdf) for more details regarding package functions and their uses.

*Note: BioData Catalyst (BDC) users will find BDC specific functions prefaced by `bdc.`.*
