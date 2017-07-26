library(tidyverse)
library(dbplyr)
library(DBI)
library(RSQLite)
library(MonetDBLite)

# 1 - On lit la DB SQLite et on prépare les variables pour MonetDB

conSQLite <- DBI::dbConnect(RSQLite::SQLite(), "~/outputs_TR8/outputs_TR8_indexSimName.sqlite")

seeds <- tbl(conSQLite, "goodSeeds") %>%
  collect()

agregats <- tbl(conSQLite, "agregats") %>%
  collect() %>%
  rename_all(funs(tolower(.)))

fp <- tbl(conSQLite, "fp") %>%
  collect() %>%
  rename_all(funs(tolower(.)))

parameters <- tbl(conSQLite, "parameters") %>%
  collect() %>%
  rename_all(funs(tolower(.)))

paroisses <- tbl(conSQLite, "paroisses") %>%
  collect() %>%
  rename_all(funs(tolower(.))) %>%
  rename(area = shape.area)

poles <- tbl(conSQLite, "poles") %>%
  collect() %>%
  rename_all(funs(tolower(.)))

results <- tbl(conSQLite, "results") %>%
  collect() %>%
  rename_all(funs(tolower(.)))


seigneurs <- tbl(conSQLite, "seigneurs") %>%
  collect() %>%
  rename_all(funs(tolower(.)))


DBI::dbDisconnect(conSQLite)

# 2 - On remplit la BDD MonetDBLite

# conMonetDB <- dbConnect(MonetDBLite::MonetDBLite(), "~/outputs_TR8/testMonetDB.db")

conMonetDB <- MonetDBLite::src_monetdblite("~/outputs_TR8/testMonetDB")

copy_to(conMonetDB, seeds, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name"))
copy_to(conMonetDB, agregats, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name", "annee"))
copy_to(conMonetDB, fp, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name", "annee"))
copy_to(conMonetDB, parameters, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name"))
copy_to(conMonetDB, paroisses, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name", "annee"))
copy_to(conMonetDB, poles, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name", "annee"))
copy_to(conMonetDB, results, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name", "annee"))
copy_to(conMonetDB, seigneurs, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name", "annee"))
# 
# dbWriteTable(conn = conMonetDB, value = seeds, name = "seeds", overwrite = TRUE, row.names = FALSE)
# dbWriteTable(conn = conMonetDB, value = agregats, name = "agregats", overwrite = TRUE, row.names = FALSE)
# dbWriteTable(conn = conMonetDB, value = fp, name = "fp", overwrite = TRUE, row.names = FALSE)
# dbWriteTable(conn = conMonetDB, value = parameters, name = "parameters", overwrite = TRUE, row.names = FALSE)
# dbWriteTable(conn = conMonetDB, value = paroisses, name = "paroisses", overwrite = TRUE, row.names = FALSE)
# dbWriteTable(conn = conMonetDB, value = poles, name = "poles", overwrite = TRUE, row.names = FALSE)
# dbWriteTable(conn = conMonetDB, value = results, name = "results", overwrite = TRUE, row.names = FALSE)
# dbWriteTable(conn = conMonetDB, value = seigneurs, name = "seigneurs", overwrite = TRUE, row.names = FALSE)

DBI::dbDisconnect(conMonetDB)
MonetDBLite::monetdblite_shutdown()
