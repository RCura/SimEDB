library(tidyverse)
library(dbplyr)
library(DBI)
library(RSQLite)
library(RPostgreSQL)



# 1 - On lit la DB SQLite et on prépare les variables pour PostgreSQL

system.time({
  conSQLite <- DBI::dbConnect(RSQLite::SQLite(), "data/outputs_TR8_indexSimName.sqlite")
  
  seeds <- tbl(conSQLite, "goodSeeds") %>%
    collect()
  
  agregats <- tbl(conSQLite, "agregats") %>%
    collect() %>%
    rename_all(funs(tolower(.)))
  
  fp2<- tbl(conSQLite, "fp") %>%
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
  
})

# 2 - On remplit la BDD PostgreSQL

system.time({
  conPgSQL <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), dbname = "robin")
  
  DBI::dbWriteTable(conPgSQL, name = "seeds", value = seeds, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name"))
  DBI::dbWriteTable(conPgSQL, name = "agregats", value = agregats, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name", "annee"))
  DBI::dbWriteTable(conPgSQL, name = "fp", value = fp, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name", "annee"))
  DBI::dbWriteTable(conPgSQL, name = "parameters", value = parameters, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name"))
  DBI::dbWriteTable(conPgSQL, name = "paroisses", value = paroisses, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name", "annee"))
  DBI::dbWriteTable(conPgSQL, name = "poles", value = poles, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name", "annee"))
  DBI::dbWriteTable(conPgSQL, name = "results", value = results, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name", "annee"))
  DBI::dbWriteTable(conPgSQL, name = "seigneurs", value = seigneurs, temporary = FALSE, overwrite = TRUE, indexes = list("seed", "sim_name", "annee"))
  
  dbListTables(conPgSQL)
  dbDisconnect(conPgSQL)
})
