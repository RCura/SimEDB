suppressPackageStartupMessages({
  
  # Interactivity
  library(shiny)
  library(shinythemes)
  library(parcoords) # devtools::install_github("timelyportfolio/parcoords", ref="feature/resize")
  
  # Data wrangling
  library(tidyverse)
  library(magrittr)
  library(stringr)
  library(forcats)
  
  
  # Plots
  library(gridExtra)
  library(ggthemes)
  
  # Tables
  library(xtable)
  library(formattable) # devtools::install_github("renkun-ken/formattable")
  
  # Spatial
  #library(sp)
  library(ShinyRatingInput) # devtools::install_github("stefanwilhelm/ShinyRatingInput")
  
  # DataBase
  library(dbplyr)
  library(DBI)
  library(RSQLite)
  
})


con <- DBI::dbConnect(RSQLite::SQLite(), "~/outputs_TR8/outputs_TR8.sqlite")

seeds <- tbl(con, "goodSeeds") %>%
agregats <- tbl(con, "agregats")
fp <- tbl(con, "fp")
parameters <- tbl(con, "parameters")
paroisses <- tbl(con, "paroisses")
poles <- tbl(con, "poles")
results <- tbl(con, "results")
seigneurs <- tbl(con, "seigneurs")

DBI::dbDisconnect(conSQLite)



# Results #


# FP #
FP_data <- fp

system.time(FP_TypeDeplacements(FP_data) -> foo1)
system.time(FP_DeplacementsDetail(FP_data) -> foo2)
system.time(FP_Concentration(results_data) -> foo3)
system.time(FP_Satisfaction(FP_data) -> foo4)

system.time({
blob <- FP_data %>%
  select(Annee, sMat, sRel, sProt, Satis) %>%
  group_by(Annee) %>%
  collect() %>%
  sample_n(size = 4E3, replace = FALSE) %>%
  ungroup()
})

dbGetQuery(con,"select * from
(select distinct Annee from fp) t1, fp t2
where t2.ID_FP in (select ID_FP from fp t3 where t3.Annee=t1.Annee limit 10);")




# Agregats #
agregats_data <- agregats
system.time(Agregats_Nb(agregats_data) -> bar1)
system.time(Agregats_Poles(agregats_data) -> bar2)
system.time(Agregats_CA(agregats_data) -> bar3)
system.time(Agregats_RT(agregats_data) -> bar4)

# Seigneurs #


# Poles #


# Paroisses #










# DataBase
library(tidyverse)
library(dbplyr)
library(DBI)
library(RSQLite)
library(MonetDBLite)

conSQLite <- DBI::dbConnect(RSQLite::SQLite(), "~/outputs_TR8/outputs_TR8.sqlite")

seeds <- tbl(conSQLite, "goodseeds") %>%
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
conMonetDB <- dbConnect(MonetDBLite::MonetDBLite(), "~/outputs_TR8/testMonetDB.db")

dbWriteTable(conn = conMonetDB, value = seeds, name = "seeds", overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn = conMonetDB, value = agregats, name = "agregats", overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn = conMonetDB, value = fp, name = "fp", overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn = conMonetDB, value = parameters, name = "parameters", overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn = conMonetDB, value = paroisses, name = "paroisses", overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn = conMonetDB, value = poles, name = "poles", overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn = conMonetDB, value = results, name = "results", overwrite = TRUE, row.names = FALSE)
dbWriteTable(conn = conMonetDB, value = seigneurs, name = "seigneurs", overwrite = TRUE, row.names = FALSE)

DBI::dbDisconnect(conMonetDB)

system.time({
  conSQLite <- DBI::dbConnect(RSQLite::SQLite(), "~/outputs_TR8/outputs_TR8.sqlite")
  fpSQL <- tbl(conSQLite, "fp") %>% collect()
  DBI::dbDisconnect(conSQLite)
})

rm(fpSQL)

system.time({
conMonetDB <- dbConnect(MonetDBLite::MonetDBLite(), "~/outputs_TR8/testMonetDB.db")
fpMonet <- tbl(src = conMonetDB, "fp") %>% collect()
DBI::dbDisconnect(conMonetDB)
})

rm(fpMonet)


system.time({
  conSQLite <- DBI::dbConnect(RSQLite::SQLite(), "~/outputs_TR8/outputs_TR8.sqlite")
  fpSQL <- tbl(conSQLite, "fp") %>% filter(sim_name == "4_4_D") %>% collect()
  DBI::dbDisconnect(conSQLite)
})

rm(fpSQL)

system.time({
  conMonetDB <- dbConnect(MonetDBLite::MonetDBLite(), "~/outputs_TR8/testMonetDB.db")
  fpMonet <- tbl(src = conMonetDB, "fp") %>% filter(sim_name == "4_4_D") %>% collect()
  DBI::dbDisconnect(conMonetDB)
})

rm(fpMonet)


conMonetDB <- dbConnect(MonetDBLite::MonetDBLite(), "~/outputs_TR8/testMonetDB.db")
fpMonet <- tbl(src = conMonetDB, "fp") %>% filter(sim_name == "4_4_B")

test <- fpMonet

DBI::dbDisconnect(conMonetDB)


MonetDBLite::monetdblite_shutdown()


library(tidyverse)
library(dbplyr)
library(DBI)
library(MonetDBLite)


# Create example dataset
rep_data <- tibble(val = runif(n = 1E6), grp = if_else(val < .5, "A", "B"))
# Copy this dataset to MonetDBLite
con <- dbConnect(MonetDBLite::MonetDBLite(), "testData")
dbWriteTable(conn = con, rep_data, name = "rep_data", overwrite = TRUE)

# Query this base
thisTable <- tbl(src = con, "rep_data")
class(thisTable) <- c(class(thisTable), "tbl_monetdb")
class(thisTable)
thisTable %>% group_by(grp) %>% sample_n(size = 10) %>% show_query()

dbGetQuery(con, 'select * FROM rep_data SAMPLE 10 GROUP BY grp ')


DBI::dbDisconnect(con)

dbGetQuery(con, 'select "grp", COUNT(*) AS "N" FROM rep_data GROUP BY grp')

dbGetQuery(con,'SELECT "grp", COUNT() AS "N" FROM "rep_data"GROUP BY "grp"')



DBI::dbDisconnect(con)
MonetDBLite::monetdblite_shutdown()


DBI::dbDisconnect(conMonetDB)
