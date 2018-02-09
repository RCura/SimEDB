library(tidyverse)
library(dbplyr)
library(DBI)
library(RSQLite)

conSQLite <- DBI::dbConnect(RSQLite::SQLite(), "data/outputs_TR8_indexSimName.sqlite")

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

sim_names <- seeds %>%
  distinct(sim_name) %>%
  pull()

for (thisSim in sim_names){
  output_folder <- sprintf("data/%s/", thisSim)
  if (!dir.exists(output_folder)){
    dir.create(output_folder)
  }
  
  seeds %>%
    filter(sim_name == thisSim) %>%
    write_csv(path = bzfile(paste0(output_folder, "seeds.csv.bz2")))
  
  agregats %>%
    filter(sim_name == thisSim) %>%
    write_csv(path = bzfile(paste0(output_folder, "agregats.csv.bz2")))
  
  fp %>%
    filter(sim_name == thisSim) %>%
    write_csv(path = bzfile(paste0(output_folder, "fp.csv.bz2")))
  
  parameters %>%
    filter(sim_name == thisSim) %>%
    write_csv(path = bzfile(paste0(output_folder, "parameters.csv.bz2")))
  
  paroisses %>%
    filter(sim_name == thisSim) %>%
    write_csv(path = bzfile(paste0(output_folder, "paroisses.csv.bz2")))
  
  poles %>%
    filter(sim_name == thisSim) %>%
    write_csv(path = bzfile(paste0(output_folder, "poles.csv.bz2")))
  
  results %>%
    filter(sim_name == thisSim) %>%
    write_csv(path = bzfile(paste0(output_folder, "results.csv.bz2")))
  
  seigneurs %>%
    filter(sim_name == thisSim) %>%
    write_csv(path = bzfile(paste0(output_folder, "seigneurs.csv.bz2")))
}

blob <- read_csv("data/4_5_A/fp.csv.bz2",
                  col_types =   cols(
                    smat = col_double(),
                    srel = col_double(),
                    sprot = col_double()
                  ))
