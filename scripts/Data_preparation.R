library(tidyverse)
simName <- "4_5_A"
simDataPath <- "data/"


sim_parameters <- read_delim(paste0(simDataPath, simName, "_parameters.csv"),
                             delim = ",", quote = "'")
sim_results <- read_csv(paste0(simDataPath, simName, "_results_global.csv"))

# On ne garde que les simulations complètes

goodSeeds <- sim_parameters %>%
  left_join(sim_results %>%
              filter(Annee == 1160) %>%
              select(seed, Annee, sim_name),
            by = c("seed", "sim_name")) %>%
  filter(Annee == 1160) %>%
  select(seed, sim_name)

sim_parameters <- sim_parameters %>%
  semi_join(goodSeeds, by = c("seed", "sim_name")) %>%
  mutate(serfs_mobiles = if_else(serfs_mobiles == "true", TRUE, FALSE))

sim_results <- sim_results %>%
  semi_join(goodSeeds, by = c("seed", "sim_name"))

sim_seigneurs <-  read_delim(paste0(simDataPath, simName, "_results_seigneurs.csv"),
             delim = ",", quote = "'", na = "nil") %>%
  semi_join(goodSeeds, by = c("seed", "sim_name")) %>%
  mutate(initial = if_else(initial == "true", TRUE, FALSE)) %>%
  mutate(ID_seigneur = as.integer(stringr::str_extract(self, pattern = "[0-9]+"))) %>%
  mutate(monAgregat = as.integer(stringr::str_extract(monAgregat, pattern = "[0-9]+"))) %>%
  select(ID_seigneur, everything()) %>%
  select(-self, -geom)
  

sim_agregats <- read_delim(paste0(simDataPath, simName, "_results_agregats.csv"),
                           delim = ",", quote = "'", na = "nil") %>%
  semi_join(goodSeeds, by = c("seed", "sim_name")) %>%
  mutate(communaute = if_else(communaute == "true", TRUE, FALSE)) %>%
  mutate(ID_agregat = as.integer(stringr::str_extract(self, pattern = "[0-9]+"))) %>%
  mutate(monPole = as.integer(stringr::str_extract(monPole, pattern = "[0-9]+"))) %>%
  select(ID_agregat, everything()) %>%
  select(-self, -geom)

sim_poles <- read_delim(paste0(simDataPath, simName, "_results_poles.csv"),
                        delim = ",", quote = "'",  na = "nil") %>%
  semi_join(goodSeeds, by = c("seed", "sim_name")) %>%
  mutate(ID_pole = as.integer(stringr::str_extract(self, pattern = "[0-9]+"))) %>%
  select(ID_pole, everything()) %>%
  select(-geom, -self)

sim_FP <- read_delim(paste0(simDataPath, simName, "_results_FP.csv"),
                     delim = ",", quote = "'", na = "nil") %>%
  semi_join(goodSeeds, by = c("seed", "sim_name")) %>%
  mutate(communaute = if_else(communaute == "true", TRUE, FALSE)) %>%
  mutate(mobile = if_else(mobile == "true", TRUE, FALSE)) %>%
  mutate(ID_FP = as.integer(stringr::str_extract(self, pattern = "[0-9]+"))) %>%
  mutate(monAgregat = as.integer(stringr::str_extract(monAgregat, pattern = "[0-9]+"))) %>%
  select(ID_FP, everything()) %>%
  select(-geom, -self)
  

sim_paroisses <- read_delim(paste0(simDataPath, simName, "_results_paroisses.csv"),
                            delim = ",", quote = "'") %>%
  semi_join(goodSeeds, by = c("seed", "sim_name")) %>%
  mutate(ID_paroisse = as.integer(stringr::str_extract(self, pattern = "[0-9]+"))) %>%
  mutate(monEglise = as.integer(stringr::str_extract(monEglise, pattern = "[0-9]+"))) %>%
  select(ID_paroisse, everything()) %>%
  select(-geom, -self)

sim_results <- sim_results %>%
  inner_join({
    sim_agregats %>%
      group_by(seed, Annee) %>%
      summarise(NbAgregats = n())
  }, by = c("seed","Annee")
  ) %>%
  inner_join({
    filter(., Annee == 840) %>%
      mutate(CFinit = charge_fiscale) %>%
      select(seed, CFinit, sim_name)
  }, by = c("seed", "sim_name")) %>%
  mutate(RatioChargeFiscale = charge_fiscale / CFinit) %>%
  select(-CFinit)


#### Add to current data ####
library(tidyverse)
library(dbplyr)
library(DBI)
library(RSQLite)

con <- DBI::dbConnect(RSQLite::SQLite(), "data/outputs_TR8_indexSimName.sqlite")

dbWriteTable(conn = con, value = goodSeeds, name = "goodSeeds", append = TRUE, row.names = FALSE)
dbWriteTable(conn = con, value = sim_agregats, name = "agregats", append = TRUE, row.names = FALSE)
dbWriteTable(conn = con, value = sim_FP, name = "fp",   append = TRUE, row.names = FALSE)
dbWriteTable(conn = con, value = sim_parameters, name = "parameters",   append = TRUE, row.names = FALSE)
dbWriteTable(conn = con, value = sim_paroisses, name = "paroisses",   append = TRUE, row.names = FALSE)
dbWriteTable(conn = con, value = sim_poles, name = "poles",   append = TRUE, row.names = FALSE)
dbWriteTable(conn = con, value = sim_results, name = "results",   append = TRUE, row.names = FALSE)
dbWriteTable(conn = con, value = sim_seigneurs, name = "seigneurs",   append = TRUE, row.names = FALSE)


# system.time({
#   con <- DBI::dbConnect(RSQLite::SQLite(), "data/outputs_TR8.sqlite")
#   sim_FP_db <- tbl(con, "fp")
#   sim_FP_db %>% filter(sim_name %in% "4_4_B") %>% collect() -> blob1
#   DBI::dbDisconnect(con)
#   print(blob1 %>% count())
# })
# rm(blob1)
# 
# system.time({
#   con <- DBI::dbConnect(RSQLite::SQLite(), "data/outputs_TR8_indexSimName.sqlite")
#   sim_FP_db <- tbl(con, "fp")
#   sim_FP_db %>% filter(sim_name %in% "4_4_B") %>% collect() -> blob2
#   DBI::dbDisconnect(con)
#   print(blob2 %>% count())
# })
# rm(blob2)

dbGetQuery(con,"CREATE INDEX index_simname ON fp (sim_name)")
DBI::dbDisconnect(con)

# 
# # Single index with all
# con3 <- DBI::dbConnect(RSQLite::SQLite(), "D:/ouputs_TR8/outputs_TR8_3.sqlite")
# dbGetQuery(con3,"CREATE INDEX index_seed ON fp (seed, sim_name, Annee)")
# DBI::dbDisconnect(con3)
# 
# # Index on each
# con4 <- DBI::dbConnect(RSQLite::SQLite(), "D:/ouputs_TR8/outputs_TR8_4.sqlite")
# dbGetQuery(con4,"CREATE INDEX index_seed ON fp (seed)")
# dbGetQuery(con4,"CREATE INDEX index_name ON fp (sim_name)")
# dbGetQuery(con4,"CREATE INDEX index_annee ON fp (Annee)")
# DBI::dbDisconnect(con4)
# 
# 
# # Benchmarks
# 
# con3 <- DBI::dbConnect(RSQLite::SQLite(), "D:/ouputs_TR8/outputs_TR8_3.sqlite")
# system.time({
#   sim_FP_db3 <- tbl(con3, "fp")
#   sim_FP_db3 %>% group_by(seed, Annee) %>% summarise(N = n()) %>% collect() -> blob
# })
# DBI::dbDisconnect(con3)
# 
# con4 <- DBI::dbConnect(RSQLite::SQLite(), "D:/ouputs_TR8/outputs_TR8_4.sqlite")
# system.time({
#   sim_FP_db4 <- tbl(con4, "fp")
#   sim_FP_db4 %>% group_by(seed, Annee) %>% summarise(N = n()) %>% collect() -> foo
# })
# DBI::dbDisconnect(con4)
# 
# # Benchmarks2
# 
# con2 <- DBI::dbConnect(RSQLite::SQLite(), "D:/ouputs_TR8/outputs_TR8.sqlite")
# system.time({
#   sim_FP_db2 <- tbl(con2, "fp")
#   sim_FP_db2 %>% filter(Annee == 1160) %>% group_by(sim_name) %>% summarise(N = n()) %>% collect() -> foobar
# })
# DBI::dbDisconnect(con2)
# 
# con3 <- DBI::dbConnect(RSQLite::SQLite(), "D:/ouputs_TR8/outputs_TR8_3.sqlite")
# system.time({
#   sim_FP_db3 <- tbl(con3, "fp")
#   sim_FP_db3 %>% filter(Annee == 1160) %>% group_by(sim_name) %>% summarise(N = n()) %>% collect() -> blob
# })
# DBI::dbDisconnect(con3)
# 
# con4 <- DBI::dbConnect(RSQLite::SQLite(), "D:/ouputs_TR8/outputs_TR8_4.sqlite")
# system.time({
#   sim_FP_db4 <- tbl(con4, "fp")
#   sim_FP_db4 %>% filter(Annee == 1160) %>% group_by(sim_name) %>% summarise(N = n()) %>% collect() -> foo
# })
# DBI::dbDisconnect(con4)
# 
# 
# 
# 
# # Benchmarks3
# 
# 
# con3 <- DBI::dbConnect(RSQLite::SQLite(), "D:/ouputs_TR8/outputs_TR8_3.sqlite")
# system.time({
#   sim_FP_db3 <- tbl(con3, "fp")
#   
#   nombre_FP_total3 <- sim_FP_db3 %>%
#     group_by(seed, sim_name, Annee) %>%
#     summarise(N_total = n())
#   
#   types_deplacements3 <- sim_FP_db3 %>%
#     filter(type_deplacement != "nil") %>%
#     filter(type_deplacement != "Non mobile") %>%
#     group_by(Annee, seed, sim_name, type_deplacement) %>%
#     summarise(N = n()) %>%
#     left_join(nombre_FP_total3, by = c("seed", "Annee", "sim_name")) %>%
#     mutate(Tx = N / (N_total * 1.0)) %>%
#     group_by(Annee, type_deplacement) %>%
#     collect()
# })
# DBI::dbDisconnect(con3)
# 
# con4 <- DBI::dbConnect(RSQLite::SQLite(), "D:/ouputs_TR8/outputs_TR8_4.sqlite")
# system.time({
#   sim_FP_db4 <- tbl(con4, "fp")
#   
#   nombre_FP_total4 <- sim_FP_db4 %>%
#     group_by(seed, sim_name, Annee) %>%
#     summarise(N_total = n())
#   
#   types_deplacements4 <- sim_FP_db4 %>%
#     filter(type_deplacement != "nil") %>%
#     filter(type_deplacement != "Non mobile") %>%
#     group_by(Annee, seed, sim_name, type_deplacement) %>%
#     summarise(N = n()) %>%
#     left_join(nombre_FP_total4, by = c("seed", "Annee", "sim_name")) %>%
#     mutate(Tx = N / (N_total * 1.0)) %>%
#     group_by(Annee, type_deplacement) %>%
#     collect()
# })
# DBI::dbDisconnect(con4)
# 
# ### Disconnect all
#   
# DBI::dbDisconnect(con)
# DBI::dbDisconnect(con2)
# DBI::dbDisconnect(con3)
# DBI::dbDisconnect(con4)
