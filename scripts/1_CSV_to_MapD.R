library(tidyverse)
library(stringr)
library(DBI)
library(RJDBC)

######################################
############# PARAMETRES #############
######################################

outputs_path <- "/home/robin/SimFeodal/outputs/"
setwd(outputs_path)

prefixe_files <- "5_0_Test_07_08"
sim_name <- "5_0_Test"
suffixe_tables <- "_4_5"
nb_replications_to_keep <- 20



options( java.parameters = c("-Xss2560k", "-Xmx8g") ) # Needed fix for rJava (JDBC) + ggplot2

conMapD <- NULL
connectToMapD <- function(){
  conMapD <<- dbConnect(drv = JDBC("com.mapd.jdbc.MapDDriver",
                                   "/home/robin/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
                                   identifier.quote="'"),
                        "jdbc:mapd:localhost:9091:mapd", "mapd", "HyperInteractive")
}

#####################################
############# ATTENTION #############
#####################################

# Utilisé pour créer les tables au départ
# Ne pas décommenter sans raison

# Creation et vidage des tables #

# connectToMapD()
# 
# thisTable <- "agregats"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                  sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                          thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "fp"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "parameters"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "paroisses"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "poles"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "results"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "seeds"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# thisTable <- "seigneurs"
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("CREATE TABLE %s%s AS SELECT * FROM %s_5 LIMIT 1;",
#                                      thisTable, suffixe_tables, thisTable))
# sqlQuery <- DBI::dbSendQuery(conn = conMapD,
#                              sprintf("TRUNCATE TABLE %s%s;",
#                                      thisTable, suffixe_tables))
# 
# dbDisconnect(conMapD)


########################################################
############# ON ISOLE LES SEEDS CORRECTES #############
########################################################

set.seed(2)
finished_seeds <- list.files(pattern = sprintf("%s_results_global.csv", prefixe_files)) %>%
  map(~read_csv(file = ., quote = "'")) %>%
  reduce(rbind)   %>%
  mutate(sim_name = !!sim_name) %>%
  filter(Annee == 1160) %>%
  mutate(seed = as.character(`string(seed)`)) %>%
  pull(seed)


params <- list.files(pattern = sprintf("%s_parameters.csv", prefixe_files)) %>%
  map(~read_csv(file = ., quote = "'")) %>%
  reduce(rbind)   %>%
  mutate(sim_name = !!sim_name) %>%
  mutate(seed = as.character(`string(seed)`)) %>%
  filter(seed %in% finished_seeds) %>%
  group_by_at(vars(-seed)) %>%
  mutate(unique_experiment = runif(n = 1)) %>%
  ungroup()

nb_experiments <- params %>%
  select(seed, unique_experiment) %>%
  group_by(unique_experiment) %>%
  nrow()
if (nb_experiments > nb_replications_to_keep){
  seeds_to_keep <- params %>%
    select(seed, unique_experiment) %>%
    group_by(unique_experiment) %>%
    sample_n(nb_replications_to_keep) %>%
    pull(seed)
} else {
  seeds_to_keep <- params %>%
    select(seed, unique_experiment) %>%
    group_by(unique_experiment) %>%
    pull(seed)
}

rm(finished_seeds)

#########################################################
############# ON NETTOIE ET ENVOI DANS MAPD #############
#########################################################

############# PARAMETERS #############

params <- list.files(pattern = sprintf("%s_parameters.csv", prefixe_files)) %>%
  map(~read_csv(file = ., quote = "'")) %>%
  reduce(rbind)   %>%
  rename(seed = `string(seed)`) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep) %>%
  mutate(sim_name = !!sim_name)
  

params_mapd <- params %>%
  rename_all(funs(tolower(.x))) %>%
  mutate(serfs_mobiles = if_else(serfs_mobiles == "true", 1L, 0L))

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_parameters.csv", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write.csv(params_mapd, fileToWrite, quote = TRUE, row.names = FALSE)

connectToMapD()
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY parameters%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(params, params_mapd, sqlQuery)


############# AGREGATS #############

results_agregats <- list.files(pattern =
                                 sprintf("%s_results_agregats.csv", prefixe_files)) %>%
  map(~read_csv(file = ., quote = "'")) %>%
  reduce(rbind)   %>%
  rename(seed = `string(seed)`) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep) %>%
  mutate(sim_name = !!sim_name)

agregats_mapd <- results_agregats %>%
  rename(id_agregat = self,
         annee = Annee,
         nbfp = nbFP,
         monpole = monPole) %>%
  select(id_agregat,
         seed,
         sim_name,
         annee,
         nbfp,
         superficie,
         communaute,
         monpole) %>%
  mutate(id_agregat = stringr::str_extract(id_agregat, pattern="\\d+"),
         id_agregat = as.integer(id_agregat),
         seed = as.character(seed),
         monpole = stringr::str_extract(monpole, pattern="\\d+"),
         monpole = as.integer(monpole),
         communaute = if_else(communaute == "true", 1L, 0L)
  )

nb_agregats <- agregats_mapd %>%
  group_by(seed, sim_name, annee) %>%
  tally() %>%
  rename(nbagregats = n)

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_agregats.csv", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write.csv(agregats_mapd, fileToWrite, quote = TRUE, row.names = FALSE)

connectToMapD()
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY agregats%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(results_agregats, agregats_mapd, sqlQuery)

############# GLOBAL #############

results_global <- list.files(pattern =
                               sprintf("%s_results_global.csv", prefixe_files)) %>%
  map(~read_csv(file = ., quote = "'")) %>%
  reduce(rbind)   %>%
  rename(seed = `string(seed)`) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep) %>%
  mutate(sim_name = !!sim_name)


results_mapd <- results_global %>%
  rename_all(funs(tolower(.x))) %>%
  left_join(nb_agregats, by = c("seed", "sim_name", "annee")) %>%
  left_join(results_global %>%
              rename(annee = Annee) %>%
              filter(annee == 840) %>%
              rename(cf_base = charge_fiscale) %>%
              select(seed,cf_base), by = "seed") %>%
  mutate(ratiochargefiscale = if_else(annee == 820, 0, charge_fiscale / cf_base)) %>%
  select(-cf_base)

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_results.csv", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write.csv(results_mapd, fileToWrite, quote = TRUE, row.names = FALSE)

connectToMapD()
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY results%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

#rm(results_global, results_mapd, sqlQuery)

############# SEEDS #############
seeds_mapd <- results_mapd %>%
  select(seed, sim_name) %>%
  group_by(seed, sim_name) %>%
  tally() %>%
  ungroup() %>%
  select(-n)

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_seeds.csv", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write.csv(seeds_mapd, fileToWrite, quote = TRUE, row.names = FALSE)

connectToMapD()
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY seeds%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)


rm(results_global, results_mapd, seeds_mapd, nb_agregats,sqlQuery)

############# SEIGNEURS #############

results_seigneurs <- list.files(pattern =
                                  sprintf("%s_results_seigneurs.csv", prefixe_files)) %>%
  map(~read_csv(file = ., quote = "'")) %>%
  reduce(rbind)   %>%
  rename(seed = `string(seed)`) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep) %>%
  mutate(sim_name = !!sim_name)

seigneurs_mapd <- results_seigneurs %>%
  rename_all(funs(tolower(.x))) %>%
  rename(id_seigneur = self) %>%
  select(id_seigneur, everything(), -geom) %>%
  mutate(id_seigneur = stringr::str_extract(id_seigneur, pattern="\\d+"),
         id_seigneur = as.integer(id_seigneur),
         monagregat = stringr::str_extract(monagregat, pattern="\\d+"),
         monagregat = as.integer(monagregat),
         initial = if_else(initial == "true", 1L, 0L)
  )

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_seigneurs.csv", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write.csv(seigneurs_mapd, fileToWrite, quote = TRUE, row.names = FALSE)

connectToMapD()
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY seigneurs%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(results_seigneurs, seigneurs_mapd, sqlQuery)

############# PAROISSES #############

results_paroisses <- list.files(pattern =
                                  sprintf("%s_results_paroisses.csv", prefixe_files)) %>%
  map(~read_csv(file = ., quote = "'")) %>%
  reduce(rbind)   %>%
  rename(seed = `string(seed)`) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep) %>%
  mutate(sim_name = !!sim_name)

paroisses_mapd <- results_paroisses %>%
  rename_all(funs(tolower(.x))) %>%
  rename(id_paroisse = self,
         area = shape.area) %>%
  select(id_paroisse, seed, sim_name, annee,
         moneglise, mode_promotion, area,
         nbfideles, satisfactionparoisse) %>%
  mutate(id_paroisse = stringr::str_extract(id_paroisse, pattern="\\d+"),
         id_paroisse = as.integer(id_paroisse),
         moneglise = stringr::str_extract(moneglise, pattern="\\d+"),
         moneglise = as.integer(moneglise)
  )

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_paroisses.csv", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write.csv(paroisses_mapd, fileToWrite, quote = TRUE, row.names = FALSE)

connectToMapD()
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY paroisses%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(results_paroisses, paroisses_mapd, sqlQuery)

############# POLES #############

results_poles <- list.files(pattern =
                              sprintf("%s_results_poles.csv", prefixe_files)) %>%
  map(~read_csv(file = ., quote = "'")) %>%
  reduce(rbind)   %>%
  rename(seed = `string(seed)`) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep) %>%
  mutate(sim_name = !!sim_name)

poles_mapd <- results_poles %>%
  rename_all(funs(tolower(.x))) %>%
  rename(id_pole = self) %>%
  select(id_pole, seed, sim_name, annee,
         everything()) %>%
  select(-geom) %>%
  mutate(id_pole = stringr::str_extract(id_pole, pattern="\\d+"),
         id_pole = as.integer(id_pole),
         monagregat = stringr::str_extract(monagregat, pattern="\\d+"),
         monagregat = as.integer(monagregat)
  )

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_poles.csv", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write.csv(poles_mapd, fileToWrite, quote = TRUE, row.names = FALSE)

connectToMapD()
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY poles%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(results_poles, poles_mapd, sqlQuery)

############# FP #############

results_fp <- list.files(pattern =
                           sprintf("%s_results_FP.csv", prefixe_files)) %>%
  map(~read_csv(file = ., quote = "'")) %>%
  reduce(rbind)   %>%
  rename(seed = `string(seed)`) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep) %>%
  mutate(sim_name = !!sim_name)


fp_mapd <- results_fp %>%
  select(self, seed, sim_name, Annee, communaute, monAgregat,
         sMat, sRel, sProt, Satis, mobile, type_deplacement,
         deplacement_from, deplacement_to, nb_preleveurs) %>%
  rename(id_fp = self,
         annee = Annee,
         monagregat = monAgregat,
         smat = sMat,
         srel = sRel,
         sprot = sProt,
         satis = Satis) %>%
  mutate(id_fp = stringr::str_extract(id_fp, pattern="\\d+"),
         id_fp = as.integer(id_fp),
         communaute = if_else(communaute == "true", 1L, 0L),
         monagregat = stringr::str_extract(monagregat, pattern="\\d+"),
         monagregat = as.integer(monagregat),
         mobile = if_else(mobile == "true", 1L, 0L)
  )

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_FP.csv", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write.csv(fp_mapd, fileToWrite, quote = TRUE, row.names = FALSE)

connectToMapD()
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY fp%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(results_fp, fp_mapd, sqlQuery)

#########
# TESTS #
#########



nonUniqueParams <- params %>%
    gather(key = "Var", value = "Value") %>%
    group_by(Var, Value) %>%
    mutate(Freq = n()) %>%
    ungroup() %>%
    filter(Freq != nrow(params)) %>%
    distinct(Var) %>%
    pull(Var)
  
  params %>% dplyr::select(!!nonUniqueParams)
  
  
  
  #####
  params_augmented <- read_csv("~/params_4_5_mapd.csv", quote = '"') %>%
    mutate(taille_cote_monde = 100) %>%
    mutate_all(.funs = funs(as.character))
  write.csv(params_augmented, "~/mapd-docker-storage/data/mapd_import/params_augmented_4_5.csv", quote = TRUE, row.names = FALSE)
  
  
  connectToMapD()
  sqlQuery <- DBI::dbSendQuery(conn = conMapD, "COPY param_test2 FROM '/mapd-storage/data/mapd_import/params_augmented_4_5.csv';")
  print(DBI::dbFetch(sqlQuery))
  dbDisconnect(conMapD)
  