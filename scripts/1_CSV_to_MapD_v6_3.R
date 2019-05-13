library(tidyverse)
library(stringr)
library(DBI)
library(RJDBC)
library(ssh)

######################################
############# PARAMETRES #############
######################################

outputs_path <- "/data/user/c/rcura/"
setwd(outputs_path)

prefixe_files <- "6_3_Obj50k"
suffixe_tables <- "_6_3"
nb_replications_to_keep <- 20

options( java.parameters = c("-Xss2560k", "-Xmx8g") ) # Needed fix for rJava (JDBC) + ggplot2

conMapD <- NULL
connectToMapD <- function(){
  conMapD <<- dbConnect(drv = JDBC("com.mapd.jdbc.MapDDriver",
                                   "/data/user/c/rcura/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
                                   identifier.quote="'"),
                        "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")
}
session_ssh <- ssh_connect("rcura@mapd.cura.info")

########################################################
############# ON ISOLE LES SEEDS CORRECTES #############
########################################################

set.seed(2)
finished_seeds <- read_csv(file = sprintf("%s_results_global.csv", prefixe_files)) %>%
  filter(annee == 1200) %>%
  pull(seed)

params <- read_csv(file = sprintf("%s_parameters.csv", prefixe_files)) %>%
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
seeds_to_keep <- as.character(seeds_to_keep)
rm(finished_seeds)


#########################################################
############# ON NETTOIE ET ENVOI DANS MAPD #############
#########################################################

############# PARAMETERS #############

# params <- read_csv(file = sprintf("%s_parameters.csv", prefixe_files)) %>%
#   filter(seed %in% seeds_to_keep)


# params_mapd <- params %>%
#   gather(parametre, valeur, -seed, -sim_name)


params <- read_csv(file = sprintf("%s_parameters.csv", prefixe_files)) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep)
params_mapd <- params %>%
  gather(parameter, valeur, -seed, -sim_name)

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_parameters.csv.gz", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write_csv(params_mapd, fileToWrite)
scp_upload(session = session_ssh, files = fileToWrite, to = "~/mapd-docker-storage/data/mapd_import/")
file.remove(fileToWrite)

connectToMapD()
sprintf("COPY parameters%s FROM '%s';", suffixe_tables, fileToRead)
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY parameters%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(params, params_mapd, sqlQuery)


############# AGREGATS #############

results_agregats <- read_csv(file = sprintf("%s_results_agregats.csv", prefixe_files), quote = '"') %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep)

agregats_mapd <- results_agregats

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_agregats.csv.gz", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write_csv(agregats_mapd, fileToWrite)
scp_upload(session = session_ssh, files = fileToWrite, to = "~/mapd-docker-storage/data/mapd_import/")
file.remove(fileToWrite)

connectToMapD()
sprintf("COPY agregats%s FROM '%s';", suffixe_tables, fileToRead)
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY agregats%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(results_agregats, agregats_mapd, sqlQuery)

############# GLOBAL #############

results_global <-read_csv(file = sprintf("%s_results_global.csv", prefixe_files)) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep)


results_mapd <- results_global %>%
  left_join(results_global %>%
              filter(annee == 840) %>%
              rename(cf_base = charge_fiscale) %>%
              select(seed,cf_base), by = "seed") %>%
  mutate(ratio_charge_fiscale = if_else(annee == 820, 0, charge_fiscale / cf_base)) %>%
  select(-cf_base)

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_global.csv.gz", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write_csv(results_mapd, fileToWrite)
scp_upload(session = session_ssh, files = fileToWrite, to = "~/mapd-docker-storage/data/mapd_import/")
file.remove(fileToWrite)

connectToMapD()
sprintf("COPY global%s FROM '%s';", suffixe_tables, fileToRead)
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY global%s FROM '%s';", suffixe_tables, fileToRead))
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

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_seeds.csv.gz", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write_csv(seeds_mapd, fileToWrite)
scp_upload(session = session_ssh, files = fileToWrite, to = "~/mapd-docker-storage/data/mapd_import/")
file.remove(fileToWrite)

connectToMapD()
sprintf("COPY seeds%s FROM '%s';", suffixe_tables, fileToRead)
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY seeds%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)


rm(results_global, results_mapd, seeds_mapd,sqlQuery)

############# SEIGNEURS #############

results_seigneurs <- read_csv(file = sprintf("%s_results_seigneurs.csv", prefixe_files)) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep)

seigneurs_mapd <- results_seigneurs

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_seigneurs.csv.gz", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write_csv(seigneurs_mapd, fileToWrite)
scp_upload(session = session_ssh, files = fileToWrite, to = "~/mapd-docker-storage/data/mapd_import/")
file.remove(fileToWrite)

connectToMapD()
sprintf("COPY seigneurs%s FROM '%s';", suffixe_tables, fileToRead)
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY seigneurs%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(results_seigneurs, seigneurs_mapd, sqlQuery)

############# PAROISSES #############

results_paroisses <- read_csv(file = sprintf("%s_results_paroisses.csv", prefixe_files)) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep)

paroisses_mapd <- results_paroisses

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_paroisses.csv.gz", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write_csv(paroisses_mapd, fileToWrite)
scp_upload(session = session_ssh, files = fileToWrite, to = "~/mapd-docker-storage/data/mapd_import/")
file.remove(fileToWrite)

connectToMapD()
sprintf("COPY paroisses%s FROM '%s';", suffixe_tables, fileToRead)
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY paroisses%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(results_paroisses, paroisses_mapd, sqlQuery)

############# POLES #############

results_poles <- read_csv(file = sprintf("%s_results_poles.csv", prefixe_files)) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep)

poles_mapd <- results_poles

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_poles.csv.gz", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write_csv(poles_mapd, fileToWrite)
scp_upload(session = session_ssh, files = fileToWrite, to = "~/mapd-docker-storage/data/mapd_import/")
file.remove(fileToWrite)

connectToMapD()
sprintf("COPY poles%s FROM '%s';", suffixe_tables, fileToRead)
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY poles%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(results_poles, poles_mapd, sqlQuery)


############# CHATEAUX #############

results_chateaux <- read_csv(file = sprintf("%s_results_chateaux.csv", prefixe_files)) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep)

chateaux_mapd <- results_chateaux

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_chateaux.csv.gz", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write_csv(chateaux_mapd, fileToWrite)
scp_upload(session = session_ssh, files = fileToWrite, to = "~/mapd-docker-storage/data/mapd_import/")
file.remove(fileToWrite)

connectToMapD()
sprintf("COPY chateaux%s FROM '%s';", suffixe_tables, fileToRead)
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY chateaux%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(results_chateaux, chateaux_mapd, sqlQuery)

############# FP #############

results_fp <- read_csv(file = sprintf("%s_results_FP_summarised.csv", prefixe_files)) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed %in% seeds_to_keep)


fp_mapd <- results_fp

fileToWrite <- sprintf("~/mapd-docker-storage/data/mapd_import/%s_MapD_FP.csv.gz", prefixe_files)
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write_csv(fp_mapd, fileToWrite)
scp_upload(session = session_ssh, files = fileToWrite, to = "~/mapd-docker-storage/data/mapd_import/")
file.remove(fileToWrite)

connectToMapD()
sprintf("COPY fp%s FROM '%s';", suffixe_tables, fileToRead)
sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY fp%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))
dbDisconnect(conMapD)

rm(results_fp, fp_mapd, sqlQuery)


#### Cleaning ####
rm(fileToRead, fileToWrite,
   nb_experiments, nb_replications_to_keep,
   outputs_path, prefixe_files,
   seeds_to_keep, suffixe_tables)

ssh_disconnect(session = session_ssh)
rm(session_ssh, conMapD, connectToMapD)

