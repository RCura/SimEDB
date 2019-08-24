library(tidyverse)
library(stringr)
library(DBI)
library(RJDBC)
library(ssh)
library(glue)

######################################
############# PARAMETRES #############
######################################

outputs_path <- "/data/user/c/rcura/"
setwd(outputs_path)
prefixe_files <- "6_6_Scenarios_base"
suffixe_tables <- "6_4"
nb_replications_to_keep <- 20


session_ssh <- ssh_connect("rcura@mapd.cura.info")
omnisci_driver <- JDBC("com.omnisci.jdbc.OmniSciDriver",
                          "/data/user/c/rcura/omnisci-jdbc-4.7.1.jar",
                          identifier.quote="'")

options( java.parameters = c("-Xss2560k", "-Xmx8g") ) # Needed fix for rJava (JDBC) + ggplot2

write_send_import <- function(table, filename, prefix_files = "6_5_1",
                              file_ext = ".csv.tar.gz", ssh_session = session_ssh,
                              db_driver = omnisci_driver, db_table,db_table_suffix = "6_4"){
  destination_path <- "~/omnisci-storage/data/mapd_import/"
  fileToWrite <-  fileToWrite <- glue({destination_path}, {prefix_files}, "_", {filename}, {file_ext})
  write_csv(table, fileToWrite)
  print("File written")
  scp_upload(session = ssh_session, files = fileToWrite, to = destination_path, verbose = TRUE)
  print("File uploaded")
  file.remove(fileToWrite)
  docker_filepath <- gsub(fileToWrite, pattern = "~", replacement = "")
  connexion <- dbConnect(db_driver, "jdbc:omnisci:mapdi.cura.info:6274:omnisci", "admin", "HyperInteractive")
  sql_query <- glue("COPY {db_table}_{db_table_suffix} FROM '{docker_filepath}';")
  run_query <- DBI::dbSendQuery(conn = connexion, sql_query)
  query_result <- DBI::dbFetch(run_query)
  dbDisconnect(connexion)
  return(query_result)
}

# testTable <- mtcars %>% as_tibble(rownames = "carname")
# write_send_import(table = testTable, filename = "omnisci_mtcars", db_table = "mtcars", db_table_suffix = "6_4")


########################################################
############# ON ISOLE LES SEEDS CORRECTES #############
########################################################

set.seed(2)
finished_seeds <- read_csv(file = sprintf("%s_results_global.csv", prefixe_files),  col_types = cols(seed = "c")) %>%
  mutate(seed = as.character(round(digits = 12,as.numeric(seed)))) %>%
  mutate(seed = str_sub(seed, start = 1, end = 9)) %>%
  filter(annee == "1200") %>%
  pull(seed)

params <- read_csv(file = sprintf("%s_parameters.csv", prefixe_files),  col_types = cols(seed = "c")) %>%
  mutate(seed = as.character(round(digits = 12,as.numeric(seed)))) %>%
  mutate(seed = str_sub(seed, start = 1, end = 9)) %>%
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
rm(finished_seeds, params, nb_experiments)


#########################################################
############# ON NETTOIE ET ENVOI DANS MAPD #############
#########################################################

############# PARAMETERS #############
params <- read_csv(file = sprintf("%s_parameters.csv", prefixe_files),  col_types = cols(seed = "c")) %>%
  mutate(seed = as.character(round(digits = 12,as.numeric(seed)))) %>%
  mutate(seed = str_sub(seed, start = 1, end = 9)) %>%
  filter(seed %in% seeds_to_keep) %>%
  gather(parametre, valeur, -seed, -sim_name)

write_send_import(table = params, filename = "omnisci_params", db_table = "parameters")
rm(params)

############# AGREGATS #############
agregats <- read_csv(file = sprintf("%s_results_agregats.csv", prefixe_files), quote = '"', col_types = cols(seed = "c")) %>%
  mutate(seed = as.character(round(digits = 12,as.numeric(seed)))) %>%
  mutate(seed = str_sub(seed, start = 1, end = 9)) %>%
  filter(seed %in% seeds_to_keep)

write_send_import(table = agregats, filename = "omnisci_agregats", db_table = "agregats")
rm(agregats)

############# GLOBAL #############
results_global <-read_csv(file = sprintf("%s_results_global.csv", prefixe_files), col_types = cols(seed = "c")) %>%
  mutate(seed = as.character(round(digits = 12,as.numeric(seed)))) %>%
  mutate(seed = str_sub(seed, start = 1, end = 9)) %>%
  filter(seed %in% seeds_to_keep)

results_mapd <- results_global %>%
  left_join(results_global %>%
              filter(annee == 840) %>%
              rename(cf_base = charge_fiscale) %>%
              select(seed,cf_base), by = "seed") %>%
  mutate(ratio_charge_fiscale = if_else(annee == 820, 0, charge_fiscale / cf_base)) %>%
  select(-cf_base)

write_send_import(table = results_mapd, filename = "omnisci_global", db_table = "global")

############# SEEDS #############
seeds_mapd <- results_mapd %>%
  select(seed, sim_name) %>%
  group_by(seed, sim_name) %>%
  tally() %>%
  ungroup() %>%
  select(-n)

write_send_import(table = seeds_mapd, filename = "omnisci_seeds", db_table = "seeds")
rm(results_global, results_mapd, seeds_mapd)

############# SEIGNEURS #############
results_seigneurs <- read_csv(file = sprintf("%s_results_seigneurs.csv", prefixe_files), col_types = cols(seed = "c")) %>%
  mutate(seed = as.character(round(digits = 12,as.numeric(seed)))) %>%
  mutate(seed = str_sub(seed, start = 1, end = 9)) %>%
  filter(seed %in% seeds_to_keep)

write_send_import(table = results_seigneurs, filename = "omnisci_seigneurs", db_table = "seigneurs")
rm(results_seigneurs)

############# PAROISSES #############
results_paroisses <- read_csv(file = sprintf("%s_results_paroisses.csv", prefixe_files), col_types = cols(seed = "c")) %>%
  mutate(seed = as.character(round(digits = 12,as.numeric(seed)))) %>%
  mutate(seed = str_sub(seed, start = 1, end = 9)) %>%
  filter(seed %in% seeds_to_keep)

write_send_import(table = results_paroisses, filename = "omnisci_paroisses", db_table = "paroisses")
rm(results_paroisses)

############# POLES #############
results_poles <- read_csv(file = sprintf("%s_results_poles.csv", prefixe_files), col_types = cols(seed = "c")) %>%
  mutate(seed = as.character(round(digits = 12,as.numeric(seed)))) %>%
  mutate(seed = str_sub(seed, start = 1, end = 9)) %>%
  filter(seed %in% seeds_to_keep)

write_send_import(table = results_poles, filename = "omnisci_poles", db_table = "poles")
rm(results_poles)

############# CHATEAUX #############
results_chateaux <- read_csv(file = sprintf("%s_results_chateaux.csv", prefixe_files), col_types = cols(seed = "c")) %>%
  mutate(seed = as.character(round(digits = 12,as.numeric(seed)))) %>%
  mutate(seed = str_sub(seed, start = 1, end = 9)) %>%
  filter(seed %in% seeds_to_keep)

write_send_import(table = results_chateaux, filename = "omnisci_chateaux", db_table = "chateaux")
rm(results_chateaux)

############# FP #############
results_fp <- read_csv(file = sprintf("%s_results_FP_summarised.csv", prefixe_files), col_types = cols(seed = "c")) %>%
  mutate(seed = as.character(round(digits = 12,as.numeric(seed)))) %>%
  mutate(seed = str_sub(seed, start = 1, end = 9)) %>%
  filter(seed %in% seeds_to_keep)

write_send_import(table = results_fp, filename = "omnisci_fp", db_table = "fp")
rm(results_fp)


#### Cleaning ####

ssh_disconnect(session = session_ssh)
rm(session_ssh)

