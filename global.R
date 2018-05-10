suppressPackageStartupMessages({
  # Data wrangling
  library(tidyverse)
  library(magrittr)
  library(stringr)
  library(forcats)
  
  # DataBase
  library(dbplyr)
  library(DBI)
  #library(MonetDBLite)
  library(RJDBC)
  
  # Interactivity
  library(shiny)
  library(shinythemes)
  library(parcoords) # devtools::install_github("timelyportfolio/parcoords", ref="feature/resize")
  library(ShinyRatingInput) # devtools::install_github("stefanwilhelm/ShinyRatingInput")
  
  # Plots
  library(gridExtra)
  library(ggthemes)
  
  # Tables
  library(xtable)
  library(formattable) # devtools::install_github("renkun-ken/formattable")
  library(DT)
  library(shinycssloaders)
  
})

# drv <- JDBC("com.mapd.jdbc.MapDDriver",
#             "/home/shiny/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
#             identifier.quote="'")
# conMapD <- dbConnect(drv, "jdbc:mapd:localhost:9091:mapd", "mapd", "HyperInteractive")
# seeds <- tbl(conMapD, "seeds")
# agregats <- tbl(conMapD, "agregats")
# fp <- tbl(conMapD, "fp")
# parameters <- tbl(conMapD, "parameters")
# paroisses <- tbl(conMapD, "paroisses")
# poles <- tbl(conMapD, "poles")
# results <- tbl(conMapD, "results")
# seigneurs <- tbl(conMapD, "seigneurs")

# conMonetDB <- dbConnect(MonetDBLite::MonetDBLite(), "data/db_Transition8")
# seeds <- tbl(conMonetDB, "seeds")
# agregats <- tbl(conMonetDB, "agregats")
# fp <- tbl(conMonetDB, "fp")
# parameters <- tbl(conMonetDB, "parameters")
# paroisses <- tbl(conMonetDB, "paroisses")
# poles <- tbl(conMonetDB, "poles")
# results <- tbl(conMonetDB, "results")
# seigneurs <- tbl(conMonetDB, "seigneurs")

# write_csv(fp %>% collect(), "~/all_outputs_TR8/fp.csv")
# write_csv(seeds %>% collect(), "~/all_outputs_TR8/seeds.csv")
# write_csv(agregats %>% collect(), "~/all_outputs_TR8/agregats.csv")
# write_csv(parameters %>% collect(), "~/all_outputs_TR8/parameters.csv")
# write_csv(paroisses %>% collect(), "~/all_outputs_TR8/paroisses.csv")
# write_csv(poles %>% collect(), "~/all_outputs_TR8/poles.csv")
# write_csv(results %>% collect(), "~/all_outputs_TR8/results.csv")
# write_csv(seigneurs %>% collect(), "~/all_outputs_TR8/seigneurs.csv")

options( java.parameters = c("-Xss2560k", "-Xmx8g") ) # Needed fix for rJava (JDBC) + ggplot2

drv <- JDBC("com.mapd.jdbc.MapDDriver",
            "/home/robin/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
            identifier.quote="'")
conMapD <- dbConnect(drv, "jdbc:mapd:localhost:9091:mapd", "mapd", "HyperInteractive")
parameters <- tbl(conMapD, "parameters")

all_sim_names <- parameters %>%
  select(sim_name) %>%
  distinct() %>%
  arrange(sim_name) %>%
  collect() %>%
  pull()

dbDisconnect(conMapD)


########## SENSITIVITY #########

# results_sensivity <- read_csv("data/4_5_OM_results_om.csv", col_types = cols()) %>%
#   rename(param = sensibility_parameter) %>%
#   rename(valeur = sensibility_value) %>%
#   distinct(param, valeur, seed, .keep_all = TRUE) %>%
#   mutate(valeur = round(valeur, digits = 5))
# 
# experiment_plan <- readRDS("data/sensib_params.Rds") %>%
#   select(param, valeur, seed) %>%
#   mutate(valeur = round(valeur, digits = 5)) %>%
#   full_join(results_sensivity, by = c("param", "valeur", "seed")) %>%
#   select(-sim_name)
# 
# rm(results_sensivity)
# 
# Objectifs <- data_frame(
#   Var = c("nb_agregats_om", "nb_chateaux_om", "nb_gros_chateaux_om", "nb_seigneurs_om",
#           "nb_eglises_paroissiales_om", "distance_eglises_paroissiales_om",
#           "proportion_fp_isoles_om", "augmentation_charge_fiscale_om"),
#   RealVar = c("Agrégats", "Châteaux",  "Gros châteaux", "Seigneurs",
#               "Églises paroissiales", "Distance moyenne entre églises",  
#               "Part de foyers paysans isolés",
#               "Augmentation de la charge fiscale des foyers paysans"),
#   Objectif = c(200, 50, 10, 200, 300, 3000, 0.2, 3)
# )
# 
# filtered_data <- experiment_plan %>%
#   group_by(param, valeur) %>%
#   arrange(nb_agregats_om) %>%
#   filter(row_number() <= 10) %>%
#   ungroup() %>%
#   gather(key = Indicateur, value = Resultat, -param, -valeur, -seed) %>%
#   filter(Indicateur != "nb_eglises_om") %>%
#   left_join(Objectifs, by = c("Indicateur" = "Var")) %>%
#   mutate(Indicateur = RealVar) %>%
#   select(-RealVar, -Objectif) %>%
#   filter(!is.na(Indicateur))
# 
# standardised_data <- filtered_data %>%
#   group_by(Indicateur) %>%
#   mutate(StdResult = scale(Resultat,  center = TRUE, scale = TRUE)) %>%
#   ungroup()
# 
# resAB <- standardised_data %>%
#   group_by(param, valeur, Indicateur) %>%
#   summarise(sd_intra =  sd(StdResult, na.rm = TRUE)) %>%
#   group_by(param) %>%
#   summarise(med_sd_intra = mean(sd_intra, na.rm = TRUE),
#             Q1_sd_intra = quantile(sd_intra, na.rm = TRUE, probs = .25))
# 
# resCD <- standardised_data %>%
#   group_by(param, valeur, Indicateur) %>%
#   summarise(avg = mean(StdResult, na.rm = TRUE)) %>%
#   group_by(param, Indicateur) %>%
#   summarise(sd_avg = sd(avg, na.rm = TRUE)) %>%
#   group_by(param) %>%
#   summarise(avg_sd_avg = mean(sd_avg, na.rm = TRUE),
#             Q3_sd_avg =  quantile(sd_avg, na.rm = TRUE, probs = .75))
# 
# 
# resEF <- filtered_data %>%
#   spread(key = Indicateur, value = Resultat) %>%
#   select(-param, -valeur, -seed) %>%
#   scale(scale = TRUE, center = Objectifs %>% arrange(RealVar) %>% pull(Objectif)) %>%
#   as.data.frame() %>%
#   bind_cols(filtered_data %>%
#               spread(key = Indicateur, value = Resultat) %>%
#               select(param, valeur, seed)) %>%
#   select(param, valeur, seed, everything()) %>%
#   gather(key = Indicateur, value = StdResultat, -param, -valeur, -seed) %>%
#   group_by(param) %>%
#   summarise(avg_ecart =  mean(abs(StdResultat), na.rm = TRUE),
#             Q1_ecart =  quantile(abs(StdResultat), na.rm = TRUE, probs =  .25))
# 
# resG <- filtered_data %>%
#   group_by(param, valeur, Indicateur) %>%
#   summarise(nb_failing_seeds = sum(is.na(Resultat), na.rm = TRUE)) %>%
#   ungroup() %>%
#   group_by(param) %>%
#   summarise(percent_failing_seeds = mean(nb_failing_seeds) * 100 / n())
# 
# resH <- filtered_data %>%
#   filter(Indicateur == "Agrégats") %>%
#   group_by(param, valeur) %>%
#   summarise(nb_failing_seeds = sum(is.na(Resultat), na.rm = TRUE)) %>%
#   mutate(is_failing = ifelse(nb_failing_seeds > 0, TRUE, FALSE)) %>%
#   ungroup() %>%
#   group_by(param) %>%
#   summarise(nb_failing_values = sum(is_failing))
# 
# sensibility_summary_table <- resAB %>%
#   left_join(resCD, by = "param") %>%
#   left_join(resEF, by = "param") %>%
#   left_join(resG, by = "param") %>%
#   left_join(resH, by =  "param") %>%
#   mutate_if(is.numeric, round, digits = 3)
# 
# rm(Objectifs, standardised_data, resAB, resCD, resEF, resG, resH)
# 
# saveRDS(object = experiment_plan, file = "data/sensib/experiment_plan.Rds")
# saveRDS(object = filtered_data, file = "data/sensib/filtered_data.Rds")
# saveRDS(object = sensibility_summary_table, file = "data/sensib/sensibility_summary_table.Rds")

experiment_plan <- readRDS("data/sensib/experiment_plan.Rds")
filtered_data <- readRDS("data/sensib/filtered_data.Rds")
