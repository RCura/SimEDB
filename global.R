suppressPackageStartupMessages({
  
  # Interactivity
  library(shiny)
  library(shinythemes)
  library(parcoords) # devtools::install_github("timelyportfolio/parcoords", ref="feature/resize")
  library(ShinyRatingInput) # devtools::install_github("stefanwilhelm/ShinyRatingInput")

  # DataBase
  library(dbplyr)
  library(DBI)
  library(RSQLite)
  
  # Plots
  library(gridExtra)
  library(ggthemes)
  
  # Tables
  library(xtable)
  library(formattable) # devtools::install_github("renkun-ken/formattable")
  
  # Data wrangling
  library(tidyverse)
  library(magrittr)
  library(stringr)
  library(forcats)
  
  
})

con <- DBI::dbConnect(RSQLite::SQLite(), "data/outputs_TR8_indexSimName.sqlite")

seeds <- tbl(con, "goodSeeds")
agregats <- tbl(con, "agregats")
fp <- tbl(con, "fp")
parameters <- tbl(con, "parameters")
paroisses <- tbl(con, "paroisses")
poles <- tbl(con, "poles")
results <- tbl(con, "results")
seigneurs <- tbl(con, "seigneurs")

goodSeeds <- seeds %>% filter(sim_name %in% "4_4_B") %>% collect()
sim_agregats <- agregats %>% filter(sim_name %in% "4_4_B") %>% collect() %>%
  mutate(communaute = as.logical(communaute))
sim_FP <- fp %>% filter(sim_name %in% "4_4_B") %>% collect()  %>%
  mutate(communaute = as.logical(communaute)) %>%
  mutate(mobile = as.logical(mobile))

sim_parameters <- parameters %>% filter(sim_name %in% "4_4_B")  %>% collect()
sim_paroisses <- paroisses %>% filter(sim_name %in% "4_4_B") %>% collect()
sim_poles <- poles %>% filter(sim_name %in% "4_4_B") %>% collect()
sim_results <- results %>% filter(sim_name %in% "4_4_B") %>% collect()
sim_seigneurs <- seigneurs %>% filter(sim_name %in% "4_4_B") %>% collect() %>%
  mutate(initial = as.logical(initial))

#load("data/sim_data_4_4_D.Rdata")


