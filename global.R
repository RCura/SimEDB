suppressPackageStartupMessages({
  # Data wrangling
  library(tidyverse)
  library(magrittr)
  library(stringr)
  library(forcats)
  
  # DataBase
  library(dbplyr)
  library(DBI)
  library(MonetDBLite) # devtools::install_github("hannesmuehleisen/MonetDBLite")
  
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
})

conMonetDB <- dbConnect(MonetDBLite::MonetDBLite(), "data/db_Transition8")

seeds <- tbl(conMonetDB, "seeds")
agregats <- tbl(conMonetDB, "agregats")
fp <- tbl(conMonetDB, "fp")
parameters <- tbl(conMonetDB, "parameters")
paroisses <- tbl(conMonetDB, "paroisses")
poles <- tbl(conMonetDB, "poles")
results <- tbl(conMonetDB, "results")
seigneurs <- tbl(conMonetDB, "seigneurs")

all_sim_names <- parameters %>%
  select(sim_name) %>%
  distinct() %>%
  arrange(sim_name) %>%
  collect() %>%
  pull()
