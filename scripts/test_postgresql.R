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
  library(DT)
})

library(RPostgreSQL)
conPgSQL <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), dbname = "robin")

seeds <- tbl(conPgSQL, "seeds")
agregats <- tbl(conPgSQL, "agregats")
fp <- tbl(conPgSQL, "fp")
parameters <- tbl(conPgSQL, "parameters")
paroisses <- tbl(conPgSQL, "paroisses")
poles <- tbl(conPgSQL, "poles")
results <- tbl(conPgSQL, "results")
seigneurs <- tbl(conPgSQL, "seigneurs")



all_sim_names <- parameters %>%
  select(sim_name) %>%
  distinct() %>%
  arrange(sim_name) %>%
  collect() %>%
  pull()

FP_data <- fp

nombre_FP_total <- FP_data %>%
  filter(sim_name == "4_5_A") %>%
  group_by(seed, sim_name, annee) %>%
  summarise(n_total = n())

system.time({
  types_deplacements <- FP_data %>%
    filter(sim_name == "4_5_A") %>%
    filter(type_deplacement != "nil") %>%
    filter(type_deplacement != "Non mobile") %>%
    group_by(annee, seed, sim_name, type_deplacement) %>%
    summarise(n = n()) %>%
    left_join(nombre_FP_total, by = c("seed", "annee", "sim_name")) %>%
    mutate(Tx = (n * 1.0) / (n_total * 1.0)) %>%
    ungroup() %>%
    collect()
})


ggplot(types_deplacements, aes(factor(annee), Tx, col = type_deplacement)) +
  geom_tufteboxplot(size = 1) +
  geom_line() +
  facet_wrap(~ type_deplacement) +
  scale_y_continuous(labels = percent) +
  scale_color_discrete(guide = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") +
  xlab("Temps") + ylab("Part des Foyers Paysans") +
  ggtitle("Type de déplacement des Foyers Paysans") +
  labs(subtitle = "Variabilité : Foyers Paysans et Réplications")
