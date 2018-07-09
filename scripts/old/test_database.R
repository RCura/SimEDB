library(dplyr)
library(dbplyr)
library(DBI)
library(RSQLite)
con <- DBI::dbConnect(RSQLite::SQLite(), "test.sqlite")

con %>%
  copy_to(sim_FP, "FP",  temporary = FALSE)

sim_FP_db <- tbl(con, "goodSeeds")


blob <- sim_FP_db %>%
  group_by(seed, sim_name, Annee) %>%
  summarise(N = n())

blob %>% collect()

DBI::dbDisconnect(con)

system.time({
  FP_Satisfaction(FP_data = sim_FP)
})

system.time({
  FP_Satisfaction(FP_data = sim_FP_db)
})

library(tidyverse)

FP_Satisfaction <- function(FP_data){
  satisfaction_data <- FP_data %>%
    select(Annee, sMat, sRel, sProt, Satis) %>%
    collect() %>%
    rename(
      Globale = Satis,
      Matérielle = sMat,
      Protection = sProt,
      Religieuse = sRel) %>%
    group_by(Annee) %>%
    sample_n(size = 4E3, replace = FALSE) %>%
    ungroup() %>%
    gather(key = Type, value = Satisfaction, -Annee)
  
  ggplot(satisfaction_data, aes(Annee, Satisfaction, col = Type, fill = Type)) +
    geom_violin(aes(group = factor(Annee))) +
    facet_wrap(~ Type) +
    geom_smooth(data = satisfaction_data %>%
                  group_by(Annee) %>%
                  sample_n(size = 100, replace = FALSE) %>%
                  ungroup(),
                alpha = .3, se = FALSE, na.rm = TRUE, method = "gam", formula = y ~ s(x, bs = "cs")) +
    theme(legend.position = "none") +
    ggtitle("Évolution de la satisfaction des FP\n(Échantillon de 4000 FP / an)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(subtitle = "Variabilité : Foyers Paysans et Réplications")
  
  print(last_plot())
}
