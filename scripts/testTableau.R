
Objectifs <- data_frame(
  Var = c("NbAgregats", "nbChateaux", "nbGdChateaux", "nbEglisesParoissiales", "distance_eglises_paroissiales", "prop_FP_isoles", "RatioChargeFiscale"),
  RealVar = c("Nombre d'agrégats", "Nombre de châteaux",  "Nombre de gros châteaux",
              "Nombre d'églises paroissiales", "Distance moyenne entre églises",  
              "Proportion de FP isolés", "Augmentation de la charge fiscale (ratio)"),
  Objectif = c(200, 50, 10, 300, 3000, 0.2, 3),
  Ordre = 1:7
)

tableau_resultats <- sim_results %>%
  filter(Annee == 1160) %>%
  select(-seed) %>%
  rename_all(funs(gsub(x = ., pattern = "_", replacement = "."))) %>%
  summarise_if(is.numeric,funs(
    Moyenne = mean,
    Mediane = median,
    Q1 = quantile(., na.rm = TRUE, probs = .25),
    Q3 = quantile(., na.rm = TRUE, probs = .75),
    StDev = sd,
    Min = min,
    Max = max
  ), na.rm = TRUE
  ) %>%
  gather(key = Var, value = Value) %>%
  separate(Var, sep = "_", into = c("Var", "Indice")) %>%
  mutate(Var = gsub(Var, pattern = ".", replacement = "_", fixed = TRUE)) %>%
  left_join(Objectifs, by = "Var") %>%
  filter(!is.na(RealVar)) %>%
  spread(key = Indice,value = Value) %>%
  arrange(Ordre) %>%
  select(RealVar, Objectif,Moyenne, Mediane, Q1, Q3, StDev, Min, Max) %>%
  rename(Indicateur = RealVar,
         `Valeur attendue` = Objectif,
         `Médiane` = Mediane,
         `1er quartile` = Q1,
         `3ème quartile` = Q3,
         `Écart-type` = StDev) %>%
  select(-Min, -Max)


view_kable <- function(x, ...){
  tab <- paste(capture.output(x, ...), collapse = '\n')
  tf <- tempfile(fileext = ".html")
  writeLines(tab, tf)
  rstudioapi::viewer(tf)
}

library(knitr)
library(kableExtra)
options(knitr.table.format = "html") 

kable(tableau_resultats) %>%
  kable_styling("striped") %>%
  add_header_above(c(" "=2,
                     "Résultats obtenus en fin de simulation" = 5)) %>%
  writeLines(con = "test.html", .) %>%
  rstudioapi::viewer(url = "test.html")
