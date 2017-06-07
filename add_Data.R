library(tidyverse)

outputs_folder <- "../GAMA/transition8/outputs/"
baseName <- "afterTMD_1"


parameters_file <- sprintf("%s%s_parameters_TMD.csv", outputs_folder, baseName)
results_file <- sprintf("%s%s_results_TMD.csv", outputs_folder, baseName)
seigneurs_file <- sprintf("%s%s_results_TMD_seigneurs.csv", outputs_folder, baseName)
agregats_file <- sprintf("%s%s_results_TMD_agregats.csv", outputs_folder, baseName)
poles_file <- sprintf("%s%s_results_TMD_poles.csv", outputs_folder, baseName)
FP_all_file <- sprintf("%s%s_results_TMD_FP_all.csv", outputs_folder, baseName)
FP_summary_file <- sprintf("%s%s_results_TMD_summFP.csv", outputs_folder, baseName)
paroisses_file <- sprintf("%s%s_results_TMD_paroisses.csv", outputs_folder, baseName)


parameters_name <-
  c(
    "seed",
    "name",
    "debut_simulation",
    "fin_simulation",
    "duree_step",
    "besoin_protection",
    "distance_detection_agregats",
    "nombre_FP_agregat",
    "nombre_agglos_antiques",
    "nombre_villages",
    "nombre_foyers_villages_max",
    "puissance_communautes",
    "apparition_communautes",
    "proba_apparition_communaute",
    "nombre_foyers_paysans",
    "taux_renouvellement",
    "taux_mobilite",
    "distance_max_dem_local",
    "seuil_puissance_armee",
    "deplacement_alternate",
    "nombre_seigneurs_objectif",
    "nombre_grands_seigneurs",
    " nombre_petits_seigneurs",
    "puissance_grand_seigneur1",
    "puissance_grand_seigneur2",
    "proba_collecter_loyer",
    "proba_creation_ZP_banaux",
    "proba_creation_ZP_basseMoyenneJustice",
    "rayon_min_PS",
    "rayon_max_PS",
    "min_fourchette_loyers_PS",
    "max_fourchette_loyers_PS",
    " proba_don_partie_ZP",
    "apparition_chateaux",
    "nb_chateaux_potentiels_GS",
    "seuil_attractivite_chateau",
    "proba_creer_chateau_GS",
    "proba_chateau_agregat",
    "proba_don_chateau_GS",
    "proba_creer_chateau_PS",
    "proba_gain_droits_hauteJustice_chateau",
    "proba_gain_droits_banaux_chateau",
    "proba_gain_droits_basseMoyenneJustice_chateau",
    "proba_promotion_groschateau_multipole",
    "proba_promotion_groschateau_autre",
    "puissance_necessaire_creation_chateau_GS",
    "puissance_necessaire_creation_chateau_PS",
    "nombre_eglises",
    "nb_eglises_paroissiales",
    "proba_gain_droits_paroissiaux",
    "nb_max_paroissiens",
    "nb_min_paroissiens",
    "ratio_paroissiens_agregats",
    "nb_paroissiens_mecontents_necessaires",
    "attrac_0_eglises",
    "attrac_1_eglises",
    "attrac_2_eglises",
    "attrac_3_eglises",
    "attrac_4_eglises",
    "attrac_GC",
    "attrac_PC",
    "attrac_communautes",
    "chateaux_GS_alternate",
    "chateaux_PS_alternate",
    "puissance_armee_FP_alternate",
    "communautes_attractives",
    "agregats_alternate",
    "poles_alternate",
    "agregats_alternate2",
    "recompute_agregats_at_end"
  )

results_name <- c(
  "seed",
  "Annee",
  "NbChateaux",
  "NbGrosChateaux",
  "NbEglises",
  "NbEglisesParoissiales",
  "DistPpvEglises",
  "DistPpvEglisesParoissiales",
  "TxFpIsoles",
  "ChargeFiscale",
  "DistPpvFpAgregat",
  "Duree"
)

seigneurs_name <-
  c(
    "seed",
    "Annee",
    "IdSeigneur",
    "Type",
    "Initial",
    "puissance",
    "NbChateauxProprio",
    "NbChateauxGardien",
    "NbFpAssujetis",
    "NbVassaux",
    "NbDebiteurs"
  )

agregats_name <-
  c(
    "seed",
    "Annee",
    "IdAgregat",
    "NbFpContenus",
    "NbFContenusAvantDem",
    "NbFpAttires",
    "Surface",
    "Communaute",
    "Geom"
  )

poles_name <- c(
  "seed",
  "Annee",
  "IdPole",
  "Attractivite",
  "NbAttracteurs",
  "Agregat",
  "NbEglises",
  "nbParoisses",
  "nbGC",
  "nbPC",
  "nbCA",
  "Geom"
)

FP_all_name <- c(
  "seed",
  "Annee",
  "nbInInIntra",
  "nbInOutIntra",
  "nbOutInIntra",
  "nbOutOutIntra",
  "nbInInInter",
  "nbInOutInter",
  "nbOutInInter",
  "nbOutOutInter"
)

FP_summary_name <- c(
  "seed",
  "Annee",
  "NbDeplacementLocaux",
  "NbDeplacementLointains",
  "NbSat25inf",
  "NbSat25_49",
  "NbSat50_75",
  "NbSat75sup"
)

paroisses_name <- c(
  "seed",
  "Annee",
  "IdParoisse",
  "monEglise",
  "Superficie",
  "NbParoissiens",
  "Satisfaction",
  "Geom"
)

new_parameters <-  read_csv(col_names = parameters_name, parameters_file)
new_results <-  read_csv(col_names = results_name, results_file)
new_seigneurs <-  read_csv(col_names = seigneurs_name, seigneurs_file)
new_agregats <-  read_csv(col_names = agregats_name, agregats_file)
new_poles <-  read_csv(col_names = poles_name, poles_file)
new_FP_all <-  read_csv(col_names = FP_all_name, FP_all_file)
new_FP_summary <-  read_csv(col_names = FP_summary_name, FP_summary_file)
new_paroisses <-  read_csv(col_names = paroisses_name, paroisses_file)


### On ne garde que les simulations complètes ###

new_goodSeeds <- new_parameters %>%
  left_join(new_results %>% filter(Annee == 1160) %>% select(seed, Annee),
            by = "seed") %>%
  filter(Annee == 1160) %>%
  group_by(name) %>%
  select(seed)


new_parameters <-  new_parameters %>% semi_join(new_goodSeeds, by = "seed")
new_results <- new_results %>% semi_join(new_goodSeeds, by = "seed")
new_seigneurs <-  new_seigneurs %>% semi_join(new_goodSeeds, by = "seed")
new_agregats <- new_agregats %>% semi_join(new_goodSeeds, by = "seed")
new_poles <- new_poles %>% semi_join(new_goodSeeds, by = "seed")
new_FP_all <- new_FP_all %>% semi_join(new_goodSeeds, by = "seed")
new_FP_summary <-  new_FP_summary %>% semi_join(new_goodSeeds, by = "seed")
new_paroisses <-  new_paroisses %>% semi_join(new_goodSeeds, by = "seed")


new_poles <- new_poles %>%
  mutate(Geom = gsub(x = Geom, pattern = "{", replacement="", perl = TRUE)) %>%
  mutate(Geom = gsub(x = Geom, pattern = "}", replacement="", perl = TRUE)) %>%
  separate(Geom, into=c("X", "Y", "Z"), sep = ",") %>%
  mutate_each(funs(as.numeric), X,Y,Z) %>%
  select(-Z)

new_agregats <- new_agregats %>%
  mutate(Geom = gsub(x = Geom, pattern = "{", replacement="", perl = TRUE)) %>%
  mutate(Geom = gsub(x = Geom, pattern = "}", replacement="", perl = TRUE)) %>%
  separate(Geom, into=c("X", "Y", "Z"), sep = ",") %>%
  mutate_each(funs(as.numeric), X,Y,Z) %>%
  select(-Z) %>%
  mutate(Communaute = as.logical(Communaute))

new_seigneurs <- new_seigneurs %>%
  mutate(Initial = as.logical(Initial))


new_results <- new_results %>%
  inner_join({
    new_agregats %>%
      group_by(seed, Annee) %>%
      summarise(NbAgregats = n())
  },
  by=c("seed", "Annee")
  ) %>%
  inner_join({
    filter(., Annee == 820) %>%
      mutate(CFinit = ChargeFiscale) %>%
      select(seed, CFinit)
  }, by="seed") %>%
  mutate(RatioChargeFiscale = ChargeFiscale / CFinit) %>%
  select(-CFinit)

new_paroisses_geom <- setDT(new_paroisses)[,strsplit(gsub('\\[{|}\\]','', Geom, perl=T), '}, *{', perl=T), .(seed, Annee, IdParoisse)] %>%
  separate(V1, into = c("X", "Y", "Z"), convert = TRUE,  sep = ",") %>%
  mutate(UID = id(list(seed, Annee, IdParoisse)))

rm(baseName, outputs_folder,
   agregats_file, agregats_name,
   FP_all_file, FP_all_name,
   FP_summary_file, FP_summary_name,
   parameters_file, parameters_name,
   paroisses_file, paroisses_name,
   poles_file, poles_name,
   results_file, results_name,
   seigneurs_file, seigneurs_name
   )

load("data/JIAP_data.Rdata")

JIAP_parameters <- JIAP_parameters %>% bind_rows(new_parameters) %>% distinct(seed, name, .keep_all = TRUE)
JIAP_results <- JIAP_results %>% bind_rows(new_results) %>% distinct(seed, Annee, .keep_all = TRUE)
JIAP_seigneurs <- JIAP_seigneurs %>% bind_rows(new_seigneurs) %>% distinct(seed, Annee, IdSeigneur, .keep_all = TRUE)
JIAP_agregats <- JIAP_agregats %>% bind_rows(new_agregats) %>% distinct(seed, Annee, IdAgregat, .keep_all = TRUE)
JIAP_poles <- JIAP_poles %>% bind_rows(new_poles) %>% distinct(seed, Annee, IdPole,.keep_all = TRUE)
JIAP_FP_all <- JIAP_FP_all %>% bind_rows(new_FP_all) %>% distinct(seed, Annee, .keep_all = TRUE)
JIAP_FP_summary <- JIAP_FP_summary %>% bind_rows(new_FP_summary) %>% distinct(seed, Annee, .keep_all = TRUE)
JIAP_paroisses <- JIAP_paroisses %>% bind_rows(new_paroisses) %>% distinct(seed, Annee, IdParoisse, .keep_all = TRUE)
JIAP_paroisses_geom <- JIAP_paroisses_geom %>% bind_rows(new_paroisses_geom) %>% distinct(seed, Annee, .keep_all = TRUE)
goodSeeds <- goodSeeds %>% bind_rows(new_goodSeeds) %>% distinct(seed, .keep_all = TRUE)


save(
  list = c(
    "JIAP_parameters",
    "JIAP_results",
    "JIAP_seigneurs",
    "JIAP_agregats",
    "JIAP_poles",
    "JIAP_FP_all",
    "JIAP_FP_summary",
    "JIAP_paroisses",
    "JIAP_paroisses_geom",
    "goodSeeds"
  ),
  file = "data/JIAP_data.Rdata"
)
