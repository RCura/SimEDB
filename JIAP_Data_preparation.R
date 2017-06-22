suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))

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


JIAP_parameters <-
  read_csv(col_names = parameters_name, "../GAMA/transition8/outputs/4_4_A_parameters.csv")
JIAP_results <-
  read_csv(col_names = results_name, "JIAP_results_TMD.csv")
JIAP_seigneurs <-
  read_csv(col_names = seigneurs_name, "JIAP_results_TMD_seigneurs.csv")
JIAP_agregats <-
  read_csv(col_names = agregats_name, "JIAP_results_TMD_agregats.csv")
JIAP_poles <-
  read_csv(col_names = poles_name, "JIAP_results_TMD_poles.csv")
JIAP_FP_all <-
  read_csv(col_names = FP_all_name, "JIAP_results_TMD_FP_all.csv")
JIAP_FP_summary <-
  read_csv(col_names = FP_summary_name, "JIAP_results_TMD_summFP.csv")
JIAP_paroisses <-
  read_csv(col_names = paroisses_name, "JIAP_results_TMD_paroisses.csv")

rm(
  agregats_name,
  FP_all_name,
  FP_summary_name,
  parameters_name,
  paroisses_name,
  poles_name,
  results_name,
  seigneurs_name
)

### On ne garde que 20 repli par experience ###

goodSeeds <- JIAP_parameters %>%
  left_join(JIAP_results %>% filter(Annee == 1160) %>% select(seed, Annee),
            by = "seed") %>%
  filter(Annee == 1160) %>%
  group_by(name) %>%
  sample_n(20) %>%
  select(seed)

JIAP_parameters <-
  JIAP_parameters %>% semi_join(goodSeeds, by = "seed")
JIAP_results <- JIAP_results %>% semi_join(goodSeeds, by = "seed")
JIAP_seigneurs <-
  JIAP_seigneurs %>% semi_join(goodSeeds, by = "seed")
JIAP_agregats <- JIAP_agregats %>% semi_join(goodSeeds, by = "seed")
JIAP_poles <- JIAP_poles %>% semi_join(goodSeeds, by = "seed")
JIAP_FP_all <- JIAP_FP_all %>% semi_join(goodSeeds, by = "seed")
JIAP_FP_summary <-
  JIAP_FP_summary %>% semi_join(goodSeeds, by = "seed")
JIAP_paroisses <-
  JIAP_paroisses %>% semi_join(goodSeeds, by = "seed")


JIAP_poles <- JIAP_poles %>%
  mutate(Geom = gsub(x = Geom, pattern = "{", replacement="", perl = TRUE)) %>%
  mutate(Geom = gsub(x = Geom, pattern = "}", replacement="", perl = TRUE)) %>%
  separate(Geom, into=c("X", "Y", "Z"), sep = ",") %>%
  mutate_each(funs(as.numeric), X,Y,Z) %>%
  select(-Z)



JIAP_agregats <- JIAP_agregats %>%
  mutate(Geom = gsub(x = Geom, pattern = "{", replacement="", perl = TRUE)) %>%
  mutate(Geom = gsub(x = Geom, pattern = "}", replacement="", perl = TRUE)) %>%
  separate(Geom, into=c("X", "Y", "Z"), sep = ",") %>%
  mutate_each(funs(as.numeric), X,Y,Z) %>%
  select(-Z) %>%
  mutate(Communaute = as.logical(Communaute))

JIAP_seigneurs <- JIAP_seigneurs %>%
  mutate(Initial = as.logical(Initial))

JIAP_results <- JIAP_results %>%
  inner_join({
    JIAP_agregats %>%
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

JIAP_paroisses_geom <- setDT(JIAP_paroisses)[,strsplit(gsub('\\[{|}\\]','', Geom, perl=T), '}, *{', perl=T), .(seed, Annee, IdParoisse)] %>%
  separate(V1, into = c("X", "Y", "Z"), convert = TRUE,  sep = ",") %>%
  mutate(UID = id(list(seed, Annee, IdParoisse)))


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





JIAP_parameters %>% group_by(name) %>% summarise(nbRep = n())