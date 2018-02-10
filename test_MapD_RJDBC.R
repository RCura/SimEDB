suppressPackageStartupMessages({
  # Data wrangling
  library(tidyverse)
  library(magrittr)
  library(stringr)
  library(forcats)
  
  # DataBase
  library(dbplyr)
  library(DBI)
  library(RJDBC) # devtools::install_github("hannesmuehleisen/MonetDBLite")
  
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

drv <- JDBC("com.mapd.jdbc.MapDDriver",
            "/opt/mapd/bin/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
            identifier.quote="'")
conMapD <- dbConnect(drv, "jdbc:mapd:localhost:9091:mapd", "mapd", "mapd")

seeds <- tbl(conMapD, "seeds")
agregats <- tbl(conMapD, "agregats")
fp <- tbl(conMapD, "fp")
parameters <- tbl(conMapD, "parameters")
paroisses <- tbl(conMapD, "paroisses")
poles <- tbl(conMapD, "poles")
results <- tbl(conMapD, "results")
seigneurs <- tbl(conMapD, "seigneurs")

##############################################################
##############################################################
##############################################################

FP_data <- fp %>% filter(sim_name %in% c("4_4_A", "4_5_A"))
results_data <- results %>% filter(sim_name %in% c("4_4_A", "4_5_A"))
agregats_data <- agregats %>% filter(sim_name %in% c("4_4_A", "4_5_A"))
poles_data <- poles %>% filter(sim_name %in% c("4_4_A", "4_5_A"))
paroisses_data <- paroisses %>% filter(sim_name %in% c("4_4_A", "4_5_A"))
seigneurs_data <- seigneurs %>% filter(sim_name %in% c("4_4_A", "4_5_A"))

##############################################################
##############################################################
##############################################################


nombre_FP_total <- FP_data %>%
  group_by(seed, sim_name, annee) %>%
  summarise(n_total = n())


types_deplacements <- FP_data %>%
  filter(!(type_deplacement %in% c("nil", "Non mobile"))) %>%
  group_by(annee, seed, sim_name, type_deplacement) %>%
  summarise(n = n()) %>%
  left_join(nombre_FP_total, by = c("seed", "annee", "sim_name")) %>%
  mutate(Tx = (n + 1E-12) / (n_total + 1E-12)) %>%
  ungroup() %>%
  collect()

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



##############################################################
##############################################################
##############################################################s

nombre_FP_total <- FP_data %>%
  filter(annee %in% c(820, 940, 1040, 1160)) %>%
  group_by(seed, sim_name, annee) %>%
  summarise(n_total = n())

details_deplacement <- FP_data %>%
  filter(annee %in% c(820, 940, 1040, 1160)) %>%
  group_by(seed, sim_name, annee, deplacement_from, deplacement_to) %>%
  summarise(nb_fp = n()) %>%
  left_join(nombre_FP_total, by = c("seed", "annee", "sim_name")) %>%
  select(-sim_name) %>%
  mutate(tx_fp = (nb_fp + 1E-12) / (n_total + 1E-12)) %>%
  group_by(annee, deplacement_from, deplacement_to) %>%
  summarise(nb_fp = mean(nb_fp + 1E-12),
            tx_fp = mean(tx_fp + 1E-12)) %>%
  ungroup() %>%
  collect() %>%
  filter(deplacement_from != "nil") %>%
  filter(deplacement_to != "nil") %>%
  mutate(deplacement_from = gsub(x = deplacement_from, pattern = "agregat", replacement = "Origine : Agrégat")) %>%
  mutate(deplacement_from = gsub(x = deplacement_from, pattern = "isole", replacement = "Origine : Isolé")) %>%
  mutate(deplacement_to = gsub(x = deplacement_to, pattern = "agregat", replacement = "agrégat")) %>%
  mutate(deplacement_to = gsub(x = deplacement_to, pattern = "pole", replacement = "pôle")) %>%
  mutate(deplacement_to = paste0(toupper(substr(deplacement_to, 1, 1)), substr(deplacement_to, 2, nchar(deplacement_to))))


ggplot(details_deplacement, aes(deplacement_to, tx_fp, fill = deplacement_to, group = deplacement_to)) + 
  geom_col(position = "dodge") +
  facet_grid(deplacement_from ~ annee, scales = "free_x") +
  xlab("Temps") + ylab("Part (moyenne) des Foyers Paysans") +
  scale_y_continuous(labels = percent) +
  ggtitle("Détail du type de déplacement des Foyers Paysans") +
  scale_fill_discrete(name = "Choix de la destination") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top")) +
  labs(subtitle = "Variabilité : Moyenne des réplications")

##############################################################
##############################################################
##############################################################

concentration_data <- results_data %>%
  select(annee, prop_fp_isoles) %>%
  collect()

ggplot(concentration_data, aes(factor(annee), prop_fp_isoles)) +
  geom_tufteboxplot() +
  ggtitle("Évolution de la part de FP isolés") +
  xlab("Temps") + ylab("Taux de FP isolés") +
  scale_y_continuous(labels = scales::percent) +
  labs(subtitle = "Variabilité : Réplications")

##############################################################
##############################################################
##############################################################

FP_satis_data <- FP_data %>%
  select(annee, satis, smat, srel, sprot)

nbFP <- FP_satis_data %>%
  select(annee) %>%
  group_by(annee) %>%
  summarise(nbtotal = n()) %>%
  collect()

globale <- FP_satis_data %>%
  group_by(annee, satis) %>%
  summarise(nb = n(), satisfaction = as.integer(satis * 10)) %>%
  group_by(annee, satisfaction) %>%
  summarise(nb = sum(nb)) %>%
  ungroup() %>%
  collect() %>%
  mutate(type = "Globale") %>%
  select(annee, nb, type, satisfaction) %>%
  mutate(satisfaction = satisfaction / 10)

materielle <- FP_satis_data %>%
  group_by(annee, smat) %>%
  summarise(nb = n(), satisfaction = as.integer(smat * 10)) %>%
  group_by(annee, satisfaction) %>%
  summarise(nb = sum(nb)) %>%
  ungroup() %>%
  collect() %>%
  mutate(type = "Matérielle") %>%
  select(annee, nb, type, satisfaction) %>%
  mutate(satisfaction = satisfaction / 10)

protection <- FP_satis_data %>%
  group_by(annee, sprot) %>%
  summarise(nb = n(), satisfaction = as.integer(sprot * 10)) %>%
  group_by(annee, satisfaction) %>%
  summarise(nb = sum(nb)) %>%
  ungroup() %>%
  collect() %>%
  mutate(type = "Protection") %>%
  select(annee, nb, type, satisfaction) %>%
  mutate(satisfaction = satisfaction / 10)

religieuse <- FP_satis_data %>%
  group_by(annee, srel) %>%
  summarise(nb = n(), satisfaction = as.integer(srel * 10)) %>%
  group_by(annee, satisfaction) %>%
  summarise(nb = sum(nb)) %>%
  ungroup() %>%
  collect() %>%
  mutate(type = "Religieuse") %>%
  select(annee, nb, type, satisfaction) %>%
  mutate(satisfaction = satisfaction / 10)



satisfaction_plotdata <- globale %>%
  union_all(materielle) %>%
  union_all(protection) %>%
  union_all(religieuse) %>%
  ungroup() %>%
  left_join(nbFP, by = c("annee")) %>%
  ungroup() %>%
  mutate(tx_fp = (nb) / (nbtotal)) %>%
  group_by(annee, type, satisfaction) %>%
  summarise(tx_fp = mean(tx_fp)) %>%
  ungroup() %>%
  collect()


ggplot(satisfaction_plotdata) +
  geom_col(aes(factor(annee), tx_fp, fill = factor(satisfaction), group = factor(satisfaction))) +
  facet_grid(type~.) +
  scale_fill_brewer(name = "Satisfaction", type = "div", palette = "RdYlBu", direction = 1) +
  scale_y_continuous(labels = percent) +
  ggtitle("Évolution de la satisfaction des foyers paysans") +
  xlab("Temps") + ylab("Distribution de la satisfaction des foyers paysans") +
  labs(subtitle = "Variabilité : Moyenne des réplications") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top", nrow = 1, title.hjust = 0.5,
                             label.position = "bottom", label.hjust = 0.5))


##################################################
##################################################
##################################################
##################################################
##################################################


nombre_agregats <- agregats_data %>%
  group_by(annee, seed) %>%
  summarise(nb = n()) %>%
  collect()

ggplot(nombre_agregats, aes(factor(annee), nb)) +
  geom_tufteboxplot() +
  xlab("Temps") + ylab("Nombre d'agrégats") +
  ggtitle("Évolution du nombre d'agrégats") +
  labs(subtitle = "Variabilité : Réplications")


##############################################################################

nbAgregats <- agregats_data %>%
  group_by(seed, sim_name, annee) %>%
  summarise(nb_agregats = n())

avecPoles <- agregats_data %>%
  filter(!is.na(monpole)) %>%
  mutate(pole = TRUE) %>%
  group_by(seed, annee) %>%
  summarise(nb_poles = n())

txAgregatsPoles <- nbAgregats %>%
  left_join(avecPoles, by = c("seed", "annee")) %>%
  mutate(tx_agregat_pole = (nb_poles + 1E-12) / (nb_agregats + 1E-12)) %>%
  collect()


ggplot(txAgregatsPoles, aes(annee, tx_agregat_pole, group = factor(annee))) +
  geom_tufteboxplot() +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  xlab("Temps") + ylab("Taux d'agrégats\n contenant un pôle") +
  ggtitle("Évolution du taux d'agrégats avec pôle") +
  labs(subtitle = "Variabilité : Réplications")


##############################################################################

nombre_agregats <- agregats_data %>%
  filter(communaute == 1) %>%
  group_by(annee, seed) %>%
  summarise(nb = n()) %>%
  collect()

ggplot(nombre_agregats, aes(factor(annee), nb)) +
  geom_tufteboxplot() +
  xlab("Temps") + ylab("Nombre d'agrégats") +
  ggtitle("Évolution du nombre d'agrégats ayant une CA") +
  labs(subtitle = "Variabilité : Réplications")


##############################################################################

rtAgregats <- agregats_data %>%
  filter(annee %in% c(820, 940, 1040, 1160)) %>%
  collect() %>%
  group_by(seed, annee) %>%
  mutate(rank = min_rank(-nbfp)) %>%
  group_by(annee, rank) %>%
  summarise(Moyenne = mean(nbfp, na.rm = TRUE),
            Q1 = quantile(nbfp, probs = 0.25),
            Q3 = quantile(nbfp, probs = 0.75)) %>%
  gather(key = `Méthode d'agrégation`, value = Value, Moyenne:Q3) %>%
  ungroup()

ggplot(rtAgregats, aes(rank, Value, group = `Méthode d'agrégation`, colour = `Méthode d'agrégation`)) +
  geom_line(size = 0.3, linetype = "dotted") +
  geom_point(size = 0.3) +
  scale_color_manual(values = c("black", "red", "blue")) +
  facet_grid(~annee, space = "free_x",  scales = "free_x") +
  scale_x_log10() + scale_y_log10() +
  ggtitle("Évolution rang-taille de la composition des agrégats") +
  theme(legend.position = "bottom") +
  xlab("Rang (log10)") + ylab("Nombre de FP\ncontenus (log10)") +
  labs(subtitle = "Variabilité : Moyenne, Q1 et Q3 des Réplications")


##############################################################################


nbAgregats <- agregats_data %>%
  group_by(seed, sim_name, annee) %>%
  summarise(nb_agregats = n()) %>%
  ungroup()%>%
  collect()

agregatsData <- poles_data %>%
  filter(!is.na(monagregat)) %>%
  filter(nbparoisses >= 1) %>%
  group_by(sim_name, annee, seed, monagregat) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(annee, seed, sim_name) %>%
  summarise(nb_agregats_paroisse = sum(n)) %>%
  ungroup() %>%
  collect()


agregatsParoisses <- nbAgregats %>%
  left_join(agregatsData, by =c("seed", "annee", "sim_name")) %>%
  mutate(tx_agregats_paroisses = (nb_agregats_paroisse + 1E-12) / (nb_agregats + 1E-12) * 100) %>%
  gather(key = Type, value = Value, nb_agregats_paroisse, tx_agregats_paroisses) %>%
  mutate(Value = if_else(is.na(Value), 0, Value)) %>%
  mutate(Type = if_else(Type == "nb_agregats_paroisse", "Nombre", "Taux (en %)"))

ggplot(agregatsParoisses, aes(factor(annee), Value)) +
  geom_tufteboxplot() +
  facet_grid(Type~., scales = "free_y") +
  xlab("Temps") + ylab("Agrégats contenant au moins une paroisse") +
  ggtitle("Évolution du nombre d'agrégats contenant au moins une paroisse") +
  labs(subtitle = "Variabilité : Réplications")


##################################################
##################################################
##################################################
##################################################
##################################################


nombre_paroisses <- paroisses_data %>%
  group_by(annee, seed) %>%
  summarise(nb = n()) %>%
  collect()

ggplot(nombre_paroisses, aes(factor(annee), nb)) +
  geom_tufteboxplot() +
  xlab("Temps") + ylab("Nombre de paroisses") +
  ggtitle("Évolution du nombre de paroisses") +
  labs(subtitle = "Variabilité : Réplications")


##################################################


fidelesBreaks <- c(-1,0,10,30,50,100,1000)
fidelesLabels <- c("0", "1-10", "11-30", "31-50", "51-100", ">100")

paroisses_breaks <- paroisses_data %>%
  filter(annee %in% c(820, 940, 1040, 1160)) %>%
  collect() %>%
  mutate(NbFidelesBreaks = cut(nbfideles, breaks = fidelesBreaks, labels = fidelesLabels)) %>%
  group_by(seed, annee, NbFidelesBreaks) %>%
  summarise(NbParoisses = n())

ggplot(paroisses_breaks, aes(factor(NbFidelesBreaks), NbParoisses)) +
  geom_tufteboxplot() +
  facet_wrap(~annee, scales = "free") +
  xlab("Nombre de paroissiens") + ylab("Fréquence") +
  ggtitle("Evolution de la composition des paroisses") +
  labs(subtitle = "Variabilité : Réplications")

##################################################


paroisses_promo <- paroisses_data %>%
  filter(!(mode_promotion %in% c("nil", "initialisation"))) %>%
  group_by(seed, annee, mode_promotion) %>%
  summarise(nb = n()) %>%
  collect() %>%
  mutate(mode_promotion = case_when(
    mode_promotion == "creation agregat" ~ "Création dans un agrégat",
    mode_promotion == "creation isole" ~ "Création en zone peu dense",
    mode_promotion == "promotion isole" ~ "Promotion en zone peu dense")
  )

ggplot(paroisses_promo, aes(annee, nb, group = factor(annee))) +
  geom_tufteboxplot() +
  facet_wrap(~ mode_promotion, ncol = 1) +
  xlab("Temps") + ylab("Nombre de nouvelles paroisses\nà chaque pas de temps") +
  ggtitle("Évolution des modes de création de nouvelles paroisses") +
  labs(subtitle = "Variabilité : Réplications")

##################################################


superficieBreaks <- c(-1,1, 5, 10, 20,50, 100,500, 1E12)
superficieBreaks <- superficieBreaks * 1E6
superficieLabels <- c("<1", "1-5", "6-10", "11-20", "21-50", "51-100", "101-500", ">500")

paroisses_sup_breaks <-  paroisses_data %>%
  filter(annee %in% c(820, 940, 1040, 1160)) %>%
  collect() %>%
  mutate(NbSuperficiesBreaks = cut(area,
                                   breaks = superficieBreaks,
                                   labels = superficieLabels)) %>%
  group_by(seed, annee, NbSuperficiesBreaks) %>%
  summarise(NbParoisses = n())


ggplot(paroisses_sup_breaks, aes(factor(NbSuperficiesBreaks), NbParoisses)) +
  geom_tufteboxplot() +
  facet_wrap(~annee) +
  xlab("Superficie des paroisses (km²)") + ylab("Fréquence") +
  ggtitle("Évolution de la superficie des paroisses") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(subtitle = "Variabilité : Réplications")



##################################################
##################################################
##################################################
##################################################
##################################################


PolesTous <- poles_data %>%
  group_by(seed, annee) %>%
  summarise(nb_poles = n()) %>%
  collect()

ggplot(data = PolesTous, aes(factor(annee), nb_poles)) + 
  geom_tufteboxplot() +
  xlab("Temps") + ylab("Nombre de\npôles") +
  ggtitle("Évolution du nombre de pôles") +
  labs(subtitle = "Variabilité : Réplications")


##################################################

PolesTous <- poles_data %>%
  group_by(seed, annee) %>%
  summarise(nb_poles = n())

tempVar <- poles_data %>%
  filter(!is.na(monagregat)) %>%
  group_by(seed,annee) %>%
  summarise(nb_pole_ag = n())

PolesAgregats <- PolesTous %>%
  left_join(tempVar, by=c("seed", "annee")) %>%
  mutate(tx_ag = (nb_pole_ag + 1E-12) / (nb_poles + 1E-12)) %>%
  collect()

tousPoles <- ggplot(data = PolesAgregats, aes(factor(annee), nb_pole_ag)) + 
  geom_tufteboxplot() +
  xlab("Temps") + ylab("Nombre de pôles\nlocalisés dans un agrégat") +
  theme(axis.title.x=element_blank())

polesAgregats <- ggplot(data = PolesAgregats, aes(factor(annee), tx_ag)) +
  geom_tufteboxplot() +
  scale_y_continuous(labels = scales::percent) +
  xlab("Temps") + ylab("Taux de pôles\nlocalisés dans un agrégat") +
  theme(axis.title.x=element_blank())

grid.arrange(tousPoles, polesAgregats, bottom = 'Temps',
             top = "Évolution de la localisation des pôles
             Variabilité : Réplications")


##################################################

compoPoles <- poles_data %>%
  filter(annee %in% c(820, 940, 1040, 1160)) %>%
  group_by(seed, annee, nbattracteurs) %>%
  summarise(nb = n()) %>%
  collect()

ggplot(compoPoles, aes(factor(nbattracteurs), nb)) +
  geom_tufteboxplot() +
  facet_wrap(~annee, scales = "free", nrow = 1) +
  xlab("Nombre d'attracteurs") +
  ylab("Fréquence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Évolution du nombre d'attracteurs des pôles") +
  labs(subtitle = "Variabilité : Réplications")


##################################################


attracPoles <- poles_data %>%
  filter(annee %in% c(820, 940, 1040, 1160)) %>%
  group_by(seed, annee, attractivite) %>%
  summarise(nb = n()) %>%
  collect()

ggplot(attracPoles, aes(factor(attractivite), nb)) +
  geom_tufteboxplot() + 
  facet_wrap(~annee, scales = "free", nrow = 1) +
  xlab("Attractivité") +
  ylab("Fréquence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Évolution de l'attractivité des pôles") +
  labs(subtitle = "Variabilité : Réplications")


##################################################


rtPoles_data <- poles_data %>%
  filter(annee %in% c(820, 940, 1040, 1160)) %>%
  collect() %>%
  group_by(seed, annee) %>%
  mutate(rank = min_rank(-nbattracteurs)) %>%
  group_by(annee, rank) %>%
  collect() %>%
  summarise(Moyenne = mean(nbattracteurs),
            Q1 = quantile(nbattracteurs, probs = 0.25),
            Q3 = quantile(nbattracteurs, probs = 0.75)) %>%
  gather(key = `Méthode d'agrégation`, value = Value, Moyenne:Q3) %>%
  ungroup()

ggplot(rtPoles_data, aes(rank, Value,
                         group = `Méthode d'agrégation`,
                         colour = `Méthode d'agrégation`)) +
  geom_line(size = 0.3, linetype = "dotted") +
  geom_point(size = 0.3) +
  scale_color_manual(values = c("black", "red", "blue")) +
  facet_grid(~annee, space = "free_x",  scales = "free_x") +
  scale_x_log10() + scale_y_log10() +
  ggtitle("Évolution rang-taille de la composition des pôles") +
  theme(legend.position = "bottom") +
  xlab("Rang (log10)") + ylab("Nombre d'attracteurs (log10)") +
  labs(subtitle = "Variabilité : Moyenne, Q1 et Q3 des Réplications")


##################################################
##################################################
##################################################
##################################################
##################################################

nbSeigneurs <- seigneurs_data %>%
  filter(!(type %in% "Grand Seigneur")) %>%
  group_by(seed, annee, type) %>%
  summarise(n = n()) %>%
  collect()

ggplot(nbSeigneurs, aes(factor(annee), col = type, fill = type, y = n)) +
  geom_tufteboxplot() +
  scale_color_discrete(name = "Type de seigneur") +
  scale_fill_discrete(name = "Type de seigneur") +
  ylab("Nombre de seigneurs") +
  xlab("Temps") +
  labs(title = "Évolution du nombre de seigneurs",
       subtitle = "Variabilité : Réplications")


##################################################



breaksGS <- c(-0,1,2,3,4,5,10,25,50,1000)
labelsGS <- c("1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")

# On ne garde que : les GS, les chatelains, et les PS/Chatelains inits

GS <- seigneurs_data %>%
  filter(annee == 1160, type == "Grand Seigneur") %>%
  collect() %>%
  rename(`Propriétés` = nbchateauxproprio, Gardiennage = nbchateauxgardien) %>%
  gather(key = TypePossession, value = NbChateaux, `Propriétés`, Gardiennage) %>%
  xtabs(formula = ~ seed + type + TypePossession + NbChateaux) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  tbl_df() %>%
  mutate(NbChateaux = as.numeric(NbChateaux)) %>%
  filter(NbChateaux > 0) %>%
  mutate(NbChateauxBreaks =  cut(NbChateaux, breaks = breaksGS, labels =  labelsGS)) %>%
  group_by(seed, NbChateauxBreaks, TypePossession, type) %>%
  summarise(NbSeigneurs = sum(Freq))

breaksChat <- c(-1,0,1,2,3,4,1000)
labelsChat <- c("0", "1", "2", "3", "4", "5+")

Chat <- seigneurs_data %>%
  filter(annee == 1160, type == "Chatelain") %>%
  collect() %>%
  rename(`Propriétés` = nbchateauxproprio, Gardiennage = nbchateauxgardien) %>%
  gather(key = TypePossession, value = NbChateaux, `Propriétés`, Gardiennage) %>%
  xtabs(formula = ~ seed + type + TypePossession + NbChateaux) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  tbl_df() %>%
  mutate(NbChateaux = as.numeric(NbChateaux)) %>%
  filter(NbChateaux > 0) %>%
  mutate(NbChateauxBreaks =  cut(NbChateaux, breaks = breaksChat, labels =  labelsChat)) %>%
  group_by(seed, NbChateauxBreaks, TypePossession, type) %>%
  summarise(NbSeigneurs = sum(Freq))

plotGS <- ggplot(data = GS, aes(NbChateauxBreaks, NbSeigneurs)) +
  geom_tufteboxplot() +
  facet_wrap(~TypePossession, scales = "free") +
  ggtitle("Grands Seigneurs") + 
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
  theme(plot.title = element_text(size = rel(1), face = "italic")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


plotChat <- ggplot(data = Chat, aes(NbChateauxBreaks, NbSeigneurs)) +
  geom_tufteboxplot() + 
  facet_wrap(~TypePossession, scales = "free") +
  ggtitle("Chatelains") + 
  theme(axis.title.y = element_blank(),axis.title.x = element_blank()) +
  theme(plot.title = element_text(size = rel(1), face = "italic")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(plotChat, plotGS, nrow = 2,
             bottom = "Nombre de châteaux", left = "Fréquence",
             top = "Distribution des possessions et gardiennages de châteaux
             Variabilité : Réplications"
)


##################################################



myBreaks <- c(-1,0,1,2,3,4,5,10,25,50,1000)
myLabels <- c("0","1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")

debiteurs_seigneurs <- seigneurs_data %>%
  filter(annee == 1160) %>%
  select(seed, annee, type, initial, nbdebiteurs) %>%
  collect() %>%
  mutate(initial = if_else(initial == 1, "Initialement\nPrésent", "Arrivé\nen cours")) %>%
  mutate(initial = factor(initial, levels = c("Arrivé\nen cours", "Initialement\nPrésent"))) %>%
  mutate(nbDebiteursBreaks =  cut(nbdebiteurs, breaks = myBreaks, labels =  myLabels)) %>%  
  group_by(seed, type, initial, nbDebiteursBreaks) %>%
  summarise(StatsDebiteurs = n())

debInitCPS <- debiteurs_seigneurs %>% filter(type != "Grand Seigneur", initial == "Initialement\nPrésent")
debNonCPS <- debiteurs_seigneurs %>% filter(type != "Grand Seigneur", initial == "Arrivé\nen cours")
debGS <- debiteurs_seigneurs %>% filter(type == "Grand Seigneur")

plotInitCPS <- ggplot(debInitCPS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
  geom_tufteboxplot() +
  facet_grid(initial ~ type, scales="free", space = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
  theme(plot.title = element_text(size = rel(1), face = "italic"))

plotNonCPS <- ggplot(debNonCPS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
  geom_tufteboxplot() +
  facet_grid(initial ~ type, scales="free", space = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
  theme(plot.title = element_text(size = rel(1), face = "italic"))

plotGS <- ggplot(debGS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
  geom_tufteboxplot() +
  facet_wrap(~type) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
  theme(plot.title = element_text(size = rel(1), face = "italic"))

lay <- rbind(c(1,1,1,3),
             c(2,2,2,3))

grid.arrange(plotNonCPS, plotInitCPS, plotGS, nrow = 1, layout_matrix = lay,
             bottom = "Nombre de Vassaux", left = "Fréquence",
             top = "Distribution du nombre de vassaux selon les types de seigneurs
             Variabilité : Réplications")


##################################################


redevances_seigneurs <- seigneurs_data %>%
  filter(annee == 1160) %>%
  select(seed, annee, type, nbfpassujettis) %>%
  collect() %>%
  mutate(type = factor(type, levels = c("Petit Seigneur", "Chatelain", "Grand Seigneur")))

ggplot(redevances_seigneurs, aes(type, nbfpassujettis)) +
  geom_tufteboxplot() +
  scale_y_log10(breaks = c(10,50,100, 500,1000, 2000)) +
  xlab("Types de seigneurs") + ylab("Nombre de FP assujetis\n(Échelle logarithmique)") +
  ggtitle("Distribution des redevances en fin de simulation") +
  labs(subtitle = "Variabilité : Seigneurs et réplications")


##################################################


x <- "nbfpassujettis"
redevancesLevels <- c("0","1-5","6-15","16-30","30-100",">100")
redevancesBreaks <- rlang::exprs(
  .data[[x]] == 0 ~ "0",
  .data[[x]] <= 5 ~ "1-5",
  .data[[x]] <= 15 ~ "6-15",
  .data[[x]] <= 30 ~ "16-30",
  .data[[x]] <= 100 ~ "30-100",
  .data[[x]] > 100 ~ ">100"
)

redevances_PS <- seigneurs_data %>%
  filter(annee == 1160) %>%
  filter(!(type %in% "Grand Seigneur")) %>%
  select(seed, annee, type, nbfpassujettis) %>%
  collect() %>%
  mutate(type = factor(type, levels = c("Petit Seigneur", "Chatelain"))) %>%
  mutate(nbFP_cut = case_when(!!!redevancesBreaks)) %>%
  mutate(nbFP_cut =  factor(nbFP_cut, levels = redevancesLevels)) %>%
  group_by(seed, type,nbFP_cut) %>%
  summarise(N = n())

ggplot(redevances_PS, aes(nbFP_cut, N)) +
  geom_tufteboxplot() +
  facet_wrap(~type) +
  xlab("Nombre de FP assujetis\n(Échelle logarithmique)") +
  ylab("Nombre de seigneurs") +
  ggtitle("Distribution des redevances en fin de simulation") +
  labs(subtitle = "Variabilité : Réplications") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##################################################


seigneurs_puissance <- seigneurs_data %>%
  filter(puissance > 0) %>%
  group_by(seed, annee, type) %>%
  collect() %>%
  summarise(Q1 = quantile(puissance, 0.25),
            Mean = mean(puissance),
            Max = max(puissance)) %>%
  gather(TypeIndic, puissance, Q1:Max)

ggplot(seigneurs_puissance, aes(factor(annee), puissance)) +
  geom_tufteboxplot() +
  facet_grid(type~TypeIndic, scales = "free") +
  xlab("Temps") + ylab("Puissance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Évolution de puissance des seigneurs\n(Puissance > 0, ≈50% des seigneurs)") +
  labs(subtitle = "Variabilité : Réplications")


##################################################


x <- "nbAgregats"
nbAgregatsLevels <- c("0","1","2","3-5",">5")
nbAgregatsBreaks <- rlang::exprs(
  .data[[x]] == 0 ~ "0",
  .data[[x]] == 1 ~ "1",
  .data[[x]] == 2 ~ "2",
  .data[[x]] <= 5 ~ "3-5",
  .data[[x]] > 5 ~ ">5"
)

seigneurs_agregats <- seigneurs_data %>%
  filter(annee ==  1160) %>%
  group_by(seed, monagregat) %>%
  summarise(nbAgregats = n()) %>%
  collect() %>%
  mutate(NbAgregats = case_when(!!!nbAgregatsBreaks)) %>%
  mutate(NbAgregats = factor(NbAgregats, levels = nbAgregatsLevels)) %>%
  right_join(agregats_data %>%
               filter(annee == 1160) %>%
               select(seed, id_agregat) %>%
               collect(),
             by = c("seed", "monagregat" = "id_agregat")) %>%
  mutate(NbAgregats = fct_explicit_na(NbAgregats, "0")) %>%
  group_by(seed, NbAgregats) %>%
  summarise(NbCas = n())

ggplot(seigneurs_agregats, aes(factor(NbAgregats), NbCas)) +
  geom_tufteboxplot() +
  labs(x = "Nombre de seigneurs par agrégat",
       y = "Nombre d'agrégats",
       title = "Nombre de seigneurs par agrégat en fin de simulation",
       subtitle = "Variabilité : Réplications",
       caption = "N.B : Sont considérés comme dans un agrégat les seigneurs localisés à moins de 200m")


dbDisconnect(conn = conMapD)