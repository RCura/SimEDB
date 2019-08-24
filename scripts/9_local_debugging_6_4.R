options( java.parameters = c("-Xss2560k", "-Xmx7g") ) # Needed fix for rJava (JDBC) + ggplot2

source("packages.R")
source("src_plots/themes.R")


# drv <- JDBC("com.mapd.jdbc.MapDDriver",
#             "/data/user/c/rcura/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
#             identifier.quote="'")

drv2 <- JDBC("com.omnisci.jdbc.OmniSciDriver",
            "/data/user/c/rcura/omnisci-jdbc-4.7.1.jar",
            identifier.quote="'")


#conMapD <- dbConnect(drv2, "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")
conMapD <- dbConnect(drv2, "jdbc:omnisci:mapdi.cura.info:6274:omnisci", "admin", "HyperInteractive")

seeds <- tbl(conMapD, "seeds_6_4")
agregats <- tbl(conMapD, "agregats_6_4")
fp <- tbl(conMapD, "fp_6_4")
parameters <- tbl(conMapD, "parameters_6_4")
paroisses <- tbl(conMapD, "paroisses_6_4")
poles <- tbl(conMapD, "poles_6_4")
results <- tbl(conMapD, "global_6_4")
seigneurs <- tbl(conMapD, "seigneurs_6_4")
chateaux <- tbl(conMapD, "chateaux_6_4")

##############################################################
##############################################################
##############################################################

sample_sim_name <- "6_6_Scenarios_base"

FP_data <- fp %>% filter(sim_name %in% !!sample_sim_name)
results_data <- results %>% filter(sim_name %in% !!sample_sim_name)
agregats_data <- agregats %>% filter(sim_name %in% !!sample_sim_name)
poles_data <- poles %>% filter(sim_name %in% !!sample_sim_name)
paroisses_data <- paroisses %>% filter(sim_name %in% !!sample_sim_name)
seigneurs_data <- seigneurs %>% filter(sim_name %in% !!sample_sim_name)
parameters_data <- parameters %>%  filter(sim_name %in% !!sample_sim_name)
chateaux_data <- chateaux %>% filter(sim_name %in% !!sample_sim_name)

##############################################################
##############################################################
##############################################################


# blob <- results_data %>%
#   filter(annee == 1200) %>%
#   select(seed, nb_agregats)
# 
#   parameters_data %>%
#     filter(parametre == "init_nb_total_fp", valeur == "50000") %>%
#     select(seed) %>%
#     left_join(blob, by = "seed") %>%
#     collect() %>%
#     filter(!is.na(nb_agregats)) %>%
#     summarise(agr = mean(nb_agregats, na.rm = TRUE))



##############################################################
##############################################################
##############################################################


seed_growth <- parameters_data %>%
  filter(parametre == "init_nb_total_fp",
         valeur == "4000") %>%
  head(1) %>%
  select(seed, parametre, valeur)
  

seed_pop <- parameters_data %>%
  filter(parametre == "init_nb_total_fp",
         valeur == "50000") %>%
  head(1) %>%
  select(seed, parametre, valeur)

seigneurs_growth <- seigneurs_data %>%
  left_join(seed_growth, by = "seed") %>%
  filter(!is.na(parametre)) %>%
  mutate(init_nb_total_fp = valeur) %>%
  select(-parametre, -valeur) %>%
  select(-date_apparition, -c(nb_chateaux_proprio:geom)) %>%
  collect()

seigneurs_pop <- seigneurs_data %>%
  left_join(seed_pop, by = "seed") %>%
  filter(!is.na(parametre)) %>%
  mutate(init_nb_total_fp = valeur) %>%
  select(-parametre, -valeur) %>%
  select(-date_apparition, -c(nb_chateaux_proprio:geom)) %>%
  collect()

all_seigneurs <- seigneurs_growth %>%
  bind_rows(seigneurs_pop)

foo <- all_seigneurs %>%
  filter(annee >= 940) %>%
  select(-seed, -sim_name, -id_seigneur) %>%
  select(init_nb_total_fp, everything()) %>%
 arrange(init_nb_total_fp, annee, desc(puissance)) %>%
  group_by(init_nb_total_fp, annee, type) %>%
  mutate(rang_local = row_number()) %>%
  ungroup() %>%
  group_by(init_nb_total_fp, annee) %>%
  mutate(rang_global = row_number()) %>%
  ungroup() %>%
  mutate(ponderation_actuelle = if_else(type == "Grand Seigneur",
                                        true = 1.25,
                                        false = 7)) %>%
  mutate(max_chateaux = if_else(type == "Grand Seigneur",
                                true = 2,
                                false = 10)) %>%
  group_by(init_nb_total_fp, annee) %>%
  mutate(proba_actuelle = puissance / sum(puissance, na.rm = TRUE) * ponderation_actuelle) %>%
  ungroup() %>%
  mutate(nb_chateaux_actuels = proba_actuelle * max_chateaux) %>%
  group_by(init_nb_total_fp, annee) %>%
  mutate(cumsum_puissance_global = cumsum(puissance)) %>%
  group_by(init_nb_total_fp, annee, type) %>%
  mutate(cumsum_puissance_local = cumsum(puissance)) %>%
  ungroup() %>%
  group_by(init_nb_total_fp, annee, type) %>%
  mutate(puissance_local_kp1 = lead(cumsum_puissance_local, n = 1),
         puissance_local_kp2 = lead(cumsum_puissance_local, n = 2),
         puissance_local_kp3 = lead(cumsum_puissance_local, n = 3)) %>%
  ungroup() %>%
  mutate(puissance_local_kp1 = if_else(is.na(puissance_local_kp1),
                                       cumsum_puissance_local,
                                       puissance_local_kp1),
         puissance_local_kp2 = if_else(is.na(puissance_local_kp2),
                                       puissance_local_kp1,
                                       puissance_local_kp2),
         puissance_local_kp3 = if_else(is.na(puissance_local_kp3),
                                       puissance_local_kp2,
                                       puissance_local_kp3)
         ) %>%
  group_by(init_nb_total_fp, annee) %>%
  mutate(puissance_global_kp1 = lead(cumsum_puissance_global, n = 1),
         puissance_global_kp2 = lead(cumsum_puissance_global, n = 2),
         puissance_global_kp3 = lead(cumsum_puissance_global, n = 3)) %>%
  ungroup() %>%
  mutate(puissance_global_kp1 = if_else(is.na(puissance_global_kp1),
                                        cumsum_puissance_global,
                                        puissance_global_kp1),
         puissance_global_kp2 = if_else(is.na(puissance_global_kp2),
                                        puissance_global_kp1,
                                       puissance_global_kp2),
         puissance_global_kp3 = if_else(is.na(puissance_global_kp3),
                                        puissance_global_kp2,
                                       puissance_global_kp3)
  ) %>%
  mutate(proba_locale_kp1 = puissance / puissance_local_kp1 * max_chateaux,
         proba_locale_kp2 = puissance / puissance_local_kp2 * max_chateaux,
         proba_locale_kp3 = puissance / puissance_local_kp3 * max_chateaux,
         proba_globale_kp1 = puissance / puissance_global_kp1 * max_chateaux,
         proba_globale_kp2 = puissance / puissance_global_kp2 * max_chateaux,
         proba_globale_kp3 = puissance / puissance_global_kp3 * max_chateaux)

foobar <- foo %>%
  group_by(init_nb_total_fp, annee, type) %>%
  summarise(proba_actuelle = sum(nb_chateaux_actuels, na.rm = TRUE),
            proba_locale_kp1 = sum(proba_locale_kp1, na.rm = TRUE),
            proba_locale_kp2 = sum(proba_locale_kp2, na.rm = TRUE),
            proba_locale_kp3 = sum(proba_locale_kp3, na.rm = TRUE),
            proba_globale_kp1 = sum(proba_globale_kp1, na.rm = TRUE),
            proba_globale_kp2 = sum(proba_globale_kp2, na.rm = TRUE),
            proba_globale_kp3 = sum(proba_globale_kp3, na.rm = TRUE)) %>%
  gather(type_proba, sum_proba, -init_nb_total_fp, -annee, -type)


plot_proba_tout_par_tour <- foobar %>%
  #filter(str_detect(type_proba, pattern = "proba_locale_")) %>%
  ggplot() +
  aes(annee, sum_proba, colour = type_proba) +
  geom_line(position=position_jitter(w=0.02, h=0.03)) +
  #geom_point(position = pd) +
  facet_grid(type~init_nb_total_fp) +
  scale_colour_colorblind()

ggsave(plot_proba_tout_par_tour,
       filename = "plot_probas_pas-de-temps.png",
       width = 30, height = 20, units = "cm", dpi = 200)


foobar %>%
  group_by(type_proba, type, init_nb_total_fp) %>%
  summarise(total_proba = sum(sum_proba)) %>%
  ungroup() %>%
  group_by(type_proba, init_nb_total_fp) %>%
  mutate(total_proba_ensemble = sum(total_proba)) %>%
  ungroup() %>%
  gather(type_total, valeur_total, -type_proba, -type, -init_nb_total_fp) -> test

plot_cumul_probas <- test %>% 
  filter(type_total == "total_proba_ensemble") %>%
  filter(type == "Grand Seigneur") %>%
  mutate(type = "Ensemble") %>%
  bind_rows(
    test %>% filter(type_total != "total_proba_ensemble")
  ) %>%
  ggplot() +
  aes(type_proba, valeur_total, fill = type_proba) +
  geom_col() +
  facet_grid(init_nb_total_fp~type, labeller = labeller(init_nb_total_fp = label_both)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_colorblind() +
  guides(fill=FALSE) +
  scale_y_log10()

ggsave(plot_cumul_probas,
       filename = "plot_proba_chateaux_cumulees.png",
        width = 30, height = 20, units = "cm", dpi = 200)

plot_puissances <- all_seigneurs %>%
  filter(annee >= 940) %>%
  group_by(annee, init_nb_total_fp, type) %>%
  summarise(nb = n(),
            sum_puissance = sum(puissance, na.rm = TRUE)) %>%
  ggplot() +
  aes(annee, sum_puissance, colour = type) +
  geom_line() +
  geom_point() +
  facet_wrap(~init_nb_total_fp, labeller = labeller(init_nb_total_fp=label_both))

ggsave(plot_puissances,
       filename = "plot_puissances_cumulees.png",
       width = 30, height = 20, units = "cm", dpi = 200)

distrib_puissance <- all_seigneurs %>%
  filter(annee %in% c(820, 900, 1000, 1100, 1200)) %>%
  arrange(desc(puissance)) %>%
  group_by(init_nb_total_fp, annee) %>%
  mutate(rank = row_number(),
         puissance_cum = cumsum(puissance),
         part_puissance_cum = puissance_cum/max(puissance_cum)) %>%
  ungroup()

plot_rt <- ggplot(distrib_puissance) +
  aes(rank, puissance) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_grid(init_nb_total_fp ~ annee)

ggsave(plot_rt,
       filename = "plot_distribution_puissances_rang-taille.png",
       width = 30, height = 20, units = "cm", dpi = 200)


plot_puissance_cum <- ggplot(distrib_puissance) +
    aes(rank, part_puissance_cum) +
    geom_line() +
    geom_point() +
    facet_grid(init_nb_total_fp ~ annee) +
    scale_y_continuous(labels = scales::percent, limits = c(0,1))


ggsave(plot_puissance_cum,
       filename = "plot_cumul_puissances.png",
       width = 30, height = 20, units = "cm", dpi = 200)

###########################

dbDisconnect(conn = conMapD)
