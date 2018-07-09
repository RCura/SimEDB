con <- DBI::dbConnect(RSQLite::SQLite(), "data/outputs_TR8_indexSimName.sqlite")

DBI::dbListTables(con)

seeds <- tbl(con, "goodSeeds")
agregats <- tbl(con, "agregats")
paroisses <- tbl(con, "paroisses")
poles <- tbl(con, "poles")


goodSeeds <- seeds %>% filter(sim_name %in% "4_4_B") %>% collect()

oneSeed <- goodSeeds %>% head(1) %>% pull(seed) 

sim_agregats <- agregats %>% filter(sim_name %in% "4_4_B") %>% collect() %>%
  mutate(communaute = as.logical(communaute))
sim_paroisses <- paroisses %>% filter(sim_name %in% "4_4_B") %>% collect()
sim_poles <- poles %>% filter(sim_name %in% "4_4_B") %>% collect()

agregats <- sim_agregats %>% filter(seed == oneSeed) %>% filter(Annee == 1160)
paroisses <- sim_paroisses %>% filter(seed == oneSeed) %>% filter(Annee == 1140)
poles <- sim_poles %>% filter(seed == oneSeed) %>% filter(Annee == 1160)

poles %>% filter(nbParoisses >= 1)
poles %>% group_by(monAgregat) %>% count() -> blob

nbAgregats <- sim_agregats %>%
  group_by(seed, sim_name, Annee) %>%
  summarise(NbAgregats = n())

blob <- sim_poles %>%
  filter(!is.na(monAgregat)) %>%
  filter(nbParoisses >= 1) %>%
  group_by(sim_name, Annee, seed, monAgregat) %>%
  count() %>%
  group_by(Annee, seed, sim_name) %>%
  summarise(NbAgregatsParoisse = sum(n)) %>%
  right_join(nbAgregats) %>%
  mutate(TxAgregatsParoisses = NbAgregatsParoisse / NbAgregats) %>%
  gather(key = Type, value = Value, NbAgregatsParoisse, TxAgregatsParoisses) %>%
  mutate(Value = if_else(is.na(Value), 0, Value)) %>%
  mutate(Type = if_else(Type == "NbAgregatsParoisse", "Nombre", "Taux (en %)"))


ggplot(blob, aes(factor(Annee), Value)) +
  geom_tufteboxplot() +
  facet_grid(Type~., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Temps") + ylab("Agrégats contenant au moins une paroisse") +
  ggtitle("Évolution du nombre d'agrégats contenant au moins une paroisse") +
  labs(subtitle = "Variabilité : Réplications")

poles %>% filter(!is.na(monAgregat)) %>% filter(nbParoisses >= 1)


DBI::dbDisconnect(con)
