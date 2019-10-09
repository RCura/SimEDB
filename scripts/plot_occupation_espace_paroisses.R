random_seeds <- paroisses_data %>%
  select(seed) %>%
  group_by(seed) %>%
  tally() %>%
  head(1) %>%
  pull(seed)

grille_monde <- paroisses_data %>%
  filter(seed %in% random_seeds) %>%
  filter(annee == 820) %>%
  select(id_paroisse, geom) %>%
  collect() %>%
  st_as_sf(wkt = "geom")%>%
  summarise() %>%
  st_make_grid(n = 20) %>%
  st_sf() %>%
  mutate(id = row_number())


mesparoisses <- paroisses_data %>%
  select(seed, annee, geom) %>%
  collect() %>%
  st_as_sf(wkt = "geom") %>%
  st_centroid() %>%
  st_join(grille_monde)

p1 <- mesparoisses %>%
  st_drop_geometry() %>%
  group_by(seed, annee, id) %>%
  tally() %>%
  group_by(seed, annee) %>%
  summarise(taux_carreaux = n() / nrow(grille_monde)) %>%
  ggplot() +
  aes(factor(annee), taux_carreaux) +
  geom_tufteboxplot() + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1.0, by = 0.2),
                     minor_breaks = seq(0.1, 0.9, by = 0.2), expand = FALSE) +
  labs(
    x = "Temps",
    y= "Part du carroyage contenant une église paroissiale",
    title = "Évolution de la desserte paroissiale",
    subtitle = "Le monde est discrétisé en 400 carreaux (20*20).\nOn représente la proportion de carreaux qui contiennent au moins une église paroissiale"
  ) +
  theme_simedb()

p1

ggsave(p1, filename = "plot_desserte_paroissiale.pdf", width = 20, height = 10, units = "cm")



