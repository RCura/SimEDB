# Tests

paroisses_data <- paroisses

fidelesBreaks <- c(-1,0,10,30,50,100,1000)
fidelesLabels <- c("0", "1-10", "11-30", "31-50", "51-100", ">100")

paroisses_breaks <- paroisses_data %>%
  filter(annee %in% c(820, 940, 1040, 1160)) %>%
  mutate(NbFidelesBreaks = case_when(
    nbfideles == 0 ~ "0",
    nbfideles <= 10 ~ "1-10",
    nbfideles <= 30 ~ "11-30",
    nbfideles <= 50 ~ "31-50",
    nbfideles <= 100 ~ "51-100",
    TRUE ~ ">100"
  )) %>%
  mutate(blob = as.numeric(NbFidelesBreaks))
  group_by(seed, annee, NbFidelesBreaks) %>%
  summarise(NbParoisses = n()) %>%
  collect()
  

ggplot(paroisses_breaks, aes(factor(NbFidelesBreaks), NbParoisses)) +
  geom_tufteboxplot() +
  facet_wrap(~annee, scales = "free") +
  xlab("Nombre de paroissiens") + ylab("Fréquence") +
  scale_x_discrete(drop = FALSE) +
  ggtitle("Evolution de la composition des paroisses") +
  labs(subtitle = "Variabilité : Réplications")

system.time({
  paroisses_data %>%
  filter(!(mode_promotion %in% c("nil", "initialisation"))) %>%
  group_by(seed, annee, mode_promotion) %>%
  summarise(nb = n()) %>%
  collect()
})


FP_data <- fp %>% filter(sim_name == '5_0')

nombre_FP_total <- FP_data %>%
  group_by(seed, sim_name, annee) %>%
  summarise(n_total = n())

system.time({
  FP_data %>%
    filter(!(type_deplacement %in% c("nil", "Non mobile"))) %>%
    group_by(annee, seed, sim_name, type_deplacement) %>%
    summarise(n = n()) %>%
    left_join(nombre_FP_total, by = c("seed", "annee", "sim_name")) %>%
    mutate(Tx = (n + 1E-12) / (n_total + 1E-12)) %>%
    ungroup() %>%
    show_query()
    collect()
})
