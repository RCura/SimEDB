foo <- sim_FP %>%
  group_by(Annee, seed, type_deplacement) %>%
  summarise(N = n()) %>%
  group_by(Annee, seed) %>%
  mutate(Tx = N / sum(N)) %>%
  filter(type_deplacement != "nil")

ggplot(foo, aes(Annee, Tx, col = type_deplacement)) + geom_smooth() + scale_y_continuous(labels = percent)


ggplot(sim_FP %>% filter(deplacement_from != "nil", deplacement_to != "nil"), aes(deplacement_from, fill = deplacement_to)) + geom_bar() + facet_grid(seed ~ Annee)


ggplot(sim_FP %>%
         filter(deplacement_from != "nil") %>%
         filter(deplacement_to != "nil") %>%
         filter(type_deplacement != "fixe") %>%
         filter(type_deplacement != "Non mobile"),
       aes(deplacement_to, fill = deplacement_to)) +
  geom_bar(position = position_stack()) +
  facet_grid(deplacement_from ~ Annee)

foo <- sim_FP %>%
  filter(deplacement_from != "nil") %>%
  filter(deplacement_to != "nil") %>%
  filter(type_deplacement != "fixe") %>%
  filter(type_deplacement != "Non mobile") %>%
  group_by(seed,Annee, deplacement_from, deplacement_to) %>%
  summarise(N = n()) %>%
  group_by(Annee, deplacement_from, deplacement_to) %>%
  summarise(N = mean(N))

ggplot(foo, aes(x= Annee, y = N, group = deplacement_to, col = deplacement_to)) +
  geom_line() +
  facet_grid(~ deplacement_from) +
  theme(legend.position = "bottom")


###### test avec DB #####

types_deplacements <- FP_data %>%
  filter(type_deplacement != "nil") %>%
  filter(type_deplacement != "Non mobile") %>%
  group_by(Annee, seed, type_deplacement) %>%
  summarise(N = n()) %>%
  group_by(Annee, seed) %>%
  summarise(Tx = N / sum(N)) %>%




ggplot(types_deplacements, aes(factor(Annee), Tx, col = type_deplacement)) +
  geom_tufteboxplot(size = 1) +
  geom_line() +
  facet_wrap(~ type_deplacement) +
  scale_y_continuous(labels = percent) +
  scale_color_discrete(guide = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="bottom") +
  xlab("Temps") + ylab("Part des Foyers Paysans") +
  ggtitle("Type de déplacement des Foyers Paysans") +
  labs(subtitle = "Variabilité : Foyers Paysans et Réplications")
