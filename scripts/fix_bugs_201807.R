#Concernant l'interface shiny d'exploration : ça marche bien ! Trois remarques :
  
# Il y a un problème avec l'axe des ordonnées du graphique présentant en taux (%) le nombre d'agrégats contenant au moins une paroisse.

agregats_data <- agregats 
poles_data <- poles
Agregats_Paroisses <- function(agregats_data, poles_data){
  
  nb_agregats <- agregats_data %>%
    group_by(seed, sim_name, annee) %>%
    summarise(nb_agregats = n()) %>%
    ungroup()%>%
    collect()
  
  # agregatsData <- poles_data %>%
  #   filter(!is.na(monagregat)) %>%
  #   filter(nbparoisses >= 1) %>%
  #   group_by(sim_name, annee, seed, monagregat) %>%
  #   summarise(n = n()) %>%
  #   ungroup() %>%
  #   group_by(annee, seed, sim_name) %>%
  #   summarise(nb_agregats_paroisse = sum(n)) %>%
  #   ungroup() %>%
  #   collect()
  
  nb_agregats_paroisses <- agregats %>%
    select(id_agregat, sim_name, seed, annee, monpole) %>%
    left_join(poles %>%
                select(sim_name, seed, annee, id_pole, monagregat, nbparoisses),
              by = c("sim_name", "seed", "annee", "monpole" = "id_pole")) %>%
    filter(nbparoisses >= 1) %>%
    group_by(seed, sim_name, annee) %>%
    summarise(nb_agregats_paroisses = n()) %>%
    collect()
  
  plot_data <-  nb_agregats %>%
    left_join(nb_agregats_paroisses, by = c("seed", "sim_name", "annee")) %>%
    mutate(taux_agregats = nb_agregats_paroisses / nb_agregats * 100) %>%
    select(-nb_agregats) %>%
    gather(key = Type, value = Value, nb_agregats_paroisses, taux_agregats) %>%
    mutate(Type = if_else(Type == "nb_agregats_paroisses", "Nombre", "Taux (en %)"))
  
  # agregatsParoisses <- nbAgregats %>%
  #   left_join(agregatsData, by =c("seed", "annee", "sim_name")) %>%
  #   mutate(tx_agregats_paroisses = (nb_agregats_paroisse + 1E-12) / (nb_agregats + 1E-12) * 100) %>%
  #   gather(key = Type, value = Value, nb_agregats_paroisse, tx_agregats_paroisses) %>%
  #   mutate(Value = if_else(is.na(Value), 0, Value)) %>%
  #   mutate(Type = if_else(Type == "nb_agregats_paroisse", "Nombre", "Taux (en %)"))
  # 
  ggplot(plot_data) +
    aes(annee, Value, group = factor(annee)) +
    geom_tufteboxplot() +
    facet_grid(Type~., scales = "free_y") +
    xlab("Temps") + ylab("Agrégats contenant au moins une paroisse") +
    ggtitle("Évolution du nombre d'agrégats contenant au moins une paroisse") +
    labs(subtitle = "Variabilité : Réplications")
}

# Graphique "Evolution de la composition des paroisses" : difficulté de lecture due au fait que deux graphiques comportent une modalité "0" et pas les deux autres.

# Les graphiques présentant la distribution des indicateurs de sortie sont, à mon avis, inutiles car regarder l'ensemble des 780 simulations n'a aucun sens thématiquement.
