FP_TypeDeplacements <- function(FP_data){
  
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
}

output$FP_TypeDeplacements <- renderPlot({
  req(filtredHaut$FP)
  FP_TypeDeplacements(filtredHaut$FP)
})

output$FP_TypeDeplacements_filter <- renderPlot({
  req(filtredBas$FP)
  FP_TypeDeplacements(filtredBas$FP)
})

FP_DeplacementsDetail <- function(FP_data){
  
  
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
}

output$FP_DeplacementsDetail <- renderPlot({
  req(filtredHaut$FP)
  FP_DeplacementsDetail(FP_data = filtredHaut$FP)
})

output$FP_DeplacementsDetail_Filter <- renderPlot({
  req(filtredBas$FP)
  FP_DeplacementsDetail(FP_data = filtredBas$FP)
})

FP_Concentration <- function(results_data){
  concentration_data <- results_data %>%
    select(annee, prop_fp_isoles) %>%
    collect()
  
  ggplot(concentration_data, aes(factor(annee), prop_fp_isoles)) +
    geom_tufteboxplot() +
    ggtitle("Évolution de la part de FP isolés") +
    xlab("Temps") + ylab("Taux de FP isolés") +
    scale_y_continuous(labels = scales::percent) +
    labs(subtitle = "Variabilité : Réplications")
}

output$FP_Concentration <- renderPlot({
  req(filtredHaut$results)
  FP_Concentration(filtredHaut$results)
})

output$FP_Concentration_Filter <- renderPlot({
  req(filtredBas$results)
  FP_Concentration(filtredBas$results)
})

FP_Satisfaction <- function(FP_data){
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
    labs(subtitle = "Variabilité : Ensemble des réplications") +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(title.position = "top", nrow = 1, title.hjust = 0.5,
                               label.position = "bottom", label.hjust = 0.5))
}

output$FP_Satisfaction <- renderPlot({
  req(filtredHaut$FP)
  FP_Satisfaction(FP_data = filtredHaut$FP)
})

output$FP_Satisfaction_Filter <- renderPlot({
  req(filtredBas$FP)
  FP_Satisfaction(FP_data = filtredBas$FP)
})