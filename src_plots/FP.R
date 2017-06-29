FP_TypeDeplacements <- function(FP_data)({
  types_deplacements <- FP_data %>%
    group_by(Annee, seed, type_deplacement) %>%
    summarise(N = n()) %>%
    group_by(Annee, seed) %>%
    mutate(Tx = N / sum(N)) %>%
    filter(type_deplacement != "nil")
  
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
})

output$FP_TypeDeplacements <- renderPlot({
  FP_TypeDeplacements(sim_FP)
})

output$FP_TypeDeplacements_filter <- renderPlot({
  req(filtred$FP)
  FP_TypeDeplacements(filtred$FP)
})

FP_DeplacementsDetail <- function(FP_data){
  details_deplacement <- FP_data %>%
    filter(Annee %in% c(820, 940, 1040, 1160)) %>%
    group_by(seed, Annee, deplacement_from, deplacement_to) %>%
    summarise(NbFP = n()) %>%
    group_by(Annee, deplacement_from, deplacement_to) %>%
    summarise(NbFP = mean(NbFP, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(deplacement_from != "nil") %>%
    filter(deplacement_to != "nil") %>%
    mutate(deplacement_from = gsub(x = deplacement_from, pattern = "agregat", replacement = "Origine : Agrégat")) %>%
    mutate(deplacement_from = gsub(x = deplacement_from, pattern = "isole", replacement = "Origine : Isolé")) %>%
    mutate(deplacement_to = gsub(x = deplacement_to, pattern = "agregat", replacement = "agrégat")) %>%
    mutate(deplacement_to = gsub(x = deplacement_to, pattern = "pole", replacement = "pôle")) %>%
    mutate(deplacement_to = paste0(toupper(substr(deplacement_to, 1, 1)), substr(deplacement_to, 2, nchar(deplacement_to))))
  
  ggplot(details_deplacement, aes(deplacement_to, NbFP, fill = deplacement_to, group = deplacement_to)) + 
    geom_col(position = "dodge") +
    facet_grid(deplacement_from ~ Annee, scales = "free_x") +
    xlab("Temps") + ylab("Nombre (moyen) de Foyers Paysans") +
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
  FP_DeplacementsDetail(FP_data = sim_FP)
})

output$FP_DeplacementsDetail_Filter <- renderPlot({
  req(filtred$FP)
  FP_DeplacementsDetail(FP_data = filtred$FP)
})

FP_Concentration <- function(results_data){
  ggplot(results_data, aes(factor(Annee), prop_FP_isoles)) +
    geom_tufteboxplot() +
    ggtitle("Évolution de la part de FP isolés") +
    xlab("Temps") + ylab("Taux de FP isolés") +
    scale_y_continuous(labels = scales::percent) +
    labs(subtitle = "Variabilité : Réplications")
}

output$FP_Concentration <- renderPlot({
  FP_Concentration(sim_results)
})

output$FP_Concentration_Filter <- renderPlot({
  req(filtred$results)
  FP_Concentration(filtred$results)
})


FP_Satisfaction <- function(FP_data){
  satisfaction_data <- FP_data %>%
    select(Annee, sMat, sRel, sProt, Satis) %>%
    rename(
      Globale = Satis,
      Matérielle = sMat,
      Protection = sProt,
      Religieuse = sRel) %>%
    group_by(Annee) %>%
    sample_n(size = 4E3, replace = FALSE) %>%
    ungroup() %>%
    gather(key = Type, value = Satisfaction, -Annee)
  
  ggplot(satisfaction_data, aes(Annee, Satisfaction, col = Type, fill = Type)) +
    geom_violin(aes(group = factor(Annee))) +
    facet_wrap(~ Type) +
    geom_smooth(alpha = .3, se = FALSE, na.rm = TRUE) +
    theme(legend.position = "none") +
    ggtitle("Évolution de la satisfaction des FP\n(Échantillon de 4000 FP / an)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(subtitle = "Variabilité : Foyers Paysans et Réplications")
}

output$FP_Satisfaction <- renderPlot({
  FP_Satisfaction(FP_data = sim_FP)
})

output$FP_Satisfaction_Filter <- renderPlot({
  req(filtred$FP)
  FP_Satisfaction(FP_data = filtred$FP)
})