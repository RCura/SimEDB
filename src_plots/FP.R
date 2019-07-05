# FP_TypeDeplacements <- function(FP_data){
#     nombre_FP_total <- FP_data %>%
#       group_by(seed, sim_name, annee) %>%
#       summarise(n_total = n())
#     types_deplacements <- FP_data %>%
#       filter(!(type_deplacement %in% c("nil", "Non mobile"))) %>%
#       group_by(annee, seed, sim_name, type_deplacement) %>%
#       summarise(n = n()) %>%
#       left_join(nombre_FP_total, by = c("seed", "annee", "sim_name")) %>%
#       mutate(Tx = (n + 1E-12) / (n_total + 1E-12)) %>%
#       ungroup() %>%
#       collect()
#    p1 <- ggplot(types_deplacements, aes(factor(annee), Tx, col = type_deplacement)) +
#       geom_tufteboxplot(size = 1) +
#       geom_line() +
#       facet_wrap(~ type_deplacement) +
#       scale_y_continuous(labels = percent) +
#       scale_color_discrete(guide = FALSE) +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#       theme(legend.position = "bottom") +
#       xlab("Temps") + ylab("Part des Foyers Paysans") +
#       ggtitle("Type de déplacement des Foyers Paysans") +
#       labs(subtitle = "Variabilité : Foyers Paysans et Réplications")
#    p1
# 
# }
# 
# callModule(plotDownloadRate, paste0("FP_TypeDeplacements","_Haut"),
#            plotFun = reactive(
#              FP_TypeDeplacements(filtredHaut$FP) +
#                labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
#                theme(plot.caption = element_text(size = 6, hjust = 0))
#            ),
#            plotName = paste0("FP_TypeDeplacements","_Haut"),
#            user = input$userName,
#            seeds = filtredSeedsHaut_plotly())
# callModule(plotDownloadRate, paste0("FP_TypeDeplacements","_Bas"),
#            plotFun = reactive(
#              FP_TypeDeplacements(filtredBas$FP) +
#                labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
#                theme(plot.caption = element_text(size = 6, hjust = 0))
#            ),
#            plotName = paste0("FP_TypeDeplacements","_Bas"),
#            user = input$userName,
#            seeds = filtredSeedsBas_plotly())


FP_TypeDeplacements_agg <- function(FP_data){
    types_migrations <- FP_data %>%
    select(seed, sim_name, annee, deplacement_fixe, deplacement_lointain, deplacement_local) %>%
    mutate(`Deplacement (Total)` = 1 - (deplacement_fixe)) %>%
    rename(Lointain = deplacement_lointain,
           Local = deplacement_local) %>%
    select(-deplacement_fixe) %>%
    collect() %>%
    gather(key = type_migration, value = taux, -seed, -sim_name, -annee)
    

  p1 <- ggplot(types_migrations, aes(factor(annee), taux, col = type_migration)) +
    geom_tufteboxplot(size = 1) +
    geom_line() +
    facet_wrap(~ type_migration) +
    scale_y_continuous(labels = percent) +
    scale_color_discrete(guide = FALSE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "bottom") +
    xlab("Temps") + ylab("% de migration") +
    ggtitle("Migrations des foyers paysans") +
    labs(subtitle = "Variabilité : foyers paysans et réplications")
  p1
}


callModule(plotDownloadRate, paste0("FP_TypeDeplacements","_Haut"),
           plotFun = reactive(
             FP_TypeDeplacements_agg(filtredHaut$FP) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("FP_TypeDeplacements","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())
callModule(plotDownloadRate, paste0("FP_TypeDeplacements","_Bas"),
           plotFun = reactive(
             FP_TypeDeplacements_agg(filtredBas$FP) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("FP_TypeDeplacements","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())




# FP_DeplacementsDetail <- function(FP_data){
#   nombre_FP_total <- FP_data %>%
#     filter(annee %in% c(820, 960, 1060, 1200)) %>%
#     group_by(seed, sim_name, annee) %>%
#     summarise(n_total = n())
#   
#   details_deplacement <- FP_data %>%
#     filter(annee %in% c(820, 960, 1060, 1200)) %>%
#     group_by(seed, sim_name, annee, deplacement_from, deplacement_to) %>%
#     summarise(nb_fp = n()) %>%
#     left_join(nombre_FP_total, by = c("seed", "annee", "sim_name")) %>%
#     select(-sim_name) %>%
#     mutate(tx_fp = (nb_fp + 1E-12) / (n_total + 1E-12)) %>%
#     group_by(annee, deplacement_from, deplacement_to) %>%
#     summarise(nb_fp = mean(nb_fp + 1E-12),
#               tx_fp = mean(tx_fp + 1E-12)) %>%
#     ungroup() %>%
#     collect() %>%
#     filter(deplacement_from != "nil") %>%
#     filter(deplacement_to != "nil") %>%
#     mutate(deplacement_from = gsub(x = deplacement_from, pattern = "agregat", replacement = "Origine : Agrégat")) %>%
#     mutate(deplacement_from = gsub(x = deplacement_from, pattern = "isole", replacement = "Origine : Isolé")) %>%
#     mutate(deplacement_to = gsub(x = deplacement_to, pattern = "agregat", replacement = "agrégat")) %>%
#     mutate(deplacement_to = gsub(x = deplacement_to, pattern = "pole", replacement = "pôle")) %>%
#     mutate(deplacement_to = paste0(toupper(substr(deplacement_to, 1, 1)), substr(deplacement_to, 2, nchar(deplacement_to))))
#   
#   ggplot(details_deplacement, aes(deplacement_to, tx_fp, fill = deplacement_to, group = deplacement_to)) + 
#     geom_col(position = "dodge") +
#     facet_grid(deplacement_from ~ annee, scales = "free_x") +
#     xlab("Temps") + ylab("Part (moyenne) des Foyers Paysans") +
#     scale_y_continuous(labels = percent) +
#     ggtitle("Détail du type de déplacement des Foyers Paysans") +
#     scale_fill_discrete(name = "Choix de la destination") +
#     theme(axis.text.x = element_blank(),
#           axis.line.x = element_blank(),
#           axis.ticks.x = element_blank()) +
#     theme(legend.position = "bottom") +
#     guides(fill = guide_legend(title.position = "top")) +
#     labs(subtitle = "Variabilité : Moyenne des réplications")
# }
# 
# 
# callModule(plotDownloadRate, paste0("FP_DeplacementsDetail","_Haut"),
#            plotFun = reactive(
#              FP_DeplacementsDetail(filtredHaut$FP)
#            ),
#            plotName = paste0("FP_DeplacementsDetail","_Haut"),
#            user = input$userName,
#            seeds = filtredSeedsHaut_plotly())
# callModule(plotDownloadRate, paste0("FP_DeplacementsDetail","_Bas"),
#            plotFun = reactive(
#              FP_DeplacementsDetail(filtredBas$FP)
#            ),
#            plotName = paste0("FP_DeplacementsDetail","_Bas"),
#            user = input$userName,
#            seeds = filtredSeedsBas_plotly())

FP_DeplacementsDetail_agg <- function(FP_data){
  
  taux_deplacements <- FP_data %>%
    select(seed, sim_name, annee, deplacement_fixe) %>%
    filter(annee %in% c(820, 900, 1000, 1100, 1200)) %>%
    mutate(taux_deplacement = 1 - deplacement_fixe) %>%
    select(-deplacement_fixe)

  details_deplacement <- FP_data %>%
    select(seed, sim_name, annee, starts_with("from_")) %>%
    filter(annee %in% c(820, 900, 1000, 1100, 1200)) %>%
    left_join(taux_deplacements, by = c("seed", "sim_name", "annee")) %>%
    mutate(from_isole_to_agregat_local = from_isole_to_agregat_local / taux_deplacement,
           from_isole_to_agregat_lointain = from_isole_to_agregat_lointain / taux_deplacement,
           from_isole_to_pole_local_hors_agregat = from_isole_to_pole_local_hors_agregat / taux_deplacement,
           from_agregat_to_agregat_local = from_agregat_to_agregat_local / taux_deplacement,
           from_agregat_to_agregat_lointain = from_agregat_to_agregat_lointain / taux_deplacement,
           from_agregat_to_pole_local_hors_agregat = from_agregat_to_pole_local_hors_agregat / taux_deplacement) %>%
    rename_at(vars(starts_with("from_")), gsub, pattern = "_", replacement = ".") %>%
    collect() %>% # mis ici car bug sinon oO
    group_by(annee) %>%
    summarise_at(vars(starts_with("from.")), list(mean = mean, 
                                                  min = min,
                                                  max = max),
                 na.rm=TRUE) %>%
#    collect() %>%
    gather(type_deplacement, taux, -annee) %>%
    separate(type_deplacement, into = c("Origine", "To"), sep = ".to.") %>%
    mutate(Origine = gsub(Origine, pattern = "from.", replacement = "")) %>%
    mutate(Origine = if_else(Origine == "isole", "Isolé", "Agrégé")) %>%
    separate(col = To, into = c("To","Indicateur"), sep = "_") %>%
    mutate(To = case_when(
      To == "agregat.local" ~ "Agrégat local",
      To == "agregat.lointain" ~ "Agrégat lointain",
      To == "pole.local.hors.agregat" ~ "Pôle local hors agrégat",
      TRUE ~ as.character(To)
    )) %>%
    spread(Indicateur, taux)
  
  
  ggplot(details_deplacement, aes(To, mean, fill = To, group = To)) + 
    geom_col(position = "dodge") +
    geom_errorbar(aes(ymin = min, ymax = max), width=.2) +
    facet_grid(Origine ~ annee, scales = "free_x", labeller = labeller(.rows = label_both)) +
    xlab("Temps") + ylab("% des migrations\n(min, moyenne, max)") +
    scale_y_continuous(labels = percent) +
    ggtitle("Détail du type de migrations des foyers paysans") +
    scale_fill_discrete(name = "Destination choisie") +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank()) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(title.position = "top")) +
    labs(subtitle = "Variabilité : Moyenne/Min/Max des réplications")
}


callModule(plotDownloadRate, paste0("FP_DeplacementsDetail","_Haut"),
           plotFun = reactive(
             FP_DeplacementsDetail_agg(filtredHaut$FP) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("FP_DeplacementsDetail","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())
callModule(plotDownloadRate, paste0("FP_DeplacementsDetail","_Bas"),
           plotFun = reactive(
             FP_DeplacementsDetail_agg(filtredBas$FP) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("FP_DeplacementsDetail","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())

FP_Concentration <- function(results_data){
  concentration_data <- results_data %>%
    select(annee, prop_fp_isoles) %>%
    collect()
  
  ggplot(concentration_data, aes(factor(annee), prop_fp_isoles)) +
    geom_tufteboxplot() +
    ggtitle("Évolution de la part de foyers paysans isolés") +
    xlab("Temps") + ylab("Taux de foyers paysans isolés") +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    labs(subtitle = "Variabilité : Réplications")
}

callModule(plotDownloadRate, paste0("FP_Concentration","_Haut"),
           plotFun = reactive(
             FP_Concentration(filtredHaut$results) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("FP_Concentration","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())
callModule(plotDownloadRate, paste0("FP_Concentration","_Bas"),
           plotFun = reactive(
             FP_Concentration(filtredBas$results) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("FP_Concentration","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())


# FP_Satisfaction <- function(FP_data){
#   FP_satis_data <- FP_data %>%
#     select(annee, satisfaction, s_materielle, s_religieuse, s_protection)
#   
#   nbFP <- FP_satis_data %>%
#     select(annee) %>%
#     group_by(annee) %>%
#     summarise(nbtotal = n()) %>%
#     collect()
#   
#   globale <- FP_satis_data %>%
#     select(annee, satisfaction) %>%
#     group_by(annee, satisfaction) %>%
#     summarise(nb = n(),
#               satisfaction2 = as.integer(satisfaction * 10)) %>%
#     group_by(annee, satisfaction2) %>%
#     summarise(nb = sum(nb, na.rm =TRUE)) %>%
#     ungroup() %>%
#     collect() %>%
#     rename(satisfaction = satisfaction2) %>%
#     mutate(type = "Globale") %>%
#     select(annee, nb, type, satisfaction) %>%
#     mutate(satisfaction = satisfaction / 10)
#   
#   materielle <- FP_satis_data %>%
#     select(annee, s_materielle) %>%
#     group_by(annee, s_materielle) %>%
#     summarise(nb = n(), satisfaction = as.integer(s_materielle * 10)) %>%
#     group_by(annee, satisfaction) %>%
#     summarise(nb = sum(nb, na.rm = TRUE)) %>%
#     ungroup() %>%
#     collect() %>%
#     mutate(type = "Matérielle") %>%
#     select(annee, nb, type, satisfaction) %>%
#     mutate(satisfaction = satisfaction / 10)
#   
#   protection <- FP_satis_data %>%
#     select(annee, s_protection) %>%
#     group_by(annee, s_protection) %>%
#     summarise(nb = n(), satisfaction = as.integer(s_protection * 10)) %>%
#     group_by(annee, satisfaction) %>%
#     summarise(nb = sum(nb, na.rm = TRUE)) %>%
#     ungroup() %>%
#     collect() %>%
#     mutate(type = "Protection") %>%
#     select(annee, nb, type, satisfaction) %>%
#     mutate(satisfaction = satisfaction / 10)
#   
#   religieuse <- FP_satis_data %>%
#     select(annee, s_religieuse) %>%
#     group_by(annee, s_religieuse) %>%
#     mutate(s_religieuse = if_else(s_religieuse > 1.0, 1.0, s_religieuse)) %>%
#     summarise(nb = n(), satisfaction = as.integer(s_religieuse * 10)) %>%
#     group_by(annee, satisfaction) %>%
#     summarise(nb = sum(nb, na.rm = TRUE)) %>%
#     ungroup() %>%
#     collect() %>%
#     mutate(type = "Religieuse") %>%
#     select(annee, nb, type, satisfaction) %>%
#     mutate(satisfaction = satisfaction / 10)
#   
#   satisfaction_plotdata <- globale %>%
#     union_all(materielle) %>%
#     union_all(protection) %>%
#     union_all(religieuse) %>%
#     ungroup() %>%
#     left_join(nbFP, by = c("annee")) %>%
#     ungroup() %>%
#     mutate(tx_fp = (nb) / (nbtotal)) %>%
#     group_by(annee, type, satisfaction) %>%
#     summarise(tx_fp = mean(tx_fp)) %>%
#     ungroup() %>%
#     collect()
#   
#   
#   ggplot(satisfaction_plotdata) +
#     geom_col(aes(factor(annee), tx_fp, fill = fct_rev(factor(satisfaction)), group = fct_rev(factor(satisfaction)))) +
#     facet_grid(type~.) +
#     scale_fill_brewer(name = "Satisfaction", type = "div", palette = "RdYlBu", direction = -1) +
#     scale_y_continuous(labels = percent) +
#     ggtitle("Évolution de la satisfaction des foyers paysans") +
#     xlab("Temps") + ylab("Distribution de la satisfaction des foyers paysans") +
#     labs(subtitle = "Variabilité : Ensemble des réplications") +
#     theme(legend.position = "bottom") +
#     guides(fill = guide_legend(title.position = "top", nrow = 1, title.hjust = 0.5,
#                                label.position = "bottom", label.hjust = 0.5))
# }
# 
# callModule(plotDownloadRate, paste0("FP_Satisfaction","_Haut"),
#            plotFun = reactive(
#              FP_Satisfaction(filtredHaut$FP) +
#                labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
#                theme(plot.caption = element_text(size = 6, hjust = 0))
#            ),
#            plotName = paste0("FP_Satisfaction","_Haut"),
#            user = input$userName,
#            seeds = filtredSeedsHaut_plotly())
# callModule(plotDownloadRate, paste0("FP_Satisfaction","_Bas"),
#            plotFun = reactive(
#              FP_Satisfaction(filtredBas$FP) +
#                labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
#                theme(plot.caption = element_text(size = 6, hjust = 0))
#            ),
#            plotName = paste0("FP_Satisfaction","_Bas"),
#            user = input$userName,
#            seeds = filtredSeedsBas_plotly())

FP_Satisfaction_agg <- function(FP_data){
  FP_satis_data <- FP_data %>%
    select(seed, sim_name, annee, starts_with("deci")) %>%
    group_by(annee) %>%
    summarise_at(vars(starts_with("deci")), mean, na.rm=TRUE) %>%
    collect() %>%
    gather(variable, taux, -annee) %>%
    separate(col = variable, into = c("foo", "Decile", "Indicateur"), sep="_") %>%
    select(-foo) %>%
    mutate(Indicateur = case_when(
      Indicateur == "satis" ~ "Globale",
      Indicateur == "smat" ~ "Matérielle",
      Indicateur == "sprot" ~ "Protection",
      Indicateur == "srel" ~ "Religieuse",
      TRUE ~ as.character(Indicateur)
    )) %>%
    mutate(Satisfaction = as.numeric(Decile) / 10) %>%
    mutate(Taux = taux * 100) %>%
    mutate(Satisfaction = factor(Satisfaction))%>%
    mutate(Satisfaction = fct_recode(Satisfaction, "< 0.1" = "0", "≥ 0.9" = "0.9"))

  
  ggplot(FP_satis_data) +
    geom_col(aes(factor(annee), Taux, fill = Satisfaction, group = Satisfaction)) +
    facet_grid(Indicateur~.) +
    scale_fill_brewer(name = "Satisfaction", type = "div", palette = "RdYlBu", direction = 0) +
    ggtitle("Évolution de la satisfaction des foyers paysans") +
    xlab("Temps") + ylab("Distribution (en %)") +
    labs(subtitle = "Variabilité : Ensemble des réplications") +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(title.position = "top", nrow = 1, title.hjust = 0.5,
                               label.position = "bottom", label.hjust = 0.5, keywidth = 2)) +
    theme(strip.text.y = element_text(size = 8),
          axis.text.y = element_text(size = 8))
          
}

callModule(plotDownloadRate, paste0("FP_Satisfaction","_Haut"),
           plotFun = reactive(
             FP_Satisfaction_agg(filtredHaut$FP) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$hautTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("FP_Satisfaction","_Haut"),
           user = input$userName,
           seeds = filtredSeedsHaut_plotly())
callModule(plotDownloadRate, paste0("FP_Satisfaction","_Bas"),
           plotFun = reactive(
             FP_Satisfaction_agg(filtredBas$FP) +
               labs(caption =  paste0("Paramètres de la sélection :\n", tablesParams$basTxt)) +
               theme(plot.caption = element_text(size = 6, hjust = 0))
           ),
           plotName = paste0("FP_Satisfaction","_Bas"),
           user = input$userName,
           seeds = filtredSeedsBas_plotly())