sensitivity <- reactiveValues(selected = NULL,
                              plotted = NULL)



# 
# plot_data <- filtered_data %>%
#   gather(key = Indicateur, value = Resultat, -param, -valeur, -seed) %>%
#   filter(param == sample_n(., 1) %>% pull(param)) %>%
#   mutate(Indicateur = stringr::str_replace(Indicateur, "_om", "")) %>%
#   mutate(Indicateur = stringr::str_replace_all(Indicateur, "_", " ")) %>%
#   mutate(Indicateur = stringr::str_wrap(Indicateur, width = 10))
# 
# plot_title <- plot_data %>% pull(param) %>% unique()
# 
# ggplot(plot_data, aes(valeur, Resultat, group = factor(valeur))) +
#   geom_tufteboxplot() +
#   facet_wrap( ~Indicateur, scales = "free", nrow = 1) +
#   labs(title = plot_title, x = "Valeurs de paramètres", y = "Valeurs des Indicateurs") +
#   theme(strip.text = element_text(size = 7),
#         axis.text = element_text(size = 7),
#         strip.background = element_blank())
# 
# ggplot(plot_data %>% group_by(Indicateur, valeur) %>% summarise(mean = median(Resultat, na.rm = TRUE))) +
#   geom_point(aes(valeur,  mean)) +
#   facet_wrap( ~Indicateur, scales = "free", nrow = 1)

output$sensitivity_summary <- DT::renderDataTable({
  sensibility_summary_table
}, extensions = "FixedColumns",style = "bootstrap", class = "table-condensed", rownames = TRUE,
 options = list(paging = FALSE, scrollX = TRUE, fixedColumns = TRUE))


observe({
  if (length(input$sensitivity_summary_rows_selected) > 0) {
    sensitivity$selected <- sensibility_summary_table[input$sensitivity_summary_rows_selected, "param"] %>% pull(param)
  } else {
    sensitivity$selected <- NULL
  }
})

observe({
  req(sensitivity$selected)
  for (currentParam in sensitivity$selected) {
    if (currentParam %in% sensitivity$plotted) {
       #do nothing
    } else {
      sensitivity$plotted <- c(sensitivity$plotted, currentParam)
      plotname <- paste("paramplot_",  currentParam, sep = "")
      insertUI(selector = "#sensitivity_plots",
               where = "beforeEnd",
               ui =  fluidRow(plotOutput(outputId = plotname,  height = "200px"))
      )
      output[[plotname]] <- renderPlot({
        plot_data <- filtered_data %>%
          filter(param == !!currentParam) %>%
          mutate(Indicateur = stringr::str_wrap(Indicateur, width = 20))
        
        ggplot(plot_data, aes(valeur, Resultat, group = factor(valeur))) +
          geom_tufteboxplot() +
          facet_wrap( ~Indicateur, scales = "free", nrow = 1) +
          labs(title = currentParam, x = "Valeurs de paramètres", y = "Valeurs des Indicateurs") +
          theme(strip.text = element_text(size = 7),
                axis.text = element_text(size = 7),
                strip.background = element_blank())
      })
    }
  }
})

observe({
  req(sensitivity$plotted)
  for (currentParam in sensitivity$plotted) {
    if (currentParam %in% sensitivity$selected) {
      # Do nothing
    } else {
      plotname <- paste("paramplot_",  currentParam, sep = "")
      removeUI(selector = sprintf('#%s', plotname))
      sensitivity$plotted <- sensitivity$plotted[sensitivity$plotted != currentParam]
    }
  }
})


# Old : 

########## SENSITIVITY #########

# results_sensivity <- read_csv("data/4_5_OM_results_om.csv", col_types = cols()) %>%
#   rename(param = sensibility_parameter) %>%
#   rename(valeur = sensibility_value) %>%
#   distinct(param, valeur, seed, .keep_all = TRUE) %>%
#   mutate(valeur = round(valeur, digits = 5))
# 
# experiment_plan <- readRDS("data/sensib_params.Rds") %>%
#   select(param, valeur, seed) %>%
#   mutate(valeur = round(valeur, digits = 5)) %>%
#   full_join(results_sensivity, by = c("param", "valeur", "seed")) %>%
#   select(-sim_name)
# 
# rm(results_sensivity)
# 
# Objectifs <- data_frame(
#   Var = c("nb_agregats_om", "nb_chateaux_om", "nb_gros_chateaux_om", "nb_seigneurs_om",
#           "nb_eglises_paroissiales_om", "distance_eglises_paroissiales_om",
#           "proportion_fp_isoles_om", "augmentation_charge_fiscale_om"),
#   RealVar = c("Agrégats", "Châteaux",  "Gros châteaux", "Seigneurs",
#               "Églises paroissiales", "Distance moyenne entre églises",  
#               "Part de foyers paysans isolés",
#               "Augmentation de la charge fiscale des foyers paysans"),
#   Objectif = c(200, 50, 10, 200, 300, 3000, 0.2, 3)
# )
# 
# filtered_data <- experiment_plan %>%
#   group_by(param, valeur) %>%
#   arrange(nb_agregats_om) %>%
#   filter(row_number() <= 10) %>%
#   ungroup() %>%
#   gather(key = Indicateur, value = Resultat, -param, -valeur, -seed) %>%
#   filter(Indicateur != "nb_eglises_om") %>%
#   left_join(Objectifs, by = c("Indicateur" = "Var")) %>%
#   mutate(Indicateur = RealVar) %>%
#   select(-RealVar, -Objectif) %>%
#   filter(!is.na(Indicateur))
# 
# standardised_data <- filtered_data %>%
#   group_by(Indicateur) %>%
#   mutate(StdResult = scale(Resultat,  center = TRUE, scale = TRUE)) %>%
#   ungroup()
# 
# resAB <- standardised_data %>%
#   group_by(param, valeur, Indicateur) %>%
#   summarise(sd_intra =  sd(StdResult, na.rm = TRUE)) %>%
#   group_by(param) %>%
#   summarise(med_sd_intra = mean(sd_intra, na.rm = TRUE),
#             Q1_sd_intra = quantile(sd_intra, na.rm = TRUE, probs = .25))
# 
# resCD <- standardised_data %>%
#   group_by(param, valeur, Indicateur) %>%
#   summarise(avg = mean(StdResult, na.rm = TRUE)) %>%
#   group_by(param, Indicateur) %>%
#   summarise(sd_avg = sd(avg, na.rm = TRUE)) %>%
#   group_by(param) %>%
#   summarise(avg_sd_avg = mean(sd_avg, na.rm = TRUE),
#             Q3_sd_avg =  quantile(sd_avg, na.rm = TRUE, probs = .75))
# 
# 
# resEF <- filtered_data %>%
#   spread(key = Indicateur, value = Resultat) %>%
#   select(-param, -valeur, -seed) %>%
#   scale(scale = TRUE, center = Objectifs %>% arrange(RealVar) %>% pull(Objectif)) %>%
#   as.data.frame() %>%
#   bind_cols(filtered_data %>%
#               spread(key = Indicateur, value = Resultat) %>%
#               select(param, valeur, seed)) %>%
#   select(param, valeur, seed, everything()) %>%
#   gather(key = Indicateur, value = StdResultat, -param, -valeur, -seed) %>%
#   group_by(param) %>%
#   summarise(avg_ecart =  mean(abs(StdResultat), na.rm = TRUE),
#             Q1_ecart =  quantile(abs(StdResultat), na.rm = TRUE, probs =  .25))
# 
# resG <- filtered_data %>%
#   group_by(param, valeur, Indicateur) %>%
#   summarise(nb_failing_seeds = sum(is.na(Resultat), na.rm = TRUE)) %>%
#   ungroup() %>%
#   group_by(param) %>%
#   summarise(percent_failing_seeds = mean(nb_failing_seeds) * 100 / n())
# 
# resH <- filtered_data %>%
#   filter(Indicateur == "Agrégats") %>%
#   group_by(param, valeur) %>%
#   summarise(nb_failing_seeds = sum(is.na(Resultat), na.rm = TRUE)) %>%
#   mutate(is_failing = ifelse(nb_failing_seeds > 0, TRUE, FALSE)) %>%
#   ungroup() %>%
#   group_by(param) %>%
#   summarise(nb_failing_values = sum(is_failing))
# 
# sensibility_summary_table <- resAB %>%
#   left_join(resCD, by = "param") %>%
#   left_join(resEF, by = "param") %>%
#   left_join(resG, by = "param") %>%
#   left_join(resH, by =  "param") %>%
#   mutate_if(is.numeric, round, digits = 3)
# 
# rm(Objectifs, standardised_data, resAB, resCD, resEF, resG, resH)
# 
# saveRDS(object = experiment_plan, file = "data/sensib/experiment_plan.Rds")
# saveRDS(object = filtered_data, file = "data/sensib/filtered_data.Rds")
# saveRDS(object = sensibility_summary_table, file = "data/sensib/sensibility_summary_table.Rds")
