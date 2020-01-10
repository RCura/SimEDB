sensitivity <- reactiveValues(selected = NULL,
                              plotted = NULL)

output$sensitivity_summary <- DT::renderDataTable({
  summary_sensib
}, extensions = list("FixedColumns" = NULL, "Buttons" = NULL),style = "bootstrap", class = "table-condensed", rownames = TRUE,
 options = list(paging = FALSE, scrollX = TRUE, fixedColumns = TRUE, dom = 'Bfrtip', buttons = list(list(extend = 'colvis', columns = c(2:9)))))


observe({
  if (length(input$sensitivity_summary_rows_selected) > 0) {
    sensitivity$selected <- summary_sensib[input$sensitivity_summary_rows_selected, "param"] %>% pull(param)
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
        sensib_plot(data = filtered_data, param = currentParam)
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


rename_and_recode_valeurs <- function(indicateur, valeurs){
  if (indicateur == "croissance_demo") {
    base <- c("0", "0.0153", "0.0372", "0.0589", "0.1289")
    names(base) <- c("0%", "1.53%", "3.72%", "5.89%", "12.89%")
  }
  if (indicateur == "dist_minmax_eglise") {
    base <- c("dynamique_reduit", "statique_reduit", "base", "statique_large", "dynamique_large")
    names(base) <- base
  }
  if (indicateur == "periode_promotion_chateaux") {
    base <- c("map([800::false,940::true,1020::false])",
              "map([800::false,940::true,1060::false])", "map([800::false,1000::true,1120::false])",
              "map([800::false,1100::true])", "map([800::false,940::true])")
    names(base) <- c("940-1000", "940-1040", "1000-1100", "1100-1200", "940-1200")
  }
  if (indicateur == "proba_creation_zp_autres_droits_ps") {
    base <- c("0.0", "0.05", "0.15", "0.25", "0.35")
    names(base) <- c("0%", "5%", "15%", "25%", "35%")
  }
  if (indicateur == "proba_gain_haute_justice_gs") {
    base <- c( "map([800::0.0])", "map([800::0.0,1000::0.5])",
               "map([800::0.0,900::0.1,920::0.2,940::0.3,960::0.4,980::0.5,1000::0.6,1020::0.7,1040::0.8,1060::0.8,1080::0.9,1100::1.0])",
               "map([800::0.0,900::0.2,1000::1.0])",
               "map([800::1.0])")
    names(base) <- c("statique_nul", "croissant_seuil", "croissant_regulier", "base", "statique_fort")
  }
  if (indicateur == "rayon_migration_locale_fp") {
    base <- c("map([800::1000])", "map([800::1000,1000::2500])", "map([800::2500])",
              "map([800::2500,1000::5000])", "map([800::5000,1000::10000])")
    names(base) <- c("statique_reduit","dynamique_reduit","base","dynamique_croissant","dynamique_large")
  }
  if (indicateur == "taux_prelevement_zp_chateau") {
    base <- c("0.0", "0.25", "0.5", "0.75", "1.0")
    names(base) <- c("0%", "25%", "50%", "75%", "100%")
  }
  
  liste_indicateurs_specifiques <- c(
    "croissance_demo", "dist_minmax_eglise", "periode_promotion_chateaux",
    "proba_creation_zp_autres_droits_ps", "proba_gain_haute_justice_gs",
    "rayon_migration_locale_fp", "taux_prelevement_zp_chateau"
  )
  if (!indicateur %in% liste_indicateurs_specifiques){
    base <- gtools::mixedsort(unique(valeurs))
    names(base) <- gtools::mixedsort(unique(valeurs))
  }
  new_factor <- fct_recode(valeurs, base[1], base[2], base[3], base[4], base[5])
  recoded_factor <- fct_relevel(new_factor, names(base))
}

sensib_plot <- function(data, param){
  data %>%
    filter(sensibility_parameter == !!param) %>%
    gather(Indicateur, Valeur, -sensibility_parameter, -sensibility_value, -type) %>%
    mutate(Indicateur = fct_recode(as.factor(Indicateur),
                                   "Nombre\nd'agrégats" = "nb_agregats",
                                   "Nombre de\ngrands\nchâteaux" = 'nb_grands_chateaux',
                                   "Nombre\nd'églises\nparoissiales" = "nb_eglises_paroissiales",
                                   "Distance\nentre\néglises" = "distance_eglises_paroissiales",
                                   "Taux de\nfoyers paysans\nisolés" = "prop_fp_isoles",
                                   "Augmentation\nde la charge\nfiscale" = "ratio_charge_fiscale"
    )) %>%
    mutate(Indicateur = fct_relevel(Indicateur, c("Nombre\nd'agrégats",
                                                  "Nombre de\ngrands\nchâteaux",
                                                  "Nombre\nd'églises\nparoissiales",
                                                  "Distance\nentre\néglises",
                                                  "Taux de\nfoyers paysans\nisolés",
                                                  "Augmentation\nde la charge\nfiscale"
    ))) %>%
    mutate(objectif = case_when(
      Indicateur == "Nombre\nd'agrégats" ~ 200,
      Indicateur == "Nombre de\ngrands\nchâteaux" ~ 10,
      Indicateur == "Nombre\nd'églises\nparoissiales" ~ 300,
      Indicateur == "Distance\nentre\néglises" ~ 3000,
      Indicateur == "Taux de\nfoyers paysans\nisolés" ~ 0.20,
      Indicateur == "Augmentation\nde la charge\nfiscale" ~ 3
    )) %>%
    rename(Parametre = sensibility_parameter) %>%
    mutate(sensibility_value = rename_and_recode_valeurs(indicateur = param,
                                                         valeurs = sensibility_value)) %>%
    mutate(Indicateur = fct_relabel(Indicateur, ~str_replace_all(.x, "\n", " "))) %>%
    ggplot() +
    aes(sensibility_value, Valeur, fill = sensibility_value) +
    #geom_boxplot(lwd = .1, outlier.size = .75, outlier.shape = 4) +
    geom_hline(aes(yintercept = objectif), linetype = "dashed", lwd = .5) +
    geom_violin(lwd = 0.2) + 
    geom_boxplot(coef = 0, width = 0.2, lwd = 0.2, outlier.shape = NA, colour = "white") +
    facet_wrap(~Indicateur, scales = "free", drop = TRUE, ncol = 6, labeller = label_wrap_gen(width = 20)) +
    scale_fill_viridis_d(name = param, end = .8) +
    labs(x = "", y = "") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(colour = "black", size = 8, margin = margin(t = 0,r = 15,b = 0,l = 0, unit = "pt")) ) +
    theme(legend.position="bottom",
          legend.justification="left",
          legend.title.align = 0,
          legend.title = element_text(face = "bold", vjust = 1),
          legend.key.width = unit(x = .75, units = "cm"),
          legend.key.height = unit(x = .75, units = "cm"),
          legend.margin=margin(-20,0,0,0),
          legend.box.margin=margin(0,0,0,0)) +
    theme(strip.text.x = element_text(margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt")))
}