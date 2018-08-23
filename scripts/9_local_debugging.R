source("packages.R")

options( java.parameters = c("-Xss2560k", "-Xmx7g") ) # Needed fix for rJava (JDBC) + ggplot2

drv <- JDBC("com.mapd.jdbc.MapDDriver",
            "/data/user/c/rcura/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
            identifier.quote="'")
conMapD <- dbConnect(drv, "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")

seeds <- tbl(conMapD, "seeds")
agregats <- tbl(conMapD, "agregats")
fp <- tbl(conMapD, "fp")
parameters <- tbl(conMapD, "parameters")
paroisses <- tbl(conMapD, "paroisses")
poles <- tbl(conMapD, "poles")
results <- tbl(conMapD, "results")
seigneurs <- tbl(conMapD, "seigneurs")

##############################################################
##############################################################
##############################################################

FP_data <- fp %>% filter(sim_name %in% c("5_0"))
results_data <- results %>% filter(sim_name %in% c("5_0"))
agregats_data <- agregats %>% filter(sim_name %in% c("5_0"))
poles_data <- poles %>% filter(sim_name %in% c("5_0"))
paroisses_data <- paroisses %>% filter(sim_name %in% c("5_0"))
seigneurs_data <- seigneurs %>% filter(sim_name %in% c("5_0"))

##############################################################
##############################################################
##############################################################

Seigneurs_Vassaux <- function(seigneurs_data){
  myBreaks <- c(-1,0,1,2,3,4,5,10,25,50,1000)
  myLabels <- c("0","1", "2", "3", "4", "5","6;10", "11;25", "26;50", ">50")
  
  debiteurs_seigneurs <- seigneurs_data %>%
    filter(annee == 1160) %>%
    select(seed, annee, type, init, nbdebiteurs) %>%
    collect() %>%
    mutate(initial = if_else(init == 1, "Initialement\nPrésent", "Arrivé\nen cours")) %>%
    mutate(initial = factor(initial, levels = c("Arrivé\nen cours", "Initialement\nPrésent"))) %>%
    mutate(nbDebiteursBreaks =  cut(nbdebiteurs, breaks = myBreaks, labels =  myLabels)) %>%  
    group_by(seed, type, initial, nbDebiteursBreaks) %>%
    summarise(StatsDebiteurs = n())
  
  debInitCPS <- debiteurs_seigneurs %>% filter(type != "Grand Seigneur", initial == "Initialement\nPrésent")
  debNonCPS <- debiteurs_seigneurs %>% filter(type != "Grand Seigneur", initial == "Arrivé\nen cours")
  debGS <- debiteurs_seigneurs %>% filter(type == "Grand Seigneur")
  
  plotInitCPS <- ggplot(debInitCPS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_grid(initial ~ type, scales="free", space = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
  
  plotNonCPS <- ggplot(debNonCPS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_grid(initial ~ type, scales="free", space = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
  
  plotGS <- ggplot(debGS, aes(nbDebiteursBreaks, StatsDebiteurs)) +
    geom_tufteboxplot() +
    facet_wrap(~type) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.title.y=element_blank(),axis.title.x=element_blank()) +
    theme(plot.title = element_text(size = rel(1), face = "italic"))
  
  lay <- rbind(c(1,1,1,3),
               c(2,2,2,3))
  
  grid.arrange(plotNonCPS, plotInitCPS, plotGS, nrow = 1, layout_matrix = lay,
               bottom = "Nombre de Vassaux", left = "Fréquence",
               top = "Distribution du nombre de vassaux selon les types de seigneurs
               Variabilité : Réplications")
}

##############################################################
##############################################################
##############################################################

Paroisses_Promo <- function(paroisses_data){
  paroisses_promo <- paroisses_data %>%
    filter(!(mode_promotion %in% c("nil", "initialisation"))) %>%
    group_by(seed, annee, mode_promotion) %>%
    summarise(nb = n()) %>%
    collect() %>%
    mutate(mode_promotion = case_when(
      mode_promotion == "creation agregat" ~ "Création dans un agrégat",
      mode_promotion == "creation isole" ~ "Création en zone peu dense",
      mode_promotion == "promotion isole" ~ "Promotion en zone peu dense")
    )
  
  ggplot(paroisses_promo, aes(factor(annee), nb)) +
    geom_tufteboxplot() +
    facet_wrap(~ mode_promotion, ncol = 1) +
    xlab("Temps") + ylab("Nombre de nouvelles paroisses\nà chaque pas de temps") +
    ggtitle("Évolution des modes de création de nouvelles paroisses") +
    labs(subtitle = "Variabilité : Réplications")
}

##############################################################
##############################################################
##############################################################

dbDisconnect(conn = conMapD)
