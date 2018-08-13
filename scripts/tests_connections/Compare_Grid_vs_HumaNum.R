library(tidyverse)
library(DBI)
library(dbplyr)
library(RJDBC)

options( java.parameters = c("-Xss2560k", "-Xmx8g") ) # Needed fix for rJava (JDBC) + ggplot2
drv <- JDBC("com.mapd.jdbc.MapDDriver",
            "/home/robin/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
            identifier.quote="'")


system.time({
  conGrid <- dbConnect(drv = drv, "jdbc:mapd:localhost:9091:mapd", "mapd", "HyperInteractive")
  sqlQuery <- DBI::dbSendQuery(conn = conGrid, "SELECT sim_name, COUNT(*) AS NB FROM fp_4_5 GROUP BY sim_name")
  print(DBI::dbFetch(sqlQuery))
  dbDisconnect(conGrid)
})


system.time({
  conHumaNum <- dbConnect(drv = drv, "jdbc:mapd:134.158.38.46:80:mapd", "mapd", "HyperInteractive")
  sqlQuery <- DBI::dbSendQuery(conn = conHumaNum, "SELECT sim_name, COUNT(*) AS NB FROM fp GROUP BY sim_name")
  print(DBI::dbFetch(sqlQuery))
  dbDisconnect(conHumaNum)
})


# Round 2


system.time({
  conGrid <- dbConnect(drv = drv, "jdbc:mapd:localhost:9091:mapd", "mapd", "HyperInteractive")
  fp_grid <- tbl(conGrid, "fp_4_5")
  FP_data_grid <- fp_grid
  nombre_FP_total_grid <- FP_data_grid %>%
    group_by(seed, sim_name, annee) %>%
    summarise(n_total = n())
  
  
  types_deplacements_grid <- FP_data_grid %>%
    filter(!(type_deplacement %in% c("nil", "Non mobile"))) %>%
    group_by(annee, seed, sim_name, type_deplacement) %>%
    summarise(n = n()) %>%
    left_join(nombre_FP_total_grid, by = c("seed", "annee", "sim_name")) %>%
    mutate(Tx = (n + 1E-12) / (n_total + 1E-12)) %>%
    ungroup() %>%
    collect()
  dbDisconnect(conGrid)
})

# First run :
# utilisateur     système      écoulé 
# 1.370       0.094      11.922 
# Second run : 
# utilisateur     système      écoulé 
# 0.540       0.178      11.056 
# Third run :
# utilisateur     système      écoulé 
# 0.260       0.056      10.748 


system.time({
  conHumaNum <- dbConnect(drv = drv, "jdbc:mapd:134.158.38.46:80:mapd", "mapd", "HyperInteractive")
  fp_humanum <- tbl(conHumaNum, "fp")
  FP_data_humanum <- fp_humanum
  nombre_FP_total_humanum <- FP_data_humanum %>%
    group_by(seed, sim_name, annee) %>%
    summarise(n_total = n())
  
  
  types_deplacements_humanum <- FP_data_humanum %>%
    filter(!(type_deplacement %in% c("nil", "Non mobile"))) %>%
    group_by(annee, seed, sim_name, type_deplacement) %>%
    summarise(n = n()) %>%
    left_join(nombre_FP_total_humanum, by = c("seed", "annee", "sim_name")) %>%
    mutate(Tx = (n + 1E-12) / (n_total + 1E-12)) %>%
    ungroup() %>%
    collect()
  dbDisconnect(conHumaNum)
})

# First run :
# utilisateur     système      écoulé 
# 0.413       0.160       8.509 
# Second run : 
# utilisateur     système      écoulé 
# 0.365       0.139       5.265 
# Third run :
# utilisateur     système      écoulé 
# 0.277       0.151       5.129 


all.equal(types_deplacements_grid, types_deplacements_humanum)
# TRUE
