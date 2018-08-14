library(tidyverse)
library(DBI)
library(dbplyr)
library(RJDBC)



options( java.parameters = c("-Xss2560k", "-Xmx7g") ) # Needed fix for rJava (JDBC) + ggplot2
drv <- JDBC("com.mapd.jdbc.MapDDriver",
            "/data/user/c/rcura/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
            identifier.quote="'")


system.time({
  conHumaNum <- dbConnect(drv = drv, "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")
  sqlQuery <- DBI::dbSendQuery(conn = conHumaNum, "SELECT sim_name, COUNT(*) AS NB FROM fp GROUP BY sim_name")
  print(DBI::dbFetch(sqlQuery))
  dbDisconnect(conHumaNum)
})



system.time({
  conHumaNum <- dbConnect(drv = drv, "jdbc:mapd:192.168.38.46:9091:mapd", "mapd", "HyperInteractive")
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

system.time({
  conHumaNum <- dbConnect(drv = drv, "jdbc:mapd:192.168.38.46:9091:mapd", "mapd", "HyperInteractive")
  fp_humanum <- tbl(conHumaNum, "fp")
  FP_data_humanum <- fp_humanum
  nombre_FP_total_humanum <- FP_data_humanum %>%
    group_by(seed, sim_name, annee) %>%
    summarise(n_total = n())
  
  blob <- nombre_FP_total_humanum %>% collect()
  dbDisconnect(conHumaNum)
})

system.time({
  conHumaNum <- dbConnect(drv = drv, "jdbc:mapd:192.168.38.46:9091:mapd", "mapd", "HyperInteractive")
  fp_humanum <- tbl(conHumaNum, "fp")
  FP_data_humanum <- fp_humanum
  foo <- FP_data_humanum %>%
    filter(!(type_deplacement %in% c("nil", "Non mobile"))) %>%
    group_by(annee, seed, sim_name, type_deplacement) %>%
    summarise(n = n()) %>%
    collect()
  
  dbDisconnect(conHumaNum)
})

system.time({
  conHumaNum <- dbConnect(drv = drv, "jdbc:mapd:192.168.38.46:9091:mapd", "mapd", "HyperInteractive")
  fp_humanum <- tbl(conHumaNum, "fp")
  FP_data_humanum <- fp_humanum
  foo <- FP_data_humanum %>%
    filter(!(type_deplacement %in% c("nil", "Non mobile"))) %>%
    group_by(annee, seed, sim_name, type_deplacement) %>%
    summarise(n = n()) %>%
    collect()
  
  dbDisconnect(conHumaNum)
})


system.time({
  conHumaNum <- dbConnect(drv = drv, "jdbc:mapd:192.168.38.46:9091:mapd", "mapd", "HyperInteractive")
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
    collect() %>%
    mutate(Tx = (n + 1E-12) / (n_total + 1E-12)) %>%
    ungroup()
  dbDisconnect(conHumaNum)
})
