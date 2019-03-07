source("packages.R")

options( java.parameters = c("-Xss2560k", "-Xmx7g") ) # Needed fix for rJava (JDBC) + ggplot2

drv <- JDBC("com.mapd.jdbc.MapDDriver",
            "/data/user/c/rcura/mapd-1.0-SNAPSHOT-jar-with-dependencies.jar",
            identifier.quote="'")
conMapD <- dbConnect(drv, "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")

parameters <- tbl(conMapD, "parameters") %>% collect()
parameters_5_1 <- tbl(conMapD, "parameters_5_1") %>% collect()
parameters_defaut <- tbl(conMapD, "parameters_defaut") %>% collect()


# RENOMMAGE DES PARAMETRES

parameters_v6 <- parameters_defaut%>%
  rename(croissance_demo = taux_augmentation_fp) %>%
  rename(taux_renouvellement_fp = taux_renouvellement) %>%
  rename(proba_fp_dependant = proba_fp_dependants) %>%
  rename(objectif_nombre_seigneurs = nombre_seigneurs_objectif) %>%
  rename(debut_construction_chateaux = apparition_chateaux) %>%
  rename(nb_chateaux_potentiels_gs = nb_chateaux_potentiels_gs) %>%
  rename(rayon_migration_locale_fp = seuils_distance_max_dem_localst) %>%
  rename(freq_migration_lointaine = proba_ponderee_deplacement_lointain) %>%
  rename(nb_min_fp_agregat = nombre_fp_agregat) %>%
  rename(distance_detection_agregat = distance_detection_agregats) %>%
  rename(proba_creation_zp_banaux = proba_creation_zp_banaux) %>%
  rename(proba_creation_zp_basse_justice = proba_creation_zp_bassemoyennejustice) %>%
  rename(rayon_min_zp_ps = rayon_min_ps) %>%
  rename(rayon_max_zp_ps = rayon_max_ps) %>%
  rename(min_taux_prelevement_zp_ps = min_fourchette_loyers_ps) %>%
  rename(max_taux_prelevement_zp_ps = max_fourchette_loyers_ps) %>%
  rename(proba_cession_droits_zp = proba_don_partie_zp) %>%
  rename(proba_don_chateau_gs = proba_don_chateau_gs) %>%
  rename(proba_gain_haute_justice_chateau_ps = proba_gain_droits_hautejustice_chateau) %>%
  rename(proba_gain_droits_basse_justice_chateau = proba_gain_droits_bassemoyennejustice_chateau) %>%
  rename(proba_chateau_gs_agregat = proba_chateau_agregat) %>%
  rename(proba_promotion_chateau_pole = proba_promotion_groschateau_multipole) %>%
  rename(proba_promotion_chateau_isole = proba_promotion_groschateau_autre) %>%
  rename(attractivite_petit_chateau = attrac_pc) %>%
  rename(attractivite_gros_chateau = attrac_gc) %>%
  rename(attractivite_1_eglise = attrac_1_eglises) %>%
  rename(attractivite_2_eglise = attrac_2_eglises) %>%
  rename(attractivite_3_eglise = attrac_3_eglises) %>%
  rename(attractivite_4_eglise = attrac_4_eglises) %>%
  rename(attractivite_communaute = attrac_communautes)

# AJOUT DES NOUVEAUX PARAMETRES

parameters_v6 <- parameters_v6 %>%
  mutate(besoin_protection_fp = "[800::0,960::0.2,980::0.4,1000::0.6,1020::0.8,1040::1.0]") %>% 
  mutate(proba_gain_haute_justice_gs = "[800::0,900::0.1,1000::1.0]") %>% 
  mutate(debut_cession_droits_seigneurs = 900) %>% 
  mutate(debut_garde_chateaux_seigneurs = 950) %>% 
  mutate(periode_promotion_chateaux = "[800::false,960::true,1060::false]") %>% 
  mutate(dist_min_eglise = "[800::5000,960::3000,1060::1500]") %>% 
  mutate(dist_max_eglise = "[800::25000,960::10000,1060::5000]") %>% 
  mutate(dist_min_chateau = 1500) %>% 
  mutate(dist_max_chateau = 5000) %>% 
  mutate(min_s_distance_chateau = 0.0) %>% 
  mutate(distance_fusion_agregat = 100) %>% 
  mutate(rayon_cession_droits_ps = 3000) %>% 
  mutate(droits_haute_justice_zp = 1) %>% 
  mutate(droits_haute_justice_zp_suzerain = 1.25) %>% 
  mutate(droits_basse_justice_zp = 0.25) %>% 
  mutate(droits_basse_justice_zp_suzerain = 0.35) %>% 
  mutate(droits_banaux_zp = 0.25) %>% 
  mutate(droits_banaux_zp_suzerain = 0.35) %>% 
  mutate(droits_fonciers_zp = 1) %>% 
  mutate(min_rayon_zp_chateau = 2000) %>% 
  mutate(max_rayon_zp_chateau = 10000) %>% 
  mutate(dist_min_entre_chateaux_ps = 3000) %>% 
  mutate(dist_min_entre_chateaux_gs = 5000)


# MISE DES VARIABLES DANS LE BON ORDRE

parameters_v6 <- parameters_v6 %>%
  select(
    seed,
    sim_name,
    taille_cote_monde,
    init_nb_total_fp,
    init_nb_agglos,
    init_nb_fp_agglo,
    init_nb_villages,
    init_nb_fp_village,
    init_nb_gs,
    puissance_grand_seigneur1,
    puissance_grand_seigneur2,
    init_nb_ps,
    init_nb_eglises,
    init_nb_eglises_paroissiales,
    croissance_demo,
    taux_renouvellement_fp,
    proba_fp_dependant,
    besoin_protection_fp,
    puissance_communautes,
    coef_redevances,
    objectif_nombre_seigneurs,
    proba_gain_haute_justice_gs,
    debut_cession_droits_seigneurs,
    debut_garde_chateaux_seigneurs,
    debut_construction_chateaux,
    nb_chateaux_potentiels_gs,
    periode_promotion_chateaux,
    dist_min_eglise,
    dist_max_eglise,
    dist_min_chateau,
    dist_max_chateau,
    min_s_distance_chateau,
    rayon_migration_locale_fp,
    freq_migration_lointaine,
    nb_min_fp_agregat,
    proba_apparition_communaute,
    apparition_communautes,
    distance_detection_agregat,
    distance_fusion_agregat,
    proba_collecter_loyer,
    proba_creation_zp_banaux,
    proba_creation_zp_basse_justice,
    rayon_min_zp_ps,
    rayon_max_zp_ps,
    min_taux_prelevement_zp_ps,
    max_taux_prelevement_zp_ps,
    proba_cession_droits_zp,
    rayon_cession_droits_ps,
    proba_don_chateau_gs,
    proba_gain_haute_justice_chateau_ps,
    proba_gain_droits_banaux_chateau,
    proba_gain_droits_basse_justice_chateau,
    droits_haute_justice_zp,
    droits_haute_justice_zp_suzerain,
    droits_basse_justice_zp,
    droits_basse_justice_zp_suzerain,
    droits_banaux_zp,
    droits_banaux_zp_suzerain,
    droits_fonciers_zp,
    min_rayon_zp_chateau,
    max_rayon_zp_chateau,
    dist_min_entre_chateaux_ps,
    dist_min_entre_chateaux_gs,
    proba_chateau_gs_agregat,
    proba_promotion_chateau_pole,
    proba_promotion_chateau_isole,
    nb_min_paroissiens,
    seuil_creation_paroisse,
    nb_paroissiens_mecontents_necessaires,
    attractivite_petit_chateau,
    attractivite_gros_chateau,
    attractivite_1_eglise,
    attractivite_2_eglise,
    attractivite_3_eglise,
    attractivite_4_eglise,
    attractivite_communaute
  )

# BACKUP TABLES

dbSendQuery(conn = conMapD, statement = "CREATE TABLE parameters_bk AS SELECT * FROM parameters")
dbSendQuery(conn = conMapD, statement = "CREATE TABLE parameters_5_1_bk AS SELECT * FROM parameters_5_1")
dbSendQuery(conn = conMapD, statement = "CREATE TABLE parameters_defaut_bk AS SELECT * FROM parameters_defaut")
dbListTables(conn = conMapD)

# CREATE TABLE IN MAPD

sql_parameters_v6 <- 
"CREATE TABLE {`table_name`} (
  seed TEXT,
  sim_name TEXT,
  SHARED DICTIONARY (seed) REFERENCES seeds(seed),
  SHARED DICTIONARY (sim_name) REFERENCES seeds(sim_name),
  taille_cote_monde SMALLINT,
  init_nb_total_fp SMALLINT,
  init_nb_agglos SMALLINT,
  init_nb_fp_agglo SMALLINT,
  init_nb_villages SMALLINT,
  init_nb_fp_village SMALLINT,
  init_nb_gs SMALLINT,
  puissance_grand_seigneur1 FLOAT,
  puissance_grand_seigneur2 FLOAT,
  init_nb_ps SMALLINT,
  init_nb_eglises SMALLINT,
  init_nb_eglises_paroissiales SMALLINT,
  croissance_demo FLOAT,
  taux_renouvellement_fp FLOAT,
  proba_fp_dependant FLOAT,
  besoin_protection_fp TEXT,
  puissance_communautes FLOAT,
  coef_redevances SMALLINT,
  objectif_nombre_seigneurs SMALLINT,
  proba_gain_haute_justice_gs TEXT,
  debut_cession_droits_seigneurs SMALLINT,
  debut_garde_chateaux_seigneurs SMALLINT,
  debut_construction_chateaux SMALLINT,
  nb_chateaux_potentiels_gs SMALLINT,
  periode_promotion_chateaux TEXT,
  dist_min_eglise TEXT,
  dist_max_eglise TEXT,
  dist_min_chateau SMALLINT,
  dist_max_chateau SMALLINT,
  min_s_distance_chateau FLOAT,
  rayon_migration_locale_fp TEXT,
  freq_migration_lointaine FLOAT,
  nb_min_fp_agregat SMALLINT,
  proba_apparition_communaute FLOAT,
  apparition_communautes SMALLINT,
  distance_detection_agregat SMALLINT,
  distance_fusion_agregat SMALLINT,
  proba_collecter_loyer FLOAT,
  proba_creation_zp_banaux FLOAT,
  proba_creation_zp_basse_justice FLOAT,
  rayon_min_zp_ps SMALLINT,
  rayon_max_zp_ps SMALLINT,
  min_taux_prelevement_zp_ps FLOAT,
  max_taux_prelevement_zp_ps FLOAT,
  proba_cession_droits_zp FLOAT,
  rayon_cession_droits_ps SMALLINT,
  proba_don_chateau_gs FLOAT,
  proba_gain_haute_justice_chateau_ps FLOAT,
  proba_gain_droits_banaux_chateau FLOAT,
  proba_gain_droits_basse_justice_chateau FLOAT,
  droits_haute_justice_zp FLOAT,
  droits_haute_justice_zp_suzerain FLOAT,
  droits_basse_justice_zp FLOAT,
  droits_basse_justice_zp_suzerain FLOAT,
  droits_banaux_zp FLOAT,
  droits_banaux_zp_suzerain FLOAT,
  droits_fonciers_zp FLOAT,
  min_rayon_zp_chateau SMALLINT,
  max_rayon_zp_chateau SMALLINT,
  dist_min_entre_chateaux_ps SMALLINT,
  dist_min_entre_chateaux_gs SMALLINT,
  proba_chateau_gs_agregat FLOAT,
  proba_promotion_chateau_pole FLOAT,
  proba_promotion_chateau_isole FLOAT,
  nb_min_paroissiens SMALLINT,
  seuil_creation_paroisse SMALLINT,
  nb_paroissiens_mecontents_necessaires SMALLINT,
  attractivite_petit_chateau FLOAT,
  attractivite_gros_chateau FLOAT,
  attractivite_1_eglise FLOAT,
  attractivite_2_eglise FLOAT,
  attractivite_3_eglise FLOAT,
  attractivite_4_eglise FLOAT,
  attractivite_communaute FLOAT);
"

drop_table <- "DROP TABLE {`table_name`}"

table_name <- "parameters"
sqlQuery <- glue_sql(sql_parameters_v6, .con = conMapD)
DBI::dbSendQuery(conn = conMapD, statement = sqlQuery)

table_name <- "parameters_5_1"
sqlQuery <- glue_sql(sql_parameters_v6, table_name, .con = conMapD)
DBI::dbSendQuery(conn = conMapD, statement = sqlQuery)

table_name <- "parameters_defaut"
sqlQuery <- glue_sql(sql_parameters_v6, table_name, .con = conMapD)
DBI::dbSendQuery(conn = conMapD, statement = sqlQuery)

sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY parameters%s FROM '%s';", suffixe_tables, fileToRead))
print(DBI::dbFetch(sqlQuery))




# REMPLISSAGE TABLES
library(ssh)
session_ssh <- ssh_connect("rcura@mapd.cura.info")
conMapD <- dbConnect(drv, "jdbc:mapd:mapdi.cura.info:9091:mapd", "mapd", "HyperInteractive")

table <- "parameters_defaut"
params_mapd <- parameters_v6

fileToWrite <- "~/mapd-docker-storage/data/mapd_import/parameters_defaut.csv.bz2"
fileToRead <- str_replace(fileToWrite, pattern = "~/mapd-docker-storage/", replacement = "/mapd-storage/")

write_csv(params_mapd, fileToWrite)
scp_upload(session = session_ssh, files = fileToWrite, to = "~/mapd-docker-storage/data/mapd_import/")
file.remove(fileToWrite)

sqlQuery <- DBI::dbSendQuery(conn = conMapD, sprintf("COPY parameters_defaut FROM '%s';", fileToRead))
print(DBI::dbFetch(sqlQuery))


dbDisconnect(conMapD)
rm(session_ssh)
rm(conMapD, drv, parameters, parameters_5_1, parameters_defaut, parameters_v6, params_mapd)
rm(sqlQuery, drop_table, fileToRead, fileToWrite, sql_parameters_v6, table, table_name)
