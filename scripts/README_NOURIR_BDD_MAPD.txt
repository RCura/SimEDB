###############################
# Import de nouvelles données #
###############################

# 1 : Copier les fichiers à insérer dans les BDD dans /home/robin/mapd-storage/data/mapd_import
# 2 : Entrer dans le container : docker container exec -i -t NomDuContainer /bin/bash
# 3  : cd /mapd-storage/data/mapd_import/
# 4 : Dans Immerse, créer les tables : CREATE TABLE fp_5 as SELECT * FROM fp LIMIT 10 ;
# 5 : Vider les tables : TRUNCATE TABLE fp_5;
# 6 : Remplir les tables depuis les csv (ici, tail -n +2 pour ne pas envoyer le header des csv)

cat 5_0_Test_MapD_FP.csv | tail -n +2 | /mapd/SampleCode/StreamInsert fp_5 mapd --host localhost --port 9091 -u mapd -p HyperInteractive --batch 1000000 --null 'NA'

cat 5_0_Test_MapD_agregats.csv | tail -n +2 | /mapd/SampleCode/StreamInsert agregats_5 mapd --host localhost --port 9091 -u mapd -p HyperInteractive --batch 10000 --null 'NA'

cat 5_0_Test_MapD_parameters.csv | tail -n +2 | /mapd/SampleCode/StreamInsert parameters_5 mapd --host localhost --port 9091 -u mapd -p HyperInteractive --batch 10000 --null 'NA'

###### /!\ Pose problème, peut-être à cause des quotes
##### Importé graphiquement via Immerse


cat 5_0_Test_MapD_paroisses.csv | tail -n +2 | /mapd/SampleCode/StreamInsert paroisses_5 mapd --host localhost --port 9091 -u mapd -p HyperInteractive --batch 10000 --null 'NA'

cat 5_0_Test_MapD_poles.csv | tail -n +2 | /mapd/SampleCode/StreamInsert poles_5 mapd --host localhost --port 9091 -u mapd -p HyperInteractive --batch 10000 --null 'NA'

cat 5_0_Test_MapD_results.csv | tail -n +2 | /mapd/SampleCode/StreamInsert results_5 mapd --host localhost --port 9091 -u mapd -p HyperInteractive --batch 10000 --null 'NA'

cat 5_0_Test_MapD_seeds.csv | tail -n +2 | /mapd/SampleCode/StreamInsert seeds_5 mapd --host localhost --port 9091 -u mapd -p HyperInteractive --batch 10000 --null 'NA'

cat 5_0_Test_MapD_seigneurs.csv | tail -n +2 | /mapd/SampleCode/StreamInsert seigneurs_5 mapd --host localhost --port 9091 -u mapd -p HyperInteractive --batch 10000 --null 'NA'





###############################################
# Fusion des tables de SimFeodal v4.5 et v5.0 #
###############################################


# Export des tables de SimFeodal 5.0
COPY (SELECT * FROM agregats_5) TO '/mapd-storage/data/mapd_export/5/agregats_5.csv';
COPY (SELECT * FROM fp_5) TO '/mapd-storage/data/mapd_export/5/fp_5.csv';
COPY (SELECT * FROM parameters_5) TO '/mapd-storage/data/mapd_export/5/parameters_5.csv';
COPY (SELECT * FROM paroisses_5) TO '/mapd-storage/data/mapd_export/5/paroisses_5.csv';
COPY (SELECT * FROM poles_5) TO '/mapd-storage/data/mapd_export/5/poles_5.csv';
COPY (SELECT * FROM results_5) TO '/mapd-storage/data/mapd_export/5/results_5.csv';
COPY (SELECT * FROM seeds_5) TO '/mapd-storage/data/mapd_export/5/seeds_5.csv';
COPY (SELECT * FROM seigneurs_5) TO '/mapd-storage/data/mapd_export/5/seigneurs_5.csv';


# Export des tables de SimFeodal 4.5
COPY (SELECT * FROM agregats WHERE sim_name = '4_5_A') TO '/mapd-storage/data/mapd_export/4/agregats_4.csv';
COPY (SELECT * FROM fp WHERE sim_name = '4_5_A') TO '/mapd-storage/data/mapd_export/4/fp_4.csv';
COPY (SELECT * FROM parameters WHERE sim_name = '4_5_A') TO '/mapd-storage/data/mapd_export/4/parameters_4.csv';
COPY (SELECT * FROM paroisses WHERE sim_name = '4_5_A') TO '/mapd-storage/data/mapd_export/4/paroisses_4.csv';
COPY (SELECT * FROM poles WHERE sim_name = '4_5_A') TO '/mapd-storage/data/mapd_export/4/poles_4.csv';
COPY (SELECT * FROM results WHERE sim_name = '4_5_A') TO '/mapd-storage/data/mapd_export/4/results_4.csv';
COPY (SELECT * FROM seeds WHERE sim_name = '4_5_A') TO '/mapd-storage/data/mapd_export/4/seeds_4.csv';
COPY (SELECT * FROM seigneurs WHERE sim_name = '4_5_A') TO '/mapd-storage/data/mapd_export/4/seigneurs_4.csv';


# Creation des tables pour SimFeodal v4.5-5.0
CREATE TABLE  agregats_4_5 AS (SELECT * FROM agregats_5 LIMIT 1);
CREATE TABLE  fp_4_5 AS (SELECT * FROM fp_5 LIMIT 1);
CREATE TABLE  parameters_4_5 AS (SELECT * FROM parameters_5 LIMIT 1);
CREATE TABLE  paroisses_4_5 AS (SELECT * FROM paroisses_5 LIMIT 1);
CREATE TABLE  poles_4_5 AS (SELECT * FROM poles_5 LIMIT 1);
CREATE TABLE  results_4_5 AS (SELECT * FROM results_5 LIMIT 1);
CREATE TABLE  seeds_4_5 AS (SELECT * FROM seeds_5 LIMIT 1);
CREATE TABLE  seigneurs_4_5 AS (SELECT * FROM seigneurs_5 LIMIT 1);

# On vide les nouvelles tables
TRUNCATE TABLE agregats_4_5;
TRUNCATE TABLE fp_4_5;
TRUNCATE TABLE parameters_4_5;
TRUNCATE TABLE paroisses_4_5;
TRUNCATE TABLE poles_4_5;
TRUNCATE TABLE results_4_5;
TRUNCATE TABLE seeds_4_5;
TRUNCATE TABLE seigneurs_4_5;

# On remplit les nouvelles tables avec les v4.5 pour commencer
COPY agregats_4_5 FROM '/mapd-storage/data/mapd_export/4/agregats_4.csv';
COPY fp_4_5 FROM '/mapd-storage/data/mapd_export/4/fp_4.csv';
COPY parameters_4_5 FROM '/mapd-storage/data/mapd_export/4/parameters_4.csv';
COPY paroisses_4_5 FROM '/mapd-storage/data/mapd_export/4/paroisses_4.csv';
COPY poles_4_5 FROM '/mapd-storage/data/mapd_export/4/poles_4.csv';
COPY results_4_5 FROM '/mapd-storage/data/mapd_export/4/results_4.csv';
COPY seeds_4_5 FROM '/mapd-storage/data/mapd_export/4/seeds_4.csv';
COPY seigneurs_4_5 FROM '/mapd-storage/data/mapd_export/4/seigneurs_4.csv';

# Puis on finit le remplissage avec la v5.0
COPY agregats_4_5 FROM '/mapd-storage/data/mapd_export/5/agregats_5.csv';
COPY fp_4_5 FROM '/mapd-storage/data/mapd_export/5/fp_5.csv';
COPY parameters_4_5 FROM '/mapd-storage/data/mapd_export/5/parameters_5.csv';
COPY paroisses_4_5 FROM '/mapd-storage/data/mapd_export/5/paroisses_5.csv';
COPY poles_4_5 FROM '/mapd-storage/data/mapd_export/5/poles_5.csv';
COPY results_4_5 FROM '/mapd-storage/data/mapd_export/5/results_5.csv';
COPY seeds_4_5 FROM '/mapd-storage/data/mapd_export/5/seeds_5.csv';
COPY seigneurs_4_5 FROM '/mapd-storage/data/mapd_export/5/seigneurs_5.csv';