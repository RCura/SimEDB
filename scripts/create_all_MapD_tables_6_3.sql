CREATE TABLE seeds_6_3 (
seed TEXT ENCODING DICT(16),
sim_name TEXT ENCODING DICT(8)
);


CREATE TABLE parameters_6_3 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds_6_3(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds_6_3(sim_name),
parametre TEXT ENCODING DICT(8),
valeur TEXT ENCODING DICT(16)
);

CREATE TABLE global_6_3 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds_6_3(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds_6_3(sim_name),
annee SMALLINT,
nb_fp INTEGER,
nb_agregats INTEGER,
nb_chateaux SMALLINT,
nb_grands_chateaux SMALLINT,
nb_eglises SMALLINT,
nb_eglises_paroissiales SMALLINT,
distance_eglises FLOAT,
distance_eglises_paroissiales FLOAT,
prop_fp_isoles FLOAT,
charge_fiscale FLOAT,
dist_ppv_agregat FLOAT,
total_duration INTEGER,
ratio_charge_fiscale FLOAT
);

CREATE TABLE seigneurs_6_3 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds_6_3(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds_6_3(sim_name),
annee SMALLINT,
id_seigneur INTEGER,
type TEXT ENCODING DICT(8),
date_apparition TEXT ENCODING DICT(8),
puissance FLOAT,
nb_chateaux_proprio SMALLINT,
nb_chateaux_gardien SMALLINT,
nb_fp_assujettis INTEGER,
nb_fp_foncier INTEGER,
nb_fp_foncier_garde INTEGER,
nb_fp_haute_justice INTEGER,
nb_fp_haute_justice_garde INTEGER,
nb_fp_autres_droits INTEGER,
nb_fp_autres_droits_garde INTEGER,
monagregat INTEGER,
geom TEXT ENCODING DICT
);

CREATE TABLE agregats_6_3 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds_6_3(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds_6_3(sim_name),
annee SMALLINT,
id_agregat INTEGER,
nombre_fp_agregat SMALLINT,
superficie FLOAT,
communaute TEXT ENCODING DICT(8),
monpole INTEGER,
geom TEXT ENCODING DICT
);


CREATE TABLE poles_6_3 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds_6_3(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds_6_3(sim_name),
annee SMALLINT,
id_pole INTEGER,
attractivite FLOAT,
nb_attracteurs SMALLINT,
monagregat INTEGER,
nb_eglises SMALLINT,
nb_paroisses SMALLINT,
nb_gc SMALLINT,
nb_pc SMALLINT,
nb_ca SMALLINT,
geom TEXT ENCODING DICT
);

CREATE TABLE fp_6_3 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds_6_3(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds_6_3(sim_name),
annee SMALLINT,
nb_fp INTEGER,
taux_fp_isoles FLOAT,
deplacement_fixe FLOAT,
deplacement_lointain FLOAT,
deplacement_local FLOAT,
from_isole_to_agregat_local FLOAT,
from_isole_to_agregat_lointain FLOAT,
from_isole_to_pole_local_hors_agregat FLOAT,
from_agregat_to_agregat_local FLOAT,
from_agregat_to_agregat_lointain FLOAT,
from_agregat_to_pole_local_hors_agregat FLOAT,
q1_redevances_acquittees FLOAT,
med_redevances_acquittees FLOAT,
q3_redevances_acquittees FLOAT,
deci_0_satis FLOAT,
deci_1_satis FLOAT,
deci_2_satis FLOAT,
deci_3_satis FLOAT,
deci_4_satis FLOAT,
deci_5_satis FLOAT,
deci_6_satis FLOAT,
deci_7_satis FLOAT,
deci_8_satis FLOAT,
deci_9_satis FLOAT,
deci_0_srel FLOAT,
deci_1_srel FLOAT,
deci_2_srel FLOAT,
deci_3_srel FLOAT,
deci_4_srel FLOAT,
deci_5_srel FLOAT,
deci_6_srel FLOAT,
deci_7_srel FLOAT,
deci_8_srel FLOAT,
deci_9_srel FLOAT,
deci_0_smat FLOAT,
deci_1_smat FLOAT,
deci_2_smat FLOAT,
deci_3_smat FLOAT,
deci_4_smat FLOAT,
deci_5_smat FLOAT,
deci_6_smat FLOAT,
deci_7_smat FLOAT,
deci_8_smat FLOAT,
deci_9_smat FLOAT,
deci_0_sprot FLOAT,
deci_1_sprot FLOAT,
deci_2_sprot FLOAT,
deci_3_sprot FLOAT,
deci_4_sprot FLOAT,
deci_5_sprot FLOAT,
deci_6_sprot FLOAT,
deci_7_sprot FLOAT,
deci_8_sprot FLOAT,
deci_9_sprot FLOAT
);

CREATE TABLE paroisses_6_3 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds_6_3(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds_6_3(sim_name),
annee SMALLINT,
id_paroisse INTEGER,
moneglise INTEGER,
mode_promotion TEXT ENCODING DICT(8),
superficie FLOAT,
nb_fideles SMALLINT,
nb_paroissiens_insatisfaits SMALLINT,
satisfaction_paroisse SMALLINT,
geom TEXT ENCODING DICT
);

CREATE TABLE chateaux_6_3 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds_6_3(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds_6_3(sim_name),
annee SMALLINT,
id_chateau INTEGER,
type TEXT ENCODING DICT(8),
rayon_zp_chateau INTEGER,
attractivite FLOAT,
droits_haute_justice TEXT ENCODING DICT(8),
monagregat INTEGER,
monproprietaire INTEGER,
monproprietaire_type TEXT ENCODING DICT(8),
mongardien INTEGER,
geom TEXT ENCODING DICT
);

