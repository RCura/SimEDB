CREATE TABLE seeds_6_1 (
seed TEXT ENCODING DICT(16),
sim_name TEXT ENCODING DICT(8)
);


CREATE TABLE parameters_6_1_bk (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds(sim_name),
parametre TEXT ENCODING DICT(8),
valeur TEXT ENCODING DICT(16)
);

CREATE TABLE parameters_6_1 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds(sim_name),
debut_simulation TEXT ENCODING DICT(8),
fin_simulation TEXT ENCODING DICT(8),
duree_step TEXT ENCODING DICT(8),
taille_cote_monde TEXT ENCODING DICT(8),
init_nb_total_fp TEXT ENCODING DICT(8),
init_nb_agglos TEXT ENCODING DICT(8),
init_nb_fp_agglo TEXT ENCODING DICT(8),
init_nb_villages TEXT ENCODING DICT(8),
init_nb_fp_village TEXT ENCODING DICT(8),
init_nb_gs TEXT ENCODING DICT(8),
puissance_grand_seigneur1 TEXT ENCODING DICT(8),
puissance_grand_seigneur2 TEXT ENCODING DICT(8),
init_nb_ps TEXT ENCODING DICT(8),
init_nb_eglises TEXT ENCODING DICT(8),
init_nb_eglises_paroissiales TEXT ENCODING DICT(8),
croissance_demo TEXT ENCODING DICT(8), 
taux_renouvellement_fp TEXT ENCODING DICT(8), 
proba_fp_dependant TEXT ENCODING DICT(8), 
besoin_protection_fp TEXT ENCODING DICT(8),
puissance_communautes TEXT ENCODING DICT(8),
proba_institution_communaute TEXT ENCODING DICT(8),
objectif_nombre_seigneurs TEXT ENCODING DICT(8),
proba_gain_haute_justice_gs TEXT ENCODING DICT(8),
proba_gain_haute_justice_chateau_ps TEXT ENCODING DICT(8),
debut_cession_droits_seigneurs TEXT ENCODING DICT(8),
debut_garde_chateaux_seigneurs TEXT ENCODING DICT(8),
debut_construction_chateaux TEXT ENCODING DICT(8),
periode_promotion_chateaux TEXT ENCODING DICT(8),
dist_min_eglise TEXT ENCODING DICT(8),
dist_max_eglise TEXT ENCODING DICT(8),
dist_min_chateau TEXT ENCODING DICT(8),
dist_max_chateau TEXT ENCODING DICT(8),
rayon_migration_locale_fp TEXT ENCODING DICT(8),
prop_migration_lointaine_fp TEXT ENCODING DICT(8),
nb_min_fp_agregat TEXT ENCODING DICT(8),
distance_detection_agregat TEXT ENCODING DICT(8),
nb_max_chateaux_par_tour_gs TEXT ENCODING DICT(8),
nb_max_chateaux_par_tour_ps TEXT ENCODING DICT(8),
proba_collecter_loyer_ps TEXT ENCODING DICT(8),
proba_creation_zp_autres_droits_ps TEXT ENCODING DICT(8),
rayon_min_zp_ps TEXT ENCODING DICT(8),
rayon_max_zp_ps TEXT ENCODING DICT(8),
min_taux_prelevement_zp_ps TEXT ENCODING DICT(8),
max_taux_prelevement_zp_ps TEXT ENCODING DICT(8),
taux_prelevement_zp_chateau TEXT ENCODING DICT(8),
proba_cession_droits_zp TEXT ENCODING DICT(8),
rayon_cession_locale_droits_ps TEXT ENCODING DICT(8),
proba_cession_locale TEXT ENCODING DICT(8),
proba_don_chateau TEXT ENCODING DICT(8),
rayon_min_zp_chateau TEXT ENCODING DICT(8),
rayon_max_zp_chateau TEXT ENCODING DICT(8),
dist_min_entre_chateaux TEXT ENCODING DICT(8),
proba_chateau_gs_agregat TEXT ENCODING DICT(8),
proba_promotion_chateau_pole TEXT ENCODING DICT(8),
proba_promotion_chateau_isole TEXT ENCODING DICT(8),
seuil_creation_paroisse TEXT ENCODING DICT(8),
nb_requis_paroissiens_insatisfaits TEXT ENCODING DICT(8),
attractivite_petit_chateau TEXT ENCODING DICT(8),
attractivite_gros_chateau TEXT ENCODING DICT(8),
attractivite_1_eglise TEXT ENCODING DICT(8),
attractivite_2_eglise TEXT ENCODING DICT(8),
attractivite_3_eglise TEXT ENCODING DICT(8),
attractivite_4_eglise TEXT ENCODING DICT(8),
attractivite_communaute TEXT ENCODING DICT(8),
coef_redevances TEXT ENCODING DICT(8),
min_s_distance_chateau TEXT ENCODING DICT(8),
distance_fusion_agregat TEXT ENCODING DICT(8),
droits_haute_justice_zp TEXT ENCODING DICT(8),
droits_haute_justice_zp_cession TEXT ENCODING DICT(8),
droits_fonciers_zp TEXT ENCODING DICT(8),
droits_fonciers_zp_cession TEXT ENCODING DICT(8),
autres_droits_zp TEXT ENCODING DICT(8),
autres_droits_zp_cession TEXT ENCODING DICT(8),
ponderation_proba_chateau_gs TEXT ENCODING DICT(8),
ponderation_proba_chateau_ps TEXT ENCODING DICT(8)
);

CREATE TABLE global_6_1 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds(sim_name),
annee SMALLINT,
nb_chateaux SMALLINT,
nbgdchateaux SMALLINT,
nb_grands_chateaux SMALLINT,
nb_eglises SMALLINT,
nb_eglises_paroissiales SMALLINT,
distance_eglises FLOAT,
distance_eglises_paroissiales FLOAT,
prop_fp_isoles FLOAT,
charge_fiscale FLOAT,
dist_ppv_agregat FLOAT,
total_duration INTEGER
ratio_charge_fiscale FLOAT
);

CREATE TABLE seigneurs_6_1 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds(sim_name),
annee SMALLINT,
id_seigneur INTEGER,
type TEXT ENCODING DICT(8),
date_apparition TEXT ENCODING DICT(8),
puissance FLOAT,
nb_chateaux_proprio SMALLINT,
nb_chateaux_gardien SMALLINT,
nb_fp_assujettis SMALLINT,
monagregat INTEGER,
geom TEXT ENCODING DICT
);

CREATE TABLE agregats_6_1 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds(sim_name),
annee SMALLINT,
id_agregat INTEGER,
nombre_fp_agregat SMALLINT,
superficie FLOAT,
communaute TEXT ENCODING DICT(8),
monpole INTEGER,
geom TEXT ENCODING DICT
);


CREATE TABLE poles_6_1 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds(sim_name),
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

CREATE TABLE fp_6_1 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds(sim_name),
annee SMALLINT,
id_fp INTEGER,
appartenance_communaute TEXT ENCODING DICT(8),
monagregat INTEGER,
s_materielle FLOAT,
s_religieuse FLOAT,
s_protection FLOAT,
satisfaction FLOAT,
mobile TEXT ENCODING DICT(8),
type_deplacement TEXT ENCODING DICT(8),
deplacement_from TEXT ENCODING DICT(8),
deplacement_to TEXT ENCODING DICT(8),
redevances_acquittees SMALLINT,
geom TEXT ENCODING DICT
);

CREATE TABLE paroisses_6_1 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds(sim_name),
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

CREATE TABLE chateaux_6_1 (
seed TEXT,
sim_name TEXT,
SHARED DICTIONARY (seed) REFERENCES seeds(seed),
SHARED DICTIONARY (sim_name) REFERENCES seeds(sim_name),
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

