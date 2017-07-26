library(tidyverse)
library(ggthemes)
library(scales)
library(dbplyr)
library(DBI)
library(MonetDBLite)


conMonetDB <- MonetDBLite::src_monetdblite("~/outputs_TR8/testMonetDB")

FP_data <- tbl(src = conMonetDB, "fp") %>% filter(sim_name == '4_4_A')

FP_data %>% show_query()

system.time({
nbFP <- FP_data %>%
  group_by(seed, annee, sim_name) %>%
  summarise(nbtotal = n())

FP_satis_data <- FP_data %>%
  select(seed, sim_name, annee, satis, smat, srel, sprot)

globale <- FP_satis_data %>%
  mutate(satisint = as.integer(satis * 10L)) %>%
  group_by(seed, sim_name, annee, satisint) %>%
  summarise(nb = n(), type = "globale", satisfaction = satisint / 10) %>%
  select(seed, sim_name, annee, nb, type, satisfaction)

materielle <- FP_satis_data %>%
  mutate(smatint = as.integer(smat * 10L)) %>%
  group_by(seed, sim_name, annee, smatint) %>%
  summarise(nb = n(), type = "materielle", satisfaction = smatint / 10) %>%
  select(seed, sim_name, annee, nb, type, satisfaction)

protection <- FP_satis_data %>%
  mutate(sprotint = as.integer(sprot * 10L)) %>%
  group_by(seed, sim_name, annee, sprotint) %>%
  summarise(nb = n(), type = "protection", satisfaction = sprotint / 10) %>%
  select(seed, sim_name, annee, nb, type, satisfaction)

religieuse <- FP_satis_data %>%
  mutate(srelint = as.integer(srel * 10L)) %>%
  group_by(seed, sim_name, annee, srelint) %>%
  summarise(nb = n(), type = "religieuse", satisfaction = srelint / 10) %>%
  select(seed, sim_name, annee, nb, type, satisfaction)
  
satis_all <- globale %>%
  union_all(materielle) %>%
  union_all(protection) %>%
  union_all(religieuse) %>%
  ungroup() %>%
  left_join(nbFP, by = c("seed", "sim_name", "annee")) %>%
  ungroup() %>%
  mutate(txFP = (nb * 1.0) / (nbtotal * 1.0)) %>%
  group_by(annee, type, satisfaction) %>%
  summarise(txFP = mean(txFP))

plot_data <- satis_all %>%
  ungroup() %>%
  collect() %>%
  mutate(type = if_else(type == "globale", "Globale", type)) %>%
  mutate(type = if_else(type == "materielle", "Matérielle", type)) %>%
  mutate(type = if_else(type == "protection", "Protection", type)) %>%
  mutate(type = if_else(type == "religieuse", "Religieuse", type))
})

ggplot(plot_data, aes(factor(annee), txFP, fill = factor(satisfaction), group = factor(satisfaction))) +
  geom_col() +
  facet_grid(type~.) +
  scale_fill_brewer(name = "Satisfaction", type = "div", palette = "RdYlBu", direction = 1) +
  scale_y_continuous(labels = percent) +
  ggtitle("Évolution de la satisfaction des foyers paysans") +
  xlab("Temps") + ylab("Distribution de la satisfaction des foyers paysans") +
  labs(subtitle = "Variabilité : Moyenne des réplications") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top", nrow = 1, title.hjust = 0.5,
                             label.position = "bottom", label.hjust = 0.5))


ggsave(last_plot(), file = "FP_Satisfaction_essai1.png", height = 20, width = 30, units=  "cm", dpi = 300)

ggplot(plot_data, aes(factor(annee), satisfaction, size = txFP, fill = factor(satisfaction))) +
  geom_point(shape = 21) +
  facet_grid(type~.) +
  scale_fill_brewer(name = "Satisfaction", type = "div", palette = "RdYlBu", direction = 1) +
  scale_y_continuous(labels = percent) +
  ggtitle("Évolution de la satisfaction des foyers paysans") +
  xlab("Temps") + ylab("Distribution de la satisfaction des foyers paysans") +
  labs(subtitle = "Variabilité : Moyenne des réplications") +
  theme(legend.position = "bottom") +
  guides(size = guide_legend(title = "Part des FP",title.position = "top", nrow = 1, title.hjust = 0.5,
                             label.position = "bottom", label.hjust = 0.5),
         fill = guide_legend(title.position = "top", nrow = 1, title.hjust = 0.5,
                             label.position = "bottom", label.hjust = 0.5))

ggsave(last_plot(), file = "FP_Satisfaction_essai2.png", height = 20, width = 30, units=  "cm", dpi = 300)

ggplot(plot_data, aes(factor(annee), satisfaction, size = txFP, fill = txFP)) +
  geom_point(shape = 21) +
  facet_grid(type~.) +
  scale_fill_distiller(name = "Part des foyers paysans\npour chaque valeur\nde satisfaction", type = "seq", palette = "Reds", direction = 1) +
  ggtitle("Évolution de la satisfaction des foyers paysans") +
  xlab("Temps") + ylab("Satisfaction des foyers paysans") +
  labs(subtitle = "Variabilité : Moyenne des réplications") +
  theme(legend.position = "right") +
  guides(size = guide_legend(title = "Part des foyers paysans\npour chaque valeur\nde satisfaction",title.position = "top", ncol = 1, title.hjust = 0.5,
                             label.position = "right", label.hjust = .5),
         fill = guide_legend(title.position = "top", ncol = 1, title.hjust = 0.5,
                             label.position = "right", label.hjust = .5))

ggsave(last_plot(), file = "FP_Satisfaction_essai3.png", height = 20, width = 30, units=  "cm", dpi = 300)

system.time({
  fpData <- tbl(src = conMonetDB, "fp")
  
  fpData %>% filter(sim_name == "4_4_B") %>% sample_n(1E5) -> blob
  blob %>%
    group_by(sim_name, annee) %>%
    count()
})

system.time({
  blob %>%
    sample_n(1E5) %>%
    group_by(sim_name, annee) %>%
    count %>%
    print()
})




system.time({
    satisfaction_data <- FP_data %>%
      select(annee, smat, srel, sprot, satis) %>%
      group_by(annee) %>%
      summarise(a = quantile(satis, probs = 0.1)) %>%
      show_query()
      rename(
        Globale = satis,
        Matérielle = smat,
        Protection = sprot,
        Religieuse = srel) %>%
      group_by(annee) %>%
      summarise(d1 = quantile(Globale, probs = 0.1),
                d2 = quantile(Globale, probs = 0.2)) %>%
      gather(key = Type, value = Satisfaction, -annee) %>%
      group_by(annee) %>%
      summarise(d1 = quantile(Satisfaction, probs = 0.1),
                d2 = quantile(Satisfaction, probs = 0.2)) %>%
      collect()
      
      collect(tbl(conMonetDB, dbplyr::sql('SELECT QUANTILE(satis,0.1) AS Q1, QUANTILE(satis,0.2) AS Q2, QUANTILE(satis,0.3) AS Q3, QUANTILE(satis,0.4) AS Q4, QUANTILE(satis,0.5) AS Q5, QUANTILE(satis,0.6) AS Q6, QUANTILE(satis,0.7) AS Q7, QUANTILE(satis,0.8) AS Q8, QUANTILE(satis,0.9) AS Q9, QUANTILE(satis,1) AS Q10,  FROM fp GROUP BY "annee"')))
      
})

ggplot(satisfaction_data, aes(annee, Satisfaction, col = Type, fill = Type)) +
  geom_violin(aes(group = factor(Annee))) +
  facet_wrap(~ Type) +
  geom_smooth(data = satisfaction_data %>%
                group_by(annee) %>%
                sample_n(size = 100, replace = FALSE) %>%
                ungroup(),
              alpha = .3, se = FALSE, na.rm = TRUE, method = "gam", formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = "none") +
  ggtitle("Évolution de la satisfaction des FP\n(Échantillon de 4000 FP / an)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(subtitle = "Variabilité : Foyers Paysans et Réplications")


DBI::dbDisconnect(conMonetDB)
MonetDBLite::monetdblite_shutdown()



library(dplyr)

dbdir <- file.path(tempdir(), "dplyrdir")
my_db_monetdb <- MonetDBLite::src_monetdblite(dbdir)
flights <- nycflights13::flights
flights$time_hour <- as.numeric( flights$time_hour )
flights_monetdb <- copy_to(my_db_monetdb, flights,overwrite = TRUE, temporary = FALSE, indexes = list(
  c("year", "month", "day"), "carrier", "tailnum"))

flights_monetdb %>% group_by(month) %>% summarise(q1 = quantile(dep_delay)) %>% show_query()
collect(tbl(my_db_monetdb, dbplyr::sql('SELECT QUANTILE(dep_delay,0.1) AS Q1, QUANTILE(dep_delay, 0.5) AS Q5, MEDIAN(dep_delay) AS MED, CORR(arr_delay, dep_delay) AS C FROM flights GROUP BY "month"')))
MonetDBLite::monetdblite_shutdown()
