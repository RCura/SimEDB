library(tidyverse)
library(ggthemes)
library(scales)
library(dbplyr)
library(DBI)
library(MonetDBLite)


conMonetDB <- MonetDBLite::src_monetdblite("D:/outputs_TR8/testMonetDB.db")



FP_data <- tbl(src = conMonetDB, "fp") %>% filter(sim_name == "4_4_B") %>% show_query

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
