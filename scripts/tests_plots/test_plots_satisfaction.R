library(viridis)

sim_FP %>%
  select(seed, sim_name, Annee, Satis, sMat, sRel, sProt) %>%
  gather(key = Type, value = Satisfaction, Satis, sMat, sRel, sProt) %>%
  mutate(SatisfactionInt = as.integer(Satisfaction * 10) / 10) %>%
  group_by(seed, Annee, Type) %>%
  mutate(Total_FP = n()) %>%
  group_by(seed, Annee, SatisfactionInt, Type, Total_FP) %>%
  summarise(NbFP = n()) %>%
  mutate(TxFP = NbFP / Total_FP) %>%
  group_by(Annee, SatisfactionInt, Type) %>%
  summarise(NbFP = mean(NbFP, na.rm = TRUE),
            TxFP = mean(TxFP, na.rm = TRUE)) %>%
  ggplot(aes(factor(Annee), TxFP, fill = SatisfactionInt)) +
    geom_col() +
  scale_y_continuous(labels = percent) +
  scale_fill_viridis(option = "D") +
  facet_wrap(~Type)


sim_FP %>%
  select(seed, sim_name, Annee, Satis) %>%
  mutate(Satis2 = as.integer(Satis * 10) / 10) %>%
  group_by(seed, Annee) %>%
  mutate(Total_FP = n()) %>%
  group_by(seed, Annee, Satis2, Total_FP) %>%
  summarise(NbFP = n()) %>%
  mutate(TxFP = NbFP / Total_FP) %>%
  group_by(Annee, Satis2) %>%
  summarise(NbFP = mean(NbFP, na.rm = TRUE),
            TxFP = mean(TxFP, na.rm = TRUE)) %>%
  ggplot(aes(factor(Annee), Satis2)) +
  geom_point(aes(size = TxFP, fill = -NbFP), shape =  22)

