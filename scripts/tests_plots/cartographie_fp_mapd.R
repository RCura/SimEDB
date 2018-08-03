
results_fp <- list.files(pattern =
                           sprintf("%s_results_FP.csv", prefixe_files)) %>%
  map(~read_csv(file = ., quote = "'")) %>%
  reduce(rbind)   %>%
  rename(seed = `string(seed)`) %>%
  mutate(seed = as.character(seed)) %>%
  filter(seed == "0.721871283469026")

agregats_map <- results_agregats %>%
  filter(seed == "0.721871283469026") %>%
  st_as_sf(wkt = "geom") %>%
  st_centroid()

blob <- as.data.frame(st_coordinates(agregats_map))
agregats_map <- bind_cols(agregats_map, blob)


map_fp <- results_fp %>%
          mutate(geom = gsub(x = geom, pattern = "{", replacement = "", fixed = TRUE)) %>%
  mutate(geom = gsub(x = geom, pattern = "}", replacement = "", fixed = TRUE)) %>%
  separate(col = geom, into = c("X", "Y", "Z"), sep = ",", convert = TRUE) %>%
  filter(monAgregat == "nil") %>%
  select(self, X, Y, Annee)

ggplot(map_fp %>%
         filter(Annee %in% c(820, 940, 1040, 1160))) +
  aes(X, Y) +
  geom_point(size = 0.00001) +
  geom_point(data = agregats_map %>% filter(Annee %in% c(820, 940, 1040, 1160)), aes(size = nbFP)) +
  facet_wrap(~Annee, nrow = 1) +
  scale_size_continuous(range = c(0.1, 5)) +
  coord_fixed() 

