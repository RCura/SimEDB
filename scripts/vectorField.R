library(tidyverse)
library(sf)
library(sp)
library(spatstat)
library(maptools)
library(viridis)

foo <- sim_FP %>%
  filter(seed %in% {
    sample_n(., size = 1) %>% pull(seed) %>% unique(.)
  }) %>%
  mutate(geom = gsub(x = geom, pattern = "{", replacement = "", fixed = TRUE)) %>%
  mutate(geom = gsub(x = geom, pattern = "}", replacement = "", fixed = TRUE)) %>%
  separate(col = geom, into = c("X", "Y", "Z"), sep = ",", convert = TRUE) %>%
  select(self, X, Y, Annee) %>%
  ungroup() %>%
  st_as_sf(coords = c("X", "Y"))

grille <- foo %>%
  st_make_grid(n = 20) %>%
  st_sf(ID = 1:length(.), .)

transitions <- foo %>%
  pull(Annee) %>%
  unique()

transitionsTemps <- list(T1 = c(transitions[1:(length(transitions) - 1)]),
                          T2 = c(transitions[2:length(transitions)])) %>%
  transpose() %>%
  simplify_all()

allGrids <- data_frame(
  ID = integer(),
  angleMoy = double(),
  angleMed = double(),
  Nb = integer(),
  X = double(),
  Y = double(),
  Annee = character()
)

for (i in transitionsTemps) {
  
  bar <- foo %>%
    filter(Annee %in% !!i) %>%
    group_by(self) %>%
    summarize(do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    mutate(length = st_length(geometry)) %>%
    filter(length > 0)
  
  
  pspbar <- bar %>%
    as(., "Spatial") %>%
    as(., "SpatialLines") %>%
    as(., "psp")
  
  pspangles <- pspbar %>%
    angles.psp(directed = TRUE)
  
  sfLines <- as(pspbar, "SpatialLines") %>%
    st_as_sf() %>%
    mutate(angle = pspangles)
  
  
  foobar <- grille %>%
    st_join(sfLines) %>%
    group_by(ID) %>%
    summarise(angleMoy = mean(angle, na.rm = TRUE),
              angleMed = median(angle, na.rm = TRUE),
              Nb = n()) %>%
    select(ID, angleMoy, angleMed, Nb)
  
  foobarCoords <- foobar %>%
    st_centroid() %>%
    st_coordinates() %>%
    as_tibble()
  
  superfoobar <- foobar %>%
    mutate(X = foobarCoords$X) %>%
    mutate(Y = foobarCoords$Y) %>%
    st_set_geometry(NULL) %>%
    mutate(Annee = paste(i[1], i[2], sep = "-"))
  
  allGrids <- allGrids %>%
    bind_rows(superfoobar)
}

tempsOrdonne <- lapply(transitionsTemps, function(x){sprintf("%s-%s", x[1], x[2])}) %>%
  unlist()
allGrids <- allGrids %>%
  mutate(Annee = factor(Annee, levels = tempsOrdonne))


blob <- ggplot(allGrids, aes(X, Y)) +
  geom_spoke(aes(angle = angleMed, col = Nb), size = 1,
             radius = 2.5E3, arrow = arrow(length = unit(.0001, units = "cm"), angle = .01, ends = "first")) +
  scale_color_viridis(direction = -1, option = "C", begin = .0, end = 1) +
  facet_wrap(~Annee) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "bottom")

ggsave(blob, filename = "vectorFields.png",  width = 30, height = 20, units = "cm", dpi = 800)

library(gganimate)
library(animation)
ani.options(ani.width = 1024, ani.height = 1024)

gganim <- ggplot(allGrids, aes(X, Y, frame = Annee)) +
  geom_spoke(aes(angle = angleMed, col = Nb), size = 1,
             radius = 2.5E3, arrow = arrow(length = unit(.0001, units = "cm"), angle = .01, ends = "first")) +
  scale_color_viridis(direction = -1, option = "C", begin = .0, end = 1) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "bottom")

gganimate(blob, filename = "vectorFields.gif", interval = .4)

