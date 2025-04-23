# ----- Observações ----- #

# - o script é a rotina completa de modelagem da adequabilidade ambiental,
# avaliação estatística e cálculo da importância relativa das variáveis.
#
# - o tópico 04 (mapas) depende exclusivamente dos anteriores, portanto é 
# necessário rodá-los antes.
#
# - caso queira utilizar uma paleta de cores diferente das propostas,
# utilize o script "script_mapas.R" para criar a nova paleta e gerar as
# novas figuras.

# ----------- ------------- #

# 00. Pacotes e definições gerais -----
library(ggspatial)
library(raster)
library(SSDM)
library(tidyverse)

options(scipen = 999) # remover notação científica dos dados

pal <- c("#76B9A5", "#E8E1A7", "#E4AD73", "#DC6D37", "#E02423") # paleta de cores original
pal_cool <- c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51") # cores frias
pal_alt <- c("#E76F51", "#F4A261", "#E9C46A", "#2A9D8F", "#264653") # inversão da pal_cool

br <- geobr::read_country(year = 2020) # shapefile brasileiro para mapas

# 01. Carregar dados para modelagem -----

## Variáveis ambientais -----
bio_sp1 <- load_var(
  path = "worldclim/p_longipes/brasil/"
)

bio_sp2 <- load_var(
  path = "worldclim/p_azul/brasil/"
)

bio_sp3 <- load_var(
  path = "worldclim/p_galianoae/brasil/"
)

bio_sp4 <- load_var(
  path = "worldclim/p_ornata/brasil/"
)

## Dados de ocorrência -----
sp1 <- load_occ(
  path = "data/",
  Env = bio_sp1,
  Xcol = "longitude",
  Ycol = "latitude",
  file = "p_longipes.csv",
  sep = ","
)

sp2 <- load_occ(
  path = "data/",
  Env = bio_sp2,
  Xcol = "longitude",
  Ycol = "latitude",
  file = "p_azul.csv",
  sep = ","
)

sp3 <- load_occ(
  path = "data/",
  Env = bio_sp3,
  Xcol = "longitude",
  Ycol = "latitude",
  file = "p_galianoae.csv",
  sep = ","
)

sp4 <- load_occ(
  path = "data/",
  Env = bio_sp4,
  Xcol = "longitude",
  Ycol = "latitude",
  file = "p_ornata.csv",
  sep = ","
)

### Renomear coluna da espécie -----
sp1 <- sp1 %>%
  rename(p_longipes = especie) %>%
  mutate(p_longipes = 1)

sp2 <- sp2 %>%
  rename(p_azul = especie) %>%
  mutate(p_azul = 1)

sp3 <- sp3 %>%
  rename(p_galianoae = especie) %>%
  mutate(p_galianoae = 1)

sp4 <- sp4 %>%
  rename(p_ornata = especie) %>%
  mutate(p_ornata = 1)

# 02. Modelagem de adequabilidade ambiental -----

## Treinar modelos -----
modelo_sp1 <- ensemble_modelling(
  algorithms = c("GLM", "GAM", "GBM", "MAXENT", "SVM"),
  Occurrences = sp1,
  Env = bio_sp1,
  Xcol = "longitude",
  Ycol = "latitude",
  rep = 15,
  name = "p_longipes_ens",
  save = TRUE,
  path = "modelos/",
  cores = 3,
  cv = "holdout",
  cv.param = c(0.75, 5),
  bin.thresh = "SES",
  axes.metric = "Pearson",
  ensemble.metric = "AUC",
  ensemble.thresh = 0.8,
  weight = TRUE
)

modelo_sp2 <- ensemble_modelling(
  algorithms = c("GLM", "GAM", "GBM", "MAXENT", "SVM"),
  Occurrences = sp2,
  Env = bio_sp2,
  Xcol = "longitude",
  Ycol = "latitude",
  rep = 15,
  name = "p_azul_ens",
  save = TRUE,
  path = "modelos/",
  cores = 3,
  cv = "holdout",
  cv.param = c(0.75, 5),
  bin.thresh = "SES",
  axes.metric = "Pearson",
  ensemble.metric = "AUC",
  ensemble.thresh = 0.8,
  weight = TRUE
)

modelo_sp3 <- ensemble_modelling(
  algorithms = c("GLM", "GAM", "GBM", "MAXENT", "SVM"),
  Occurrences = sp3,
  Env = bio_sp3,
  Xcol = "longitude",
  Ycol = "latitude",
  rep = 15,
  name = "p_galianoae_ens",
  save = TRUE,
  path = "modelos/",
  cores = 3,
  cv = "holdout",
  cv.param = c(0.75, 5),
  bin.thresh = "SES",
  axes.metric = "Pearson",
  ensemble.metric = "AUC",
  ensemble.thresh = 0.8,
  weight = TRUE
)

modelo_sp4 <- ensemble_modelling(
  algorithms = c("GLM", "GAM", "GBM", "MAXENT", "SVM"),
  Occurrences = sp4,
  Env = bio_sp4,
  Xcol = "longitude",
  Ycol = "latitude",
  rep = 15,
  name = "p_ornata_ens",
  save = TRUE,
  path = "modelos/",
  cores = 3,
  cv = "holdout",
  cv.param = c(0.75, 5),
  bin.thresh = "SES",
  axes.metric = "Pearson",
  ensemble.metric = "AUC",
  ensemble.thresh = 0.8,
  weight = TRUE
)

# 03. Procedimentos pós-modelagem -----

## Avaliação estatística (AUC) -----
m_auc_sp1 <- modelo_sp1@evaluation %>%
  mutate(species = "p_longipes", .before = threshold)

m_auc_sp2 <- modelo_sp2@evaluation %>%
  mutate(species = "p_azul", .before = threshold)

m_auc_sp3 <- modelo_sp3@evaluation %>%
  mutate(species = "p_galianoae", .before = threshold)

m_auc_sp4 <- modelo_sp4@evaluation %>%
  mutate(species = "p_ornata", .before = threshold)

### Criar matriz de avaliação com todas espécies -----
m_auc_spp <- rbind(
  m_auc_sp1,
  m_auc_sp2,
  m_auc_sp3,
  m_auc_sp4
)

## Contribuição das variáveis (Pearson) -----
var_imp_sp1 <- modelo_sp1@variable.importance
var_imp_sp2 <- modelo_sp2@variable.importance
var_imp_sp3 <- modelo_sp3@variable.importance
var_imp_sp4 <- modelo_sp4@variable.importance

# 04. Criar mapas de adequabilidade -----

## Criar dataframes de adequabilidade -----
adeq_sp1 <- as.data.frame(modelo_sp1@projection, xy = TRUE) %>%
  rename(
    longitude = x,
    latitude = y,
    adeq = Probability
  )

adeq_sp2 <- as.data.frame(modelo_sp2@projection, xy = TRUE) %>%
  rename(
    longitude = x,
    latitude = y,
    adeq = Probability
  )

adeq_sp3 <- as.data.frame(modelo_sp3@projection, xy = TRUE) %>%
  rename(
    longitude = x,
    latitude = y,
    adeq = Probability
  )

adeq_sp4 <- as.data.frame(modelo_sp4@projection, xy = TRUE) %>%
  rename(
    longitude = x,
    latitude = y,
    adeq = Probability
  )

## Criar plots -----
p_longipes <- ggplot() +
  geom_raster(data = adeq_sp1, aes(longitude, latitude, fill = adeq)) +
  geom_sf(data = br, color = "#464646", fill = NA) +
  geom_point(
    data = sp1,
    aes(x = longitude, y = latitude, color = "black"), size = 1
  ) +
  scale_fill_gradientn(
    colours = pal,
    na.value = NA,
    limits = c(0.00, 1.00)
  ) + # parâmetros da escala de cor
  scale_color_manual(
    values = "black",
    name = NULL,
    labels = expression(italic("Paradossenus longipes (Taczanowski, 1874)"))
  ) +
  coord_sf() +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(0.25, "in"), pad_y = unit(0.30, "in")
  ) +
  annotation_scale(location = "br", width_hint = 0.5) +
  labs(
    x = "Longitude", # texto do eixo x
    y = "Latitude", # texto do eixo y
    fill = "Adequabilidade\n    Ambiental"
  ) +
  guides(
    fill = guide_colorbar(order = 1),
    shape = guide_legend(order = 2)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgrey", linewidth = 0.25),
    panel.grid.minor = element_line(color = "lightgrey", linewidth = 0.25),
    legend.title = element_text(face = "bold", size = 12, vjust = 1.25),
    legend.text = element_text(size = 10),
    legend.key.width = unit(2, "cm"),
    # legend.box = "vertical",
    legend.position = "bottom",
    legend.justification = "center"
  )

p_azul <- ggplot() +
  geom_raster(data = adeq_sp2, aes(longitude, latitude, fill = adeq)) +
  geom_sf(data = br, color = "#464646", fill = NA) +
  geom_point(
    data = sp2,
    aes(x = longitude, y = latitude, color = "black"), size = 1
  ) +
  scale_fill_gradientn(
    colours = pal,
    na.value = NA,
    limits = c(0.00, 1.00)
  ) + # parâmetros da escala de cor
  scale_color_manual(
    values = "black",
    name = NULL,
    labels = expression(italic("Paratrechalea azul Carico, 2005"))
  ) +
  coord_sf() +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(0.25, "in"), pad_y = unit(0.30, "in")
  ) +
  annotation_scale(location = "br", width_hint = 0.5) +
  labs(
    x = "Longitude", # texto do eixo x
    y = "Latitude", # texto do eixo y
    fill = "Adequabilidade\n    Ambiental"
  ) +
  guides(
    fill = guide_colorbar(order = 1),
    shape = guide_legend(order = 2)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgrey", linewidth = 0.25),
    panel.grid.minor = element_line(color = "lightgrey", linewidth = 0.25),
    legend.title = element_text(face = "bold", size = 12, vjust = 1.25),
    legend.text = element_text(size = 10),
    legend.key.width = unit(2, "cm"),
    # legend.box = "vertical",
    legend.position = "bottom",
    legend.justification = "center"
  )

p_galianoae <- ggplot() +
  geom_raster(data = adeq_sp3, aes(longitude, latitude, fill = adeq)) +
  geom_sf(data = br, color = "#464646", fill = NA) +
  geom_point(
    data = sp3,
    aes(x = longitude, y = latitude, color = "black"), size = 1
  ) +
  scale_fill_gradientn(
    colours = pal,
    na.value = NA,
    limits = c(0.00, 1.00)
  ) + # parâmetros da escala de cor
  scale_color_manual(
    values = "black",
    name = NULL,
    labels = expression(italic("Paratrechalea galianoae Carico, 2005"))
  ) +
  coord_sf() +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(0.25, "in"), pad_y = unit(0.30, "in")
  ) +
  annotation_scale(location = "br", width_hint = 0.5) +
  labs(
    x = "Longitude", # texto do eixo x
    y = "Latitude", # texto do eixo y
    fill = "Adequabilidade\n    Ambiental"
  ) +
  guides(
    fill = guide_colorbar(order = 1),
    shape = guide_legend(order = 2)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgrey", linewidth = 0.25),
    panel.grid.minor = element_line(color = "lightgrey", linewidth = 0.25),
    legend.title = element_text(face = "bold", size = 12, vjust = 1.25),
    legend.text = element_text(size = 10),
    legend.key.width = unit(2, "cm"),
    # legend.box = "vertical",
    legend.position = "bottom",
    legend.justification = "center"
  )

p_ornata <- ggplot() +
  geom_raster(data = adeq_sp4, aes(longitude, latitude, fill = adeq)) +
  geom_sf(data = br, color = "#464646", fill = NA) +
  geom_point(
    data = sp4,
    aes(x = longitude, y = latitude, color = "black"), size = 1
  ) +
  scale_fill_gradientn(
    colours = pal,
    na.value = NA,
    limits = c(0.00, 1.00)
  ) + # parâmetros da escala de cor
  scale_color_manual(
    values = "black",
    name = NULL,
    labels = expression(italic("Paratrechalea ornata (Mello-Leitão, 1943)"))
  ) +
  coord_sf() +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(0.25, "in"), pad_y = unit(0.30, "in")
  ) +
  annotation_scale(location = "br", width_hint = 0.5) +
  labs(
    x = "Longitude", # texto do eixo x
    y = "Latitude", # texto do eixo y
    fill = "Adequabilidade\n    Ambiental"
  ) +
  guides(
    fill = guide_colorbar(order = 1),
    shape = guide_legend(order = 2)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgrey", linewidth = 0.25),
    panel.grid.minor = element_line(color = "lightgrey", linewidth = 0.25),
    legend.title = element_text(face = "bold", size = 12, vjust = 1.25),
    legend.text = element_text(size = 10),
    legend.key.width = unit(2, "cm"),
    # legend.box = "vertical",
    legend.position = "bottom",
    legend.justification = "center"
  )

## Exportar figuras -----
ggsave(
  filename = "figuras/ens_p_lonngipes.png",
  plot = p_longipes,
  device = "png",
  scale = 2,
  bg = "white"
)

ggsave(
  filename = "figuras/ens_p_azul.png",
  plot = p_azul,
  device = "png",
  scale = 2,
  bg = "white"
)

ggsave(
  filename = "figuras/ens_p_galianoae.png",
  plot = p_galianoae,
  device = "png",
  scale = 2,
  bg = "white"
)

ggsave(
  filename = "figuras/ens_p_ornata.png",
  plot = p_ornata,
  device = "png",
  scale = 2,
  bg = "white"
)
