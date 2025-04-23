# 00. Paleta de cores e shapefiles -----
pal <- c("#76B9A5", "#E8E1A7", "#E4AD73", "#DC6D37", "#E02423") # paleta de cores original
pal_cool <- c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51") # cores frias
pal_alt <- c("#E76F51", "#F4A261", "#E9C46A", "#2A9D8F", "#264653") # inversão da pal_cool

br <- geobr::read_country(year = 2020) # shapefile brasileiro para mapas

# 01. Carregar mapas de adequabilidade -----
adeq_sp1 <- as.data.frame(raster("modelos/p_longipes_ens/Rasters/Probability.tif"), xy = TRUE) %>%
  rename(
    longitude = x,
    latitude = y,
    adeq = Probability
  )

adeq_sp2 <- as.data.frame(raster("modelos/p_azul_ens/Rasters/Probability.tif"), xy = TRUE) %>%
  rename(
    longitude = x,
    latitude = y,
    adeq = Probability
  )

adeq_sp3 <- as.data.frame(raster("modelos/p_galianoae_ens/Rasters/Probability.tif"), xy = TRUE) %>%
  rename(
    longitude = x,
    latitude = y,
    adeq = Probability
  )

adeq_sp4 <- as.data.frame(raster("modelos/p_ornata_ens/Rasters/Probability.tif"), xy = TRUE) %>%
  rename(
    longitude = x,
    latitude = y,
    adeq = Probability
  )

# 02. Gerar novos maps -----
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

## Exportar figuras atualizadas ------
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
