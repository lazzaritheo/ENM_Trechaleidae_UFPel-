# Explorando os dados com pacotes 

#Mapa
#plot data to get an overview
library(ggplot2)
wm <- borders("world", colour = "gray50", fill = "white" )
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = final_data,
             aes(x = lon, y = lat),
             colour = "orange2",
             size = 0.5,
             shape = 7) +
  theme_bw()
# Diferenciando as espécies
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = final_data,
             aes(x = lon, y = lat, colour = species),
             size = 1, show.legend = F, alpha = 0.2) +
  #scale_color_viridis_d() +
  theme_minimal()

#Package CoordinateCleaner

# 1. Identificar e excluir pontos no oceano 
library(CoordinateCleaner)

# Limpeza para identificar e excluir pontos localizados no oceano
cleaned_data <- clean_coordinates(
  x = occ_Trechaleidae,
  lon = "lon",
  lat = "lat",
  species = "species",
  tests = c("seas")
)

# Verificar quais pontos foram identificados como localizados no oceano
table(cleaned_data$.summary)  # Verificar os registros limpos (TRUE) e removidos (FALSE)

# Filtrando apenas os registros válidos
final_data <- cleaned_data[cleaned_data$.summary == TRUE, ]

#Pontos removidos:
exclude <- cleaned_data[cleaned_data$.summary == FALSE, ]

library(openxlsx)
write.xlsx(exclude, "pontos_excluidos_Coordinate.xlsx")





# 2. Identificar Alta Densidade de Pontos de Ocorrência

#a) Mapas de Calor (Heatmap) usando ggplot2
library(ggplot2)

# Carregar o mapa da América do Sul
mapa <- ne_countries(scale = "medium",  returnclass = "sf")

# Criar o gráfico com o mapa base
ggplot() +
  geom_sf(data = mapa, fill = "gray90", color = "black") +  # Adiciona o shape da América do Sul
  geom_point(data = final_data, aes(x = lon, y = lat), color = "red", size = 1) +  # Adiciona os pontos de ocorrência
  stat_density_2d(data = final_data, aes(x = lon, y = lat, fill = after_stat(level)), geom = "polygon", color = "black") +
  scale_fill_viridis_c() +
  coord_sf(xlim = c(min(final_data$lon) - 5, max(final_data$lon) + 5),
           ylim = c(min(final_data$lat) - 5, max(final_data$lat) + 5), expand = FALSE) +
  theme_minimal() +
  labs(title = "Mapa de Calor de Densidade de Pontos de Ocorrência",
       x = "Longitude", y = "Latitude", fill = "Densidade")



#b) Filtragem Espacial com spThin (NÂO TA DANDO CERTO)
library(spThin)

# Filtragem com distância mínima de 10 km entre os pontos
thin_data <- thin(
  loc.data = final_data,
  lat.col = "lat",
  long.col = "lon",
  spec.col = "species",
  thin.par = 10,   # Distância mínima (em km)
  reps = 100,      # Número de repetições
  write.files = FALSE
)

# Selecionar a replicação com o maior número de pontos
final_thinned_data <- thin_data[[which.max(sapply(thin_data, nrow))]]

#exclui espaços extras
final_data$species <- trimws(final_data$species)

