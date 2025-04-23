### Manipulação dos dados 
library(openxlsx)

dados_GBIF <- read.xlsx("tabelas_dados/dados_brutosGBIF.xlsx")
dados_revisao <- read.xlsx("tabelas_dados/dados_revisao.xlsx")
dados_2023 <- read.xlsx("tabelas_dados/Dados_2023.xlsx", sheet = 2)

dados_GBIF$ref <- "GBIF"

### Convert column 'lon' and 'lat' to numeric
dados_revisao <- dados_revisao %>% mutate(lon = as.numeric(lon))
dados_revisao <- dados_revisao %>% mutate(lat = as.numeric(lat))


###  Join dataframes
library(dplyr)

dados_brutos <- bind_rows(dados_2023, dados_GBIF, dados_revisao)

# Remover ocorrências duplicadas por espécies
library(dplyr)
# Filtrando pontos únicos de longitude e latitude para cada espécie
dados_brutos_unic <- dados_brutos %>%
  group_by(species) %>%
  distinct(lon, lat, .keep_all = TRUE) %>%
  ungroup()

## Excluir coordenadas da China, Finland e Germany 
library(dplyr)
dados_brutos_unic2 <- dados_brutos_unic %>%
  filter(!country %in% c("Finland", "China", "Germany"))

library(ggplot2)
#Mapa dos pontos antes do filtro com o pacote Coordenate
mapa_antesCoordenate <- ggplot() +
  geom_polygon(data = mapa_mundial, aes(x = long, y = lat, group = group), fill = "lightgrey") +  # Ajuste para o fundo
  geom_sf(data = paises, fill = NA, color = "#ffffff") +  # Bordas dos países em branco
  geom_point(data = dados_brutos_unic2, aes(x = lon, y = lat, color = species), 
             alpha = 0.8, size = 1, show.legend = F) +  # Manter legendas visíveis
  labs(
    title = "Distribuição atual das espécies de Trechaleidae",
    x = "Longitude", 
    y = "Latitude",
    caption = "Fonte: Elaborado pelo autor."
  ) +
  scale_color_viridis_d(option = "turbo") +  # Paleta vibrante
  coord_sf(label_graticule = waiver(),
           label_axes = waiver())+
  #theme_minimal(base_size = 14) +  # Estética dark, com fontes maiores
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Ajuste do tamanho na legenda
  theme(
    legend.position = "right",  # Posição da legenda
    legend.background = element_rect(fill = "grey", color = NA),  # Fundo escuro sem borda
    legend.text = element_text(color = "white", size = 10),  # Texto em branco
    legend.title = element_text(color = "white", size = 12),  # Título da legenda
    legend.key = element_rect(fill = "grey40", color = NA),  # Simplicidade na legenda
    panel.grid = element_line(color = "grey")  # Grade do painel discreta
  )

# Limpando os dados com o pacote CoordinateCleaner
# 1. Identificar e excluir pontos no oceano 
library(CoordinateCleaner)

# Limpeza para identificar e excluir pontos localizados no oceano
cleaned_data <- clean_coordinates(
  x = dados_brutos_unic2,
  lon = "lon",
  lat = "lat",
  species = "species",
  tests = c("seas")
)

# Verificar quais pontos foram identificados como localizados no oceano
table(cleaned_data$.summary)  # Verificar os registros limpos (TRUE) e removidos (FALSE)

# Filtrando apenas os registros válidos
cleaned_data_final <- cleaned_data[cleaned_data$.summary == TRUE, ]

#Pontos removidos:
data_excluded_ocean <- cleaned_data[cleaned_data$.summary == FALSE, ]

#save:
library(openxlsx)
write.xlsx(data_excluded_ocean, "tabelas_dados/pontos_excluidos_Coordinate.xlsx")
write.xlsx(cleaned_data_final, "tabelas_dados/bigdata_Coordinate_final.xlsx")

# Mapas após limpeza:

# Mapa com pontos válidos

map_trechaleidae4 <- ggplot() +
  geom_polygon(data = mapa_mundial, aes(x = long, y = lat, group = group), fill = "lightgrey") +  # Ajuste para o fundo
  geom_sf(data = paises, fill = NA, color = "black") +  # Bordas dos países em branco
  geom_point(data = cleaned_data_final, aes(x = lon, y = lat, color = species), 
             alpha = 0.3, size = 1, show.legend = F) +  # Manter legendas visíveis
  labs(
    title = "Distribuição atual das espécies de Trechaleidae",
    x = "Longitude", 
    y = "Latitude",
    caption = "Fonte: Elaborado pelo autor."
  ) +
  scale_color_viridis_d(option = "turbo") +  # Paleta vibrante
  coord_sf(expand = F)+
  #theme_minimal(base_size = 14) +  # Estética dark, com fontes maiores
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Ajuste do tamanho na legenda
  theme(
    legend.position = "right",  # Posição da legenda
    legend.background = element_rect(fill = "white", color = NA),  # Fundo escuro sem borda
    legend.text = element_text(color = "black", size = 10),  # Texto em branco
    legend.title = element_text(color = "black", size = 12),  # Título da legenda
    legend.key = element_rect(fill = "grey40", color = NA),  # Simplicidade na legenda
    panel.grid = element_line(color = "grey")  # Grade do painel discreta
  )
#Salvar 
# Opção 1:
ggsave(
  filename = "map_trechaleidae3.png",
  plot = map_trechaleidae3,
  device = "png",
  scale = 1,
  bg = "white"
)
#Opção 2:
ggsave(
  filename = "map_trechaleidae4.png",
  plot = map_trechaleidae4,
  device = "png",
  scale = 1,
  bg = "white"
)

## Criar limites da América do Sul 

# Defina os limites de longitude e latitude (PARA CORTAR O MAPA)
limites_lon <- c(-140, -30)  # Defina os limites de longitude (exemplo)
limites_lat <- c(-60, 60)     # Defina os limites de latitude (exemplo)

# limite 2
limites_lon2 <- c(-95, -37)  # Defina os limites de longitude (exemplo)
limites_lat2 <- c(-38, 38)     # Defina os limites de latitude (exemplo)

# Mapa da distribuição mostrando só a América do Sul 7
ggplot() +
  geom_polygon(data = mapa_mundial, aes(x = long, y = lat, group = group), fill = "lightgrey") +  # Ajuste para o fundo
  geom_sf(data = paises, fill = NA, color = "white") +  # Bordas dos países em branco
  geom_point(data = cleaned_data_final, aes(x = lon, y = lat, color = species), 
             alpha = 0.3, size = 1, show.legend = F) +  # Manter legendas visíveis
  labs(
    title = "Distribuição atual das espécies de Trechaleidae",
    x = "Longitude", 
    y = "Latitude",
    caption = "Fonte: Elaborado pelo autor."
  ) +
  scale_color_viridis_d(option = "turbo") +  # Paleta vibrante
  coord_sf(expand = F)+
  #theme_minimal(base_size = 14) +  # Estética dark, com fontes maiores
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Ajuste do tamanho na legenda
  coord_sf(xlim = limites_lon, ylim = limites_lat) +
  theme(
    legend.position = "right",  # Posição da legenda
    legend.background = element_rect(fill = "white", color = NA),  # Fundo escuro sem borda
    legend.text = element_text(color = "black", size = 8),  # Texto em branco
    legend.title = element_text(color = "black", size = 12),  # Título da legenda
    legend.key = element_rect(fill = "grey40", color = NA),  # Simplicidade na legenda
    panel.grid = element_line(color = "grey")  # Grade do painel discreta
  )
#Salvar 
# Opção 1:
ggsave(
  filename = "map_trechaleidae5.png",
  plot = map_trechaleidae_Amer,
  device = "png",
  scale = 1,
  bg = "white"
)
#Opção 2:
ggsave(
  filename = "map_trechaleidae6.png",
  plot = map_trechaleidae_Amer2,
  device = "png",
  scale = 1,
  bg = "white"
)

# Mapa com os pontos no oceano 
ggplot() +
  geom_polygon(data = mapa_mundial, aes(x = long, y = lat, group = group), fill = "lightgrey") +  # Ajuste para o fundo
  geom_sf(data = paises, fill = NA, color = "#ffffff") +  # Bordas dos países em branco
  #geom_point(data = data_excluded_ocean, aes(x = lon, y = lat, color = species), 
           #  alpha = 0.8, size = 1, shape = 23, show.legend = F) +  # Manter legendas visíveis
  #geom_text(data = data_excluded_ocean, aes(x = lon, y = lat, label = "🕷️"), size =3.5) +
  geom_text(data = data_excluded_ocean, aes(x = lon, y = lat, label = "🕷️"), size =2.5) +
  labs(
    title = "Dados excluidos pelo pacote CoordinateCleaner",
    x = "Longitude", 
    y = "Latitude",
    caption = "Fonte: Elaborado pelo autor."
  ) +
  scale_color_viridis_d(option = "turbo") +  # Paleta vibrante
  coord_sf(expand = FALSE)+
  #theme_minimal(base_size = 14) +  # Estética dark, com fontes maiores
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Ajuste do tamanho na legenda
  theme(
    legend.position = "right",  # Posição da legenda
    legend.background = element_rect(fill = "grey", color = NA),  # Fundo escuro sem borda
    legend.text = element_text(color = "white", size = 10),  # Texto em branco
    legend.title = element_text(color = "white", size = 12),  # Título da legenda
    legend.key = element_rect(fill = "grey40", color = NA),  # Simplicidade na legenda
    panel.grid = element_line(color = "grey")  # Grade do painel discreta
  )
# Opção 1:
ggsave(
  filename = "data_excluded_ocean2.png",
  plot = points_ocean,
  device = "png",
  scale = 1,
  bg = "white"
)
#Opção 2:
ggsave(
  filename = "data_excluded_ocean3.png",
  plot = points_ocean_size2_5,
  device = "png",
  scale = 1,
  bg = "white"
)

## Exploração dos dados
# Contagem de ocorrências por espécie
occ_sp <- cleaned_data_final%>%
  group_by(species) %>%
  summarise(ocorrencias = n())

#Salvar:
write.xlsx(occ_sp, "tabelas_dados/occ_sp.xlsx")

#Frequência de Ocorrências por Espécie e País
library(dplyr)
freq_sp_localidade <-cleaned_data_final %>%
  group_by(species, country) %>%
  summarise(frequencia = n())

#Salvar
library(openxlsx)
write.xlsx(freq_sp_localidade, "tabelas_dados/freq_sp_localidade.xlsx")

# Gráficos das frequências das espécies por países 
#total de ocorrências por países
library(scales)
library(ggplot2)
map_freq_paises2 <- ggplot(freq_sp_localidade, aes(x = country, y = species, fill = frequencia)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "turbo", trans = scales::modulus_trans(p = 0.1), labels = label_number(accuracy = 1)) +  # Escala log para visualizar melhor
  labs(title = "Número de ocorrências de Trechaleidae por País",
       x = "Países",
       y = "Espécies",
       fill = "Frequência")+#,
  #caption = "Fonte: Dados retirados do GBIF") +
  theme_light() +
  theme(
    axis.text.y = element_text(size = 4.5),  # Tamanho do texto do eixo Y
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Tamanho do texto do eixo X
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Tamanho do título
    axis.title.x = element_text(size = 12),  # Tamanho do título do eixo X
    axis.title.y = element_text(size = 12),  # Tamanho do título do eixo Y
    legend.title = element_text(size = 12),  # Tamanho do título da legenda
    legend.text = element_text(size = 10)   # Tamanho do texto da legenda
  ) 
#Opção 2:
map_freq_paises4 <- ggplot(freq_sp_localidade, aes(x = country, y = species, fill = frequencia)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "turbo", trans = scales::modulus_trans(p = 0.1), labels = label_number(accuracy = 1)) +  # Escala log para visualizar melhor
  labs(title = "Número de ocorrências de Trechaleidae por País",
       x = "Países",
       y = "Espécies",
       fill = "Frequência")+#,
  #caption = "Fonte: Dados retirados do GBIF") +
  theme_light() +
  theme(
    axis.text.y = element_text(size = 3),  # Tamanho do texto do eixo Y
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),  # Tamanho do texto do eixo X
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),  # Tamanho do título
    axis.title.x = element_text(size = 10),  # Tamanho do título do eixo X
    axis.title.y = element_text(size = 10),  # Tamanho do título do eixo Y
    legend.title = element_text(size = 12),  # Tamanho do título da legenda
    legend.text = element_text(size = 8)   # Tamanho do texto da legenda
  ) 
# Opção 3:
map_freq_paises7 <- ggplot(freq_sp_localidade, aes(x = country, y = species, fill = frequencia)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "turbo", trans = scales::modulus_trans(p = 0.1), labels = label_number(accuracy = 1)) +  # Escala log para visualizar melhor
  labs(title = "Número de ocorrências de Trechaleidae por País",
       x = "Países",
       y = "Espécies",
       fill = "Frequência")+#,
  #caption = "Fonte: Dados retirados do GBIF") +
  theme_light() +
  theme(
    axis.text.y = element_text(size = 3),  # Tamanho do texto do eixo Y
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),  # Tamanho do texto do eixo X
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),  # Tamanho do título
    axis.title.x = element_text(size = 7),  # Tamanho do título do eixo X
    axis.title.y = element_text(size = 7),  # Tamanho do título do eixo Y
    legend.title = element_text(size = 7),  # Tamanho do título da legenda
    legend.text = element_text(size = 4)   # Tamanho do texto da legenda
  ) 


#save:
ggsave(
  filename = "map_freq_paises9.png",  # Nome do arquivo salvo (formato PNG)
  plot = map_freq_paises7,            # Especifica o gráfico a ser salvo
  device = "png",                      # Define o formato da imagem (PNG)
  scale = 1.5,                         # Ajusta o tamanho do gráfico (1.5x maior)
  bg = NULL,                        # Define o fundo branco (por padrão seria transparente)
  width = 8,                           # Largura do gráfico salvo em polegadas
  height = 5,                          # Altura do gráfico salvo em polegadas
  dpi = 600                            # Define a resolução da imagem (300 dpi = alta qualidade)
)

## Filtrando os dados com o pacote spThim 

install.packages("spThin")  # Caso ainda não tenha instalado
library(spThin)

# Aplicando o filtro para cada espécie individualmente
dados_filtrados_spThin <- thin(
  loc.data = cleaned_data_final,   # DataFrame com os dados brutos
  lat.col = "lat",    # Nome da coluna de latitude
  long.col = "lon",  # Nome da coluna de longitude
  spec.col = "species",    # Coluna de identificação da espécie
  thin.par = 10,           # Distância mínima de 10 km entre os pontos da MESMA espécie
  reps = 100,              # Número de repetições para encontrar o melhor subconjunto
  locs.thinned.list.return = TRUE,  # Retorna os pontos filtrados como lista
  write.files = FALSE      # Não salva arquivos automaticamente
)

install.packages("writexl")  # Ou use openxlsx se preferir
library(writexl)

write_xlsx(dados_filtrados_spThin, "dados_filtrados_sp_Thim.xlsx")




