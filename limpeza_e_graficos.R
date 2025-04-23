library(openxlsx)
dados_brutos <- read.xlsx("tabelas_dados/dados_brutos.xlsx")

# Remover ocorrências duplicadas por espécies
library(dplyr)
# Filtrando pontos únicos de longitude e latitude para cada espécie
dados_brutos_unic <- dados_brutos %>%
  group_by(species) %>%
  distinct(lon, lat, .keep_all = TRUE) %>%
  ungroup()

# Salvar tabela com ocorrências filtradas e limpas:
library(openxlsx)
write.xlsx(dados_brutos, "dados_brutos.xlsx")

# Exploração dos dados 
library(dplyr)
library(ggplot2)
# Contagem de ocorrências por espécie
occ_sp <- dados_brutos_unic%>%
  group_by(species) %>%
  summarise(ocorrencias = n())

#Salvar:
write.xlsx(occ_sp, "tabelas_dados/occ_sp.xlsx")


# Criando um mapa
# Carregar pacotes necessários
library(ggplot2)
library(maps)

# Criar o mapa
mapa_mundial <- map_data("world") # Substitua por "usa" ou "brasil" dependendo do seu país

# Defina os limites de longitude e latitude (PARA CORTAR O MAPA)
limites_lon <- c(-140, -30)  # Defina os limites de longitude (exemplo)
limites_lat <- c(-60, 60)     # Defina os limites de latitude (exemplo)

# limite 2
limites_lon2 <- c(-95, -37)  # Defina os limites de longitude (exemplo)
limites_lat2 <- c(-38, 38)     # Defina os limites de latitude (exemplo)

#Colocar divisa dos países 
library(rnaturalearth)
library(rnaturalearthdata)
# Carregar dados dos países (bordas)
paises <- ne_countries(scale = "medium", returnclass = "sf")
# Calcular o centroide (coordenadas centrais) de cada país
paises_sf_centroids <- paises %>%
  st_centroid() %>%  # Calcula o centroide dos países
  st_coordinates() %>%  # Extrai as coordenadas do centroide
  as.data.frame()  # Converte para um data.frame
# Adiciona o nome do país e as coordenadas centrais
paises_sf_centroids <- paises %>%
  st_centroid() %>%
  bind_cols(as.data.frame(st_coordinates(.))) %>%
  select(country = name, longitude = X, latitude = Y)
# Supondo que você tenha um objeto `paises` com as coordenadas centrais (lon, lat) e os nomes dos países

# Certifique-se de que você tem o objeto `paises_sf_centroids`
# Ele deve conter as colunas `country`, `longitude`, e `latitude`

# Adicionar os nomes dos países no gráfico
ggplot() +
  geom_polygon(data = mapa_mundial, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_sf(data = paises, fill = NA, color = "#263510") +  # Adicionando as bordas dos países
  geom_point(data = dados_brutos_unic, aes(x = lon, y = lat, color = species), 
             alpha = 0.2, size = 1.5, show.legend = FALSE) +  # `show.legend = FALSE` remove a legenda
  labs(title = "Distribuição Trechaleidae",
       x = "Longitude", 
       y = "Latitude") +
  scale_color_viridis_d(option = "turbo") +  # Define a paleta de cores
  theme_minimal() +  # Usando `theme_dark` para um visual mais escuro
  #coord_sf(xlim = limites_lon, ylim = limites_lat) +  # Usando coord_sf() para definir limites
  guides(shape = guide_legend(override.aes = list(size = 1.5)))


# Mapa da distribuição das espécies 
library(viridis)
ggplot() +
  geom_polygon(data = mapa_mundial, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_sf(data = paises, fill = NA, color = "#263510") +  # Adicionando as bordas dos países
  geom_point(data = dados_limpos, aes(x = lon, y = lat, color = species), 
             alpha = 0.2, size = 1.5, show.legend = F) +  # `show.legend = FALSE` remove a legenda
  labs(title = "Distribuição atual das espécies de Trechaleidae",
       x = "Longitude", 
       y = "Latitude",
       caption = "Fonte: Elaborado pelo autor.") +
  scale_color_viridis_d(option = "turbo") +  # Define a paleta de cores
  theme_dark() +  # Usando `theme_minimal` para um visual mais limpo
  coord_sf(xlim = limites_lon, ylim = limites_lat) +  # Usando coord_sf() em vez de coord_cartesian()
  guides(shape = guide_legend(override.aes = list(size = 1.5)))

##outra opção

ggplot() +
  geom_polygon(data = mapa_mundial, aes(x = long, y = lat, group = group), fill = "lightgrey") +  # Ajuste para o fundo
  geom_sf(data = paises, fill = NA, color = "#ffffff") +  # Bordas dos países em branco
  geom_point(data = dados_limpos, aes(x = lon, y = lat, color = species), 
             alpha = 0.8, size = 1, show.legend = TRUE) +  # Manter legendas visíveis
  labs(
    title = "Distribuição atual das espécies de Trechaleidae",
    x = "Longitude", 
    y = "Latitude",
    caption = "Fonte: Elaborado pelo autor."
  ) +
  scale_color_viridis_d(option = "turbo") +  # Paleta vibrante
  theme_dark(base_size = 14) +  # Estética dark, com fontes maiores
  coord_sf(xlim = limites_lon, ylim = limites_lat, expand = FALSE) +  # Aumentar escala do mapa
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Ajuste do tamanho na legenda
  theme(
    legend.position = "right",  # Posição da legenda
    legend.background = element_rect(fill = "grey", color = NA),  # Fundo escuro sem borda
    legend.text = element_text(color = "white", size = 10),  # Texto em branco
    legend.title = element_text(color = "white", size = 12),  # Título da legenda
    legend.key = element_rect(fill = "grey20", color = NA),  # Simplicidade na legenda
    panel.grid = element_line(color = "grey")  # Grade do painel discreta
  )

### Mapa dos pontos excluidos com o pacote Coordinate
excluidos_filter_Coordinate2 <- ggplot() +
geom_polygon(data = mapa_mundial, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_sf(data = paises, fill = NA, color = "#263510") +  # Adicionando as bordas dos países
  geom_point(data = exclude, aes(x = lon, y = lat, color = species), 
             alpha = 0.3, size = 2, show.legend = F) +  # `show.legend = FALSE` remove a legenda
  labs(title = "Dados excluidos pelo pacote CoordinateCleaner",
       x = "Longitude", 
       y = "Latitude") +
  scale_color_viridis_d(option = "turbo") +  # Define a paleta de cores
  theme_minimal() +  # Usando `theme_minimal` para um visual mais limpo
  coord_sf(xlim = limites_lon2, ylim = limites_lat2) +  # Usando coord_sf() em vez de coord_cartesian()
  guides(shape = guide_legend(override.aes = list(size = 1.5)))


ggsave(
  filename = "excluiidos_filter_CoordinateCleaner2.png",
  plot = excluidos_filter_Coordinate2,
  device = "png",
  scale = 2,
  bg = "white"
)


#Frequência de Ocorrências por Espécie e País
library(dplyr)
freq_sp_localidade <-dados_brutos_unic %>%
  group_by(species, country) %>%
  summarise(frequencia = n())

#Salvar
write.xlsx(freq_sp_localidade, "tabelas_dados/freq_sp_localidade.xlsx")


# Gráficos das frequências das espécies por países 
#total de ocorrências por países
library(scales)
library(ggplot2)
ggplot(freq_sp_localidade, aes(x = country, y = species, fill = frequencia)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "turbo", trans = scales::modulus_trans(p = 0.1), labels = label_number(accuracy = 1)) +  # Escala log para visualizar melhor
  labs(title = "Número de ocorrências de Trechaleidae por País",
       x = "Países",
       y = "Espécies",
       fill = "Frequência")+#,
       #caption = "Fonte: Dados retirados do GBIF") +
  theme_light() +
  theme(
    axis.text.y = element_text(size = 8),  # Tamanho do texto do eixo Y
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Tamanho do texto do eixo X
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Tamanho do título
    axis.title.x = element_text(size = 12),  # Tamanho do título do eixo X
    axis.title.y = element_text(size = 12),  # Tamanho do título do eixo Y
    legend.title = element_text(size = 12),  # Tamanho do título da legenda
    legend.text = element_text(size = 10)   # Tamanho do texto da legenda
  ) 

ggsave(
  filename = "ssp_paises10.png",
  plot = ssp_paises2,
  device = "png",
  scale = 2,
  bg = "white",width = 8, height = 5, dpi = 300
)

# Explorando funções do pacote spThin
library(spThin)
