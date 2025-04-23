# Exemplo de contagem das ocorrências por espécie
library(dplyr)
library(ggplot2)


# Contagem de ocorrências por espécie
dados_comNA <- dados_df%>%
  group_by(species) %>%
  summarise(ocorrencias = n())
# Criar uma nova coluna 'categoria' com base no número de ocorrências
dados_ocorrencias <- dados_ocorrencias %>%
  mutate(categoria = case_when(
    ocorrencias >= 1 & ocorrencias <= 100 ~ "pouco amostrada",
    ocorrencias >= 101 ~ "frequentemente amostrada"
  ))
# Visualizar o resultado
print(dados_ocorrencias)

# Criar o gráfico de barras com cores personalizadas
ggplot(dados_ocorrencias, aes(x = categoria, fill = categoria)) +
  geom_bar() +
  labs(title = "Distribuição das Ocorrências por Categoria",
       x = "Categoria de Ocorrência",
       y = "Número de Espécies") +
  scale_fill_manual(values = c("pouco amostrada" = "tomato", 
                               #"frequentemente amostrada" = "green4skyblue", 
                               "frequentemente amostrada" = "green3")) +  # Defina suas cores aqui
  theme_classic()

# # Criar o gráfico de pizza
# ggplot(dados_ocorrencias, aes(x = "", fill = categoria)) +
#   geom_bar(width = 1) +  # Barras empilhadas para o gráfico de pizza
#   coord_polar(theta = "y") +  # Transformar em gráfico de pizza
#   labs(title = "Distribuição das Ocorrências por Categoria") +
#   theme_void()  # Remover eixos e fundo
# # Criar o gráfico de pizza com cores personalizadas
# ggplot(dados_ocorrencias, aes(x = "", fill = categoria)) +
#   geom_bar(width = 1) +
#   coord_polar(theta = "y") +
#   labs(title = "Distribuição das Ocorrências por Categoria") +
#   scale_fill_manual(values = c("pouco amostrada" = "orange",
#                                "frequentemente amostrada" = "purple",
#                                "moderadamente amostrada" = "cyan")) +  # Defina suas cores aqui
#   theme_void()


# Supondo que seu dataframe se chama dados_ocorrencias
frequencia_especies <- dados_df_naremov %>%
  group_by(species) %>%
  summarise(frequencia = n())#frequencia==ocorrencia

# Visualizar as frequências
print(frequencia_especies)
# Cálculo da frequência por categoria
frequencia_categorias <- dados_ocorrencias %>%
  group_by(categoria) %>%
  summarise(frequencia = n())

# Visualizar as frequências
print(frequencia_categorias)


# Carregar pacotes necessários
library(ggplot2)
library(maps)
cores_categoria <- c("pouco amostrada" = "tomato3", 
                     #"frequentemente amostrada" = "skyblue3", 
                     "frequentemente amostrada" = "green3")
# Transformando o data.freme em SpatialPointsDataFrame
library(sp)

# Converte os dados em coordenadas espaciais
coordinates(dados_df_naremov) <- ~lon+lat
proj4string(dados_df_naremov) <- CRS("+init=epsg:4326")



# Criar o mapa
mapa_mundial <- map_data("world") # Substitua por "usa" ou "brasil" dependendo do seu país

# Converter mapa_brasil para SpatialPolygonsDataFrame:
library(broom)
mapa_brasil_df <- fortify(mapa_brasil)
colnames(mapa_brasil)


# Defina os limites de longitude e latitude (CORTAR O MAPA)
limites_lon <- c(-140, -30)  # Defina os limites de longitude (exemplo)
limites_lat <- c(-60, 60)     # Defina os limites de latitude (exemplo)


#Colocar divisa dos países 
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Carregar dados dos países (bordas)
paises <- ne_countries(scale = "medium", returnclass = "sf")

# Plote o mapa com as bordas dos países
#Costumizando o mapa:

# Carregar o pacote de cores desejado
library(viridis)
library(ggplot2)
# Escolher a paleta de cores para as espécies
cores_paleta <- brewer.pal(n = length(unique(dados_df_naremov$species)), "Set3")  # Ajuste o "Set3" ou escolha uma das paletas disponíveis
# Mapa da distribuição das espécies 
ggplot() +
  geom_polygon(data = mapa_brasil, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_sf(data = paises, fill = NA, color = "#263510") +  # Adicionando as bordas dos países
  geom_point(data = dados_df_naremov, aes(x = lon, y = lat, color = species), 
             alpha = 0.2, size = 0.9, show.legend = T) +  # `show.legend = FALSE` remove a legenda
  labs(title = "Distribuição das Ocorrências de Trechaleidae",
       x = "Longitude", 
       y = "Latitude") +
  scale_color_viridis_d(option = "turbo") +  # Define a paleta de cores
  theme_minimal() +  # Usando `theme_minimal` para um visual mais limpo
  coord_sf(xlim = limites_lon, ylim = limites_lat) +  # Usando coord_sf() em vez de coord_cartesian()
  guides(shape = guide_legend(override.aes = list(size = 1.5)))


# Caminhos dos arquivos climáticos
library(raster)
library(dplyr)

temp_media <- raster("climate/wc2.1_10m/wc2.1_10m_bio_1.tif")   
precip_media <- raster("climate/wc2.1_10m/wc2.1_10m_bio_12.tif")
altitude <- raster("HYP_HR_SR_OB_DR.tif")

# Extraia somente as colunas de longitude e latitude para a função extract
coordenadas <- dados_df_naremov[, c("lon", "lat")]
# Extrai os valores de temperatura média e precipitação média
dados_df_naremov$temperatura_media <- extract(temp_media, coordenadas)
dados_df_naremov$precipitacao_media <- extract(precip_media, coordenadas)
dados_df_naremov$altitude <- extract(altitude, coordenadas)


# Certifique-se de que longitude e latitude são vetores numéricos
coordenadas <- dados_df_naremov[, c("lon", "lat")]

# Extraia os valores dos rasters para os pontos de ocorrência
dados_df_naremov$temperatura_media <- raster::extract(temp_media, coordenadas)
dados_df_naremov$precipitacao_media <- raster::extract(precip_media, coordenadas)
dados_df_naremov$altitude <- raster::extract(altitude, coordenadas)

# Calcular médias, medianas, mínimos e máximos para cada espécie
dados_agrupados <- dados_df_naremov %>%
  group_by(species) %>%
  summarize(
    n_ocorrencias = n(),
    temp_media_avg = mean(temperatura_media, na.rm = TRUE),
    precip_media_avg = mean(precipitacao_media, na.rm = TRUE),
    altitude_avg = mean(altitude, na.rm = TRUE),
    temp_media_min = min(temperatura_media, na.rm = TRUE),
    temp_media_max = max(temperatura_media, na.rm = TRUE),
    precip_media_min = min(precipitacao_media, na.rm = TRUE),
    precip_media_max = max(precipitacao_media, na.rm = TRUE),
    altitude_min = min(altitude, na.rm = TRUE),
    altitude_max = max(altitude, na.rm = TRUE)
  )

#Dados agrupados por espécies e países 
## Agregar os dados por species e country
dados_agrup_country <- dados_df_naremov %>%
  group_by(species, country) %>%
  summarize(
    n_ocorrencias = n(),
    temp_media_avg = mean(temperatura_media, na.rm = TRUE),
    precip_media_avg = mean(precipitacao_media, na.rm = TRUE),
    altitude_avg = mean(altitude, na.rm = TRUE),
    temp_media_min = min(temperatura_media, na.rm = TRUE),
    temp_media_max = max(temperatura_media, na.rm = TRUE),
    precip_media_min = min(precipitacao_media, na.rm = TRUE),
    precip_media_max = max(precipitacao_media, na.rm = TRUE),
    altitude_min = min(altitude, na.rm = TRUE),
    altitude_max = max(altitude, na.rm = TRUE)
  )


#Gráfico da relação dpor páises
ggplot(dados_agrup_country, aes(x = precip_media_avg, y = n_ocorrencias, color = country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  labs(title = "Relação entre Precipitação Média e Número de Ocorrências por País",
       x = "Precipitação Média",
       y = "Número de Ocorrências") +
  theme_minimal()

ggplot(dados_agrup_country, aes(x = precip_media_max - precip_media_min, y = n_ocorrencias, color = country)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Relação entre Variação de Precipitação e Número de Ocorrências por País",
       x = "Variação de Precipitação (Máxima - Mínima)",
       y = "Número de Ocorrências") +
  theme_minimal()


ggplot(dados_agrup_country, aes(x = precip_media_max - precip_media_min, y = n_ocorrencias)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~country) +
  labs(title = "Relação entre a variação da precipitação  e número de ocorrências por País",
       x = "Precipitação Média",
       y = "Número de Ocorrências") +
  theme_minimal()

cor(dados_agrup_country$n_ocorrencias, dados_agrup_country$precip_media_avg, use = "complete.obs")
cor(dados_agrup_country$n_ocorrencias, dados_agrup_country$precip_media_max - dados_agrup_country$precip_media_min, use = "complete.obs")

ggplot(dados_agrup_country, aes(x = country, y = n_ocorrencias)) +
  geom_boxplot() +
  labs(title = "Distribuição do número de ocorrências por país", x = "País", y = "Número de Ocorrências")


# Converte o SpatialPointsDataFrame em data.frame, se necessário
dados_df_naremov <- as.data.frame(dados_df_naremov)


#Frequência de Ocorrências por Espécie e País
library(dplyr)
freq_sp_localidade <-occ_Trechaleidae %>%
  group_by(species, country) %>%
  summarise(frequencia = n())
# Gráficos das frequências das espécies por países 
#total de ocorrências por países
ggplot(freq_sp_localidade, aes(x = country, y = species, fill = frequencia)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "turbo", trans = "log") +  # Escala log para visualizar melhor
  labs(title = "Frequência de Ocorrência de Espécies por País",
       x = "País",
       y = "Espécie") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))  # Ajuste do tamanho do texto



# # Opção 2
# ggplot(freq_sp_localidade, aes(x = country, y = species, size = frequencia, color = frequencia)) +
#   geom_point(alpha = 0.7) +
#   scale_color_viridis_c(option = "inferno", trans = "log") +
#   labs(title = "Intensidade de Ocorrência de Espécies por País",
#        x = "País",
#        y = "Espécie",
#        size = "Frequência") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 6))


# #Opção 3
# ggplot(freq_sp_localidade, aes(x = country, y = frequencia, fill = species)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Frequência de Ocorrência por País e Espécie",
#        x = "País",
#        y = "Frequência de Ocorrências") +
#   scale_fill_viridis_d(option = "turbo") +
#   theme_minimal()
library(ggplot2)
# Distribuição da altitude por espécie
ggplot(dados_df_naremov, aes(x = precipitacao_media, fill = species)) +
  geom_density(alpha = 0.6,show.legend = FALSE) +
  labs(title = "Distribuição da altitude por espécies")


# Distribuição de Temperatura e Precipitação por espécie
ggplot(dados_df_naremov, aes(x = temperatura_media, fill = species)) +
  geom_density(alpha = 0.6,show.legend = FALSE) +
  labs(title = "Distribuição de Temperatura Média da espécies")

# Filtrando o dataframe para uma espécie específica
dados_frequentemente_amostrados <- dados_df_naremov %>% 
  filter(species %in% c("Cupiennius coccineus F.O.Pickard-Cambridge, 1901",
                        "Cupiennius bimaculatus (Taczanowski, 1874)",
                        "Cupiennius getazi Simon, 1891",
                        "Cupiennius salei (Keyserling, 1877)"))

ggplot(dados_frequentemente_amostrados, aes(x = altitude, fill = species)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribuição de Temperatura Média para Cupiennius salei")

# Gráfico de dispersão entre altitude e temperatura média
ggplot(dados_df_naremov, aes(x = altitude, y = temperatura_media, color = species)) +
  geom_point(alpha = 0.6, show.legend = FALSE) +
  labs(title = "Relação entre Altitude e Temperatura Média por espécies")

# Gráfico de boxplot entre altitude e temperatura média
ggplot(dados_frequentemente_amostrados, aes(x = altitude, y = temperatura_media, color = species)) +
  geom_boxplot() +
  labs(title = "Relação entre Altitude e Temperatura Média para Espécies")
# Gráfico de boxplot entre altitude e precpitação média
ggplot(dados_frequentemente_amostrados, aes(x = altitude, y = precipitacao_media, color = species)) +
  geom_boxplot() +
  labs(title = "Relação entre Altitude e Precipitação Média para Espécies")



ggplot(dados_frequentemente_amostrados, aes(x = precipitacao_media, fill = species)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribuição de Precipitação Média por Categoria")

plot(temperatura_media~precipitacao_media, data = dados_df_naremov)

# Calcule estatísticas descritivas dentro de cada categoria (NÃO DEU CERTO-REPENSAR MELHOR ESSAS RELAÇÕES)
# summary_stats_variation <- dados_df %>%
#   group_by(species) %>%
#   summarise(
#     temperatura_media = mean(temperatura_media, na.rm = TRUE),
#     temperatura_sd = sd(temperatura_media, na.rm = TRUE),
#     temperatura_min = min(temperatura_media, na.rm = TRUE),
#     temperatura_max = max(temperatura_media, na.rm = TRUE),
#     
#     precipitacao_media = mean(precipitacao_media, na.rm = TRUE),
#     precipitacao_sd = sd(precipitacao_media, na.rm = TRUE),
#     precipitacao_min = min(precipitacao_media, na.rm = TRUE),
#     precipitacao_max = max(precipitacao_media, na.rm = TRUE),
#     
#     ocorrencias_total = sum(ocorrencias),
#     especies_distintas = n_distinct(species)
#   )

# Boxplot com as médias climáticas para cada categoria --------------------
#           === Temperatura ===


# Boxplot para Temperatura Média por Categoria
ggplot(dados_df, aes(x = species, y = temperatura_media, fill = categoria)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Variação da Temperatura Média por Categoria",
       x = "Categoria",
       y = "Temperatura Média (°C)") +
  theme_minimal()

#=====   Espécies freqrentemente amostradas   =======

# Criando o boxplot para a categoria filtrada
# Criando o boxplot para as espécies dentro da categoria "frequentemente amostrada"
ggplot(dados_frequentemente_amostrados, aes(x = species, y = temperatura_media)) +
  geom_boxplot(aes(fill = species), alpha = 0.7, show.legend = TRUE) +  # Removerá os rótulos do eixo x
  labs(title = "Variação da Temperatura Média por Espécie - Frequentemente Amostrada",
       x = "Espécies",  
       y = "Temperatura Média (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove os nomes das espécies do eixo x
        axis.ticks.x = element_blank()) +  # Remove as marcas do eixo x
  guides(fill = guide_legend(title = "Espécies"))  # Personaliza a legenda, se necessário

# Precipitação
ggplot(dados_frequentemente_amostrados, aes(x = species, y = precipitacao_media)) +
  geom_boxplot(aes(fill = species), alpha = 0.7, show.legend = TRUE) +  # Removerá os rótulos do eixo x
  labs(title = "Variação da Temperatura Média por Espécie - Frequentemente Amostrada",
       x = "Espécies",  
       y = "Temperatura Média (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove os nomes das espécies do eixo x
        axis.ticks.x = element_blank()) +  # Remove as marcas do eixo x
  guides(fill = guide_legend(title = "Espécies"))  # Personaliza a legenda, se necessário





# #=========   Espécies moderadamente amostradas   ================
# 
# # Filtrando os dados para a categoria "pouco amostradas"
# dados_moderadamente_amostrados <- dados_df %>%
#   filter(categoria == "moderadamente amostrada")
# 
# # Criando o boxplot para a categoria filtrada
# # Criando o boxplot para as espécies dentro da categoria "frequentemente amostrada"
# ggplot(dados_moderadamente_amostrados, aes(x = species, y = temperatura_media)) +
#   geom_boxplot(aes(fill = species), alpha = 0.7, show.legend = TRUE) +  # Removerá os rótulos do eixo x
#   labs(title = "Variação da Temperatura Média por Espécie - Moderadamente Amostrada",
#        x = "Espécies",  
#        y = "Temperatura Média (°C)") +
#   theme_minimal() +
#   theme(axis.text.x = element_blank(),  # Remove os nomes das espécies do eixo x
#         axis.ticks.x = element_blank()) +  # Remove as marcas do eixo x
#   guides(fill = guide_legend(title = "Espécies"))  # Personaliza a legenda, se necessário
# 


#=========   Espécies pouco amostradas   ================
# Filtrando os dados para a categoria "pouco amostradas"
dados_pouco_amostrados <- dados_df %>%
  filter(categoria == "pouco amostrada")

# Criando o boxplot para a categoria filtrada
# Criando o boxplot para as espécies dentro da categoria "frequentemente amostrada"
ggplot(dados_unicos, aes(x = species, y = temperatura_media)) +
  geom_boxplot(aes(fill = species), alpha = 0.7, show.legend = TRUE) +  # Removerá os rótulos do eixo x
  labs(title = "Variação da Temperatura Média por Espécie - Frequentemente Amostrada",
       x = "Espécies",  
       y = "Temperatura Média (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove os nomes das espécies do eixo x
        axis.ticks.x = element_blank()) +  # Remove as marcas do eixo x
  guides(fill = guide_legend(title = "Espécies"))  # Personaliza a legenda, se necessário


#           === Precipitação ===

# Boxplot para Precipitação Média por Categoria
ggplot(dados_unicos, aes(x = species, y = precipitacao_media)) +
  geom_boxplot(aes(fill = species), alpha = 0.7, show.legend = TRUE) +  # Removerá os rótulos do eixo x
  labs(title = "Variação da Temperatura Média por Espécie - Frequentemente Amostrada",
       x = "Espécies",  
       y = "Precipitação") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove os nomes das espécies do eixo x
        axis.ticks.x = element_blank()) +  # Remove as marcas do eixo x
  guides(fill = guide_legend(title = "Espécies"))  # Personaliza a legenda, se necessário





## Outras opções de gráfico

# # Gráfico de Violino para Temperatura Média por Categoria
# ggplot(dados_df, aes(x = categoria, y = temperatura_media, fill = categoria)) +
#   geom_violin(alpha = 0.7) +
#   labs(title = "Distribuição da Temperatura Média por Categoria",
#        x = "Categoria",
#        y = "Temperatura Média (°C)") +
#   theme_minimal()
# 
# # Gráfico de Violino para Precipitação Média por Categoria
# ggplot(dados_df, aes(x = categoria, y = precipitacao_media, fill = categoria)) +
#   geom_violin(alpha = 0.7) +
#   labs(title = "Distribuição da Precipitação Média por Categoria",
#        x = "Categoria",
#        y = "Precipitação Média (mm)") +
#   theme_minimal()

# Explorando Categorias ---------------------------------------------------
library(ggplot2)
library(dplyr)

# Criação de um resumo dos dados
resumo_dados <- dados_ocorrencias %>%
  group_by(categoria) %>%
  summarise(
    num_especies = n_distinct(species),
    total_ocorrencias = sum(ocorrencias)
  )

# Transformando o resumo_dados para formato longo
resumo_dados_long <- resumo_dados %>%
  pivot_longer(cols = c(num_especies, total_ocorrencias), 
               names_to = "tipo", values_to = "valor")

#outras opções de gráficos
ggplot(resumo_dados_long, aes(x = categoria, y = valor, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = valor), position = position_dodge(width = 0.9), 
            vjust = -0.5) +  # Ajuste a posição vertical do texto
  scale_fill_manual(values = c("red2", "skyblue2"), 
                    name = "Dados", 
                    labels = c("Número de espécies", "Número de ocorrências")) +
  labs(title = "Número de Espécies e Ocorrências por Categoria",
       x = "Categorias",
       y = "Número de ocorrências",
       caption = "Fonte: Dados retirados do GBIF",) +
    theme_grey()

# Filtrar valores únicos de coordenadas 
library(dplyr)

# Filtrar dados únicos para cada espécie com base em longitude e latitude
dados_unicos <- dados_frequentemente_amostrados %>%
  group_by(species) %>%
  distinct(lon, lat, .keep_all = TRUE) %>%
  ungroup()

# Visualizar os primeiros registros
head(dados_unicos)


library(dplyr)

# Criar tabela resumida
tabela_resumida <- dados_unicos %>%
  group_by(country) %>%
  summarise(
    abundancia = n(), # Quantidade total de ocorrências (abundância)
    riqueza = n_distinct(species), # Número de espécies únicas (riqueza)
    precip_media_avg = mean(precipitacao_media, na.rm = TRUE), # Média da precipitação
    precip_media_min = min(precipitacao_media, na.rm = TRUE), # Precipitação mínima
    precip_media_max = max(precipitacao_media, na.rm = TRUE), # Precipitação máxima
    temp_media_avg = mean(temperatura_media, na.rm = TRUE), # Média da temperatura
    temp_media_min = min(temperatura_media, na.rm = TRUE), # Temperatura mínima
    temp_media_max = max(temperatura_media, na.rm = TRUE)  # Temperatura máxima
  ) %>%
  arrange(desc(abundancia)) # Ordenar pela abundância

# Visualizar a tabela resumida
print(tabela_resumida)
summary(tabela_resumida)

tabela_resumida_geral <- dados_df_naremov %>%
  group_by(country) %>%
  summarise(
    abundancia = n(), # Quantidade total de ocorrências (abundância)
    riqueza = n_distinct(species), # Número de espécies únicas (riqueza)
    precip_media_avg = mean(precipitacao_media, na.rm = TRUE), # Média da precipitação
    precip_media_min = min(precipitacao_media, na.rm = TRUE), # Precipitação mínima
    precip_media_max = max(precipitacao_media, na.rm = TRUE), # Precipitação máxima
    temp_media_avg = mean(temperatura_media, na.rm = TRUE), # Média da temperatura
    temp_media_min = min(temperatura_media, na.rm = TRUE), # Temperatura mínima
    temp_media_max = max(temperatura_media, na.rm = TRUE)  # Temperatura máxima
  ) %>%
  arrange(desc(abundancia)) # Ordenar pela abundância
print(tabela_resumida_geral)
summary(tabela_resumida_geral)
