################################################################################
###                  Projeto Aranhas da família Trechaleidae                ####
################################################################################ 

# Pacotes
library(randomcoloR)
library(ggplot2)

# 1. Exploração dos dados                 
# Mapa da distribuição das espécies 

# # Gere uma paleta com 55 cores distintas
# paleta_cores <- distinctColorPalette(30)
# print(paleta_cores)

ggplot() +
  geom_polygon(data = mapa_brasil, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_sf(data = paises, fill = NA, color = "#263510") +  # Adicionando as bordas dos países
  geom_point(data = dados_df_naremov, aes(x = lon, y = lat, color = species), 
             alpha = 0.2, size = 1, show.legend = F) +  # `show.legend = FALSE` remove a legenda
  labs(title = "Distribuição das espécies de Trechaleidae",
       x = "Longitude", 
       y = "Latitude") +
  scale_color_manual(values = c("#FF1493", "#8B008B", "#FF6347", "purple", "darkred", "#32CD32", "#DC143C", "#FFD700",
                                "#D2691E", "#B4D6EA", "#80AED2", "#C71585", "#7CFC40", "#00BFFF", "#FF8C00", "#ADFF2F",
                                "#008080", "#00008B", "#0000CD", "#FF4500", "#2E8B57", "#DAA520", "#DDA0DD", "#00CED1",
                                "#4B0082", "#4682B4", "#6A5ACD", "#1E90FF", "#9400D3", "#FF00FF", "#FF6347", "#6B8E23",
                                "#6495ED", "#ABE07C", "#E2CB7F", "#ADFF2F", "#FF1493", "#F08080", "#FF7F50", "#1C1C1C",
                                "#D8DDA8", "#0000FF", "#FF4500", "#B8860B", "#5FE390", "#228B65", "#DB7093", "#228B22",
                                "#986852", "#F4A460", "#20B2AA", "#708090", "#1f77b4", "#808000", "#9ACD32")
                     
  ) +#scale_color_viridis_d(option = "turbo") +  # Define a paleta de cores,
  theme_minimal() +  
  coord_sf(xlim = limites_lon, ylim = limites_lat) +  # Usando coord_sf() em vez de coord_cartesian()
  guides(shape = guide_legend(override.aes = list(size = 0.2)))

# Mapa 2
library(ggnewscale)

ggplot() +
  geom_polygon(data = mapa_brasil, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_sf(data = paises, fill = NA, color = "#263510") +
  geom_point(data = subset(dados_df_naremov, species %in% c("Cupiennius salei", "Cupiennius coccineus")),
             aes(x = lon, y = lat, color = species), alpha = 0.5, size = 1.5) +
  scale_color_viridis_d(option = "turbo") +
  new_scale_color() +
  geom_point(data = subset(dados_df_naremov, !species %in% c("Cupiennius salei", "Cupiennius coccineus")),
             aes(x = lon, y = lat, color = species), alpha = 0.5, size = 1.5) +
  scale_color_viridis_d(option = "magma") +
  theme_minimal() +
  coord_sf(xlim = limites_lon, ylim = limites_lat)


library(ggplot2)
library(sf)
library(dplyr)

# Calcular os centróides dos países
centroides_paises <- st_centroid(paises)
# Selecionar os países que você quer rotular (exemplo com Brasil, Argentina e Uruguai)
paises_rotulados <- centroides_paises %>%
  filter(name %in% c("Brazil", "Argentina", "Uruguay",  "Mexico", "Honduras","French Guiana",
                     "Costa Rica","Belize", "Peru", "Chile", "Venezuela","Finland","Nicaragua",
                     "Dominican Republic", "Germany","Colombia","Guatemala","El Salvador","Bolivia",
                     "Haiti","Panama","Ecuador", "United States","Paraguay","Suriname"))

# Adicionar o mapa e os nomes dos países
ggplot() +
  geom_polygon(data = mapa_brasil, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_sf(data = paises, fill = NA, color = "#263510") +  # Adicionando as bordas dos países
  geom_point(data = dados_df_naremov, aes(x = lon, y = lat, color = species), 
             alpha = 0.2, size = 1.5, check_overlap = T, show.legend = F) +
  geom_sf_text(data = paises_rotulados, aes(x = st_coordinates(geometry)[,1],
                                          y = st_coordinates(geometry)[,2],
                                          label = name),
            size = 2.3, color = "black", fontface = "bold") +  # Adicionando os nomes dos países
  labs(title = "Distribuição das espécies de Trechaleidae",
       x = "Longitude", 
       y = "Latitude") +
  scale_color_manual(values = c("#FF1493", "#8B008B", "#FF6347", "purple", "darkred", "#32CD32", "#DC143C", "#FFD700",
                                "#D2691E", "#B4D6EA", "#80AED2", "#C71585", "#7CFC40", "#00BFFF", "#FF8C00", "#ADFF2F",
                                "#008080", "#00008B", "#0000CD", "#FF4500", "#2E8B57", "#DAA520", "#DDA0DD", "#00CED1",
                                "#4B0082", "#4682B4", "#6A5ACD", "#1E90FF", "#9400D3", "#FF00FF", "#FF6347", "#6B8E23",
                                "#6495ED", "#ABE07C", "#E2CB7F", "#ADFF2F", "#FF1493", "#F08080", "#FF7F50", "#1C1C1C",
                                "#D8DDA8", "#0000FF", "#FF4500", "#B8860B", "#5FE390", "#228B65", "#DB7093", "#228B22",
                                "#986852", "#F4A460", "#20B2AA", "#708090", "#1f77b4", "#808000", "#9ACD32")
                     
  ) +
  theme_minimal() +  
  coord_sf(xlim = limites_lon, ylim = limites_lat) +
  guides(shape = guide_legend(override.aes = list(size = 0.2)))


# Gráficos das frequências das espécies por países 
# Primeiro passo:
library(dplyr)
freq_sp_localidade <-dados_df_naremov %>%
  group_by(species, country) %>%
  summarise(frequencia = n())

# Gráfico com frequência de espécies por páises 
ggplot(freq_sp_localidade, aes(x = country, y = species, fill = frequencia)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "turbo", trans = "log") +  # Escala log para visualizar melhor
  labs(title = "Frequência dos dados de ocorrência das espécies por país",
       x = "País",
       y = "Espécie") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))  # Ajuste do tamanho do texto


# Extraindo variáveis ambientais ------------------------------------------

library(raster)
library(dplyr)
# Caminhos dos arquivos climáticos
temp_media <- raster("climate/wc2.1_10m/wc2.1_10m_bio_1.tif")   
precip_media <- raster("climate/wc2.1_10m/wc2.1_10m_bio_12.tif")
altitude <- raster("HYP_HR_SR_OB_DR.tif")

# Extraia somente as colunas de longitude e latitude para a função extract
coordenadas <- dados_df_naremov[, c("lon", "lat")]

# Extrai os valores de temperatura média e precipitação média
dados_df_naremov$temperatura_media <- extract(temp_media, coordenadas)
dados_df_naremov$precipitacao_media <- extract(precip_media, coordenadas)
dados_df_naremov$altitude <- extract(altitude, coordenadas)

# Gráfico de dispersão entre altitude e precipitação média
ggplot(dados_df_naremov, aes(x = altitude, y = precipitacao_media, color = species)) +
  geom_point(alpha = 0.6, show.legend = FALSE) +
  labs(title = "Relação entre Altitude e Precipitação Média por espécies",
       x = "Altitude",
       y = "Precipitação Média (mm)")

#construa um coplot para ver possiveis efeitos de interações
coplot(tabela_resumida_2$riqueza~ tabela_resumida_2$|xdatLocal$Temp, pch=16,panel=panel.smooth, rows=1) #teste o argumento overlap=0


# Analisando a qualidade dos dados ----------------------------------------

# Aplicando o teste de Levene para variância da frequência entre países
# # Aqui o argumento `frequencia` representa a variável de interesse e `country` os grupos
# library(car)
# leveneTest(frequencia ~ as.factor(country), data = freq_sp_localidade)
# 
# # Ajusta o modelo de ANOVA
# anova_result <- aov(frequencia ~ as.factor(country), data = freq_sp_localidade)
# 
# # Exibe o resumo dos resultados da ANOVA
# summary(anova_result)
# 
# shapiro.test(residuals(anova_result))

# Histogramas para verificar a distribuição das variáveis
ggplot(dados_agrup_country, aes(x = precip_media_avg)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Precipitação Média")

ggplot(dados_agrup_country, aes(x = precip_media_max - precip_media_min)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histograma da Variação de Precipitação")

# Teste de normalidade
shapiro.test(dados_coccineus$temperatura_media)
shapiro.test(dados_agrup_country$precip_media_max - dados_agrup_country$precip_media_min)

dados_agrup_country$precip_media_avg_log <- log(dados_agrup_country$precip_media_avg + 1)  # Se tiver valores zero
dados_agrup_country$precip_media_max_min_log <- log(dados_agrup_country$precip_media_max - dados_agrup_country$precip_media_min + 1)

shapiro.test(dados_agrup_country$precip_media_avg_log)
shapiro.test(dados_agrup_country$precip_media_max_min_log)


library(fitdistrplus) #PACOTE PARA COMAPRAR DISTRIBUIÇÕES
descdist(dados_df_naremov$species ~ dados_df_naremov$precipitacao_media, discrete = T, boot = 1000)


library(fitdistrplus)

# Testar as distribuições para 'precipitacao_media'
descdist(tabela_resumida_geral$precip_media_avg, discrete = FALSE, boot = 1000)








# # Criando o modelo ajustando o numero de ocorrência
# modelo_glm_log <- glm(n_ocorrencias ~ precip_media_avg + (precip_media_max - precip_media_min), 
#                   family = poisson(), data = dados_agrup_country, 
#                   offset = log(n_ocorrencias)) 
# summary(modelo_glm)


# Modelo considerando o país como variável aleatória
library(lme4)
modelo_glmm <- glmer(n_ocorrencias ~ precip_media_avg + (precip_media_max - precip_media_min) + (1|country),
                     data = dados_agrup_country, family = poisson())
summary(modelo_glmm)
plot(residuals(modelo_glmm))
shapiro.test(residuals(modelo_glmm))
check_model(modelo_glmm)

# Modelo 2
modelo_glmm_alt <- glmer(n_ocorrencias ~ precip_media_avg + (precip_media_max - precip_media_min) + altitude_avg + (1|country),
                     data = dados_agrup_country, family = poisson())
summary(modelo_glmm_alt)
check_model(modelo_glmm_alt)
check_homogeneity(modelo_glmm_alt)

# Criando um modelo linear generalizado (GLM)
modelo_glm <- glm(n_ocorrencias ~ temp_media_avg + precip_media_avg + altitude_avg, 
                  data = dados_agrupados, 
                  family = poisson)
check_model(modelo_glm)
summary(modelo_glm)
shapiro.test(resid(glm(n_ocorrencias ~ temp_media_avg + precip_media_avg + altitude_avg, 
                       data = dados_agrupados)))
names(modelo_glm)
modelo_glm$residuals

#Gráficos
qqnorm(modelo_glm$residuals)
qqline(modelo_glm$residuals)

# Carregar pacotes necessários
library(dplyr)

# Teste Shapiro-Wilk para variáveis contínuas em dados_agrupados
shapiro_results <- dados_agrupados %>%
  summarize(
    shapiro_temp = shapiro.test(temp_media_avg)$p.value,
    shapiro_precip = shapiro.test(precip_media_avg)$p.value,
    shapiro_altitude = shapiro.test(altitude_avg)$p.value
  )

print(shapiro_results) #NÂO SÃO NORMAIS

# Shapiro para tabela considerando os píses 




# Histograma para verificar a distribuição
hist(dados_agrupados$temp_media_avg, main="Histograma de Temp Média", xlab="Temp Média")
hist(dados_agrupados$precip_media_avg, main="Histograma de Precipitação Média", xlab="Precipitação Média")
hist(dados_agrupados$altitude_avg, main="Histograma de Altitude", xlab="Altitude")

# Q-Q plot para verificar normalidade visualmente
qqnorm(dados_agrupados$temp_media_avg, main="Q-Q Plot de Temp Média")
qqline(dados_agrupados$temp_media_avg, col="red")
qqnorm(dados_agrupados$precip_media_avg, main="Q-Q Plot de Precipitação Média")
qqline(dados_agrupados$precip_media_avg, col="red")
qqnorm(dados_agrupados$altitude_avg, main="Q-Q Plot de Altitude")
qqline(dados_agrupados$altitude_avg, col="red")







