# Criando a tabela resumida

tabela_resumida_2 <- dados_df_naremov %>%
  group_by(country) %>%
  summarise(
    abundancia = n(), # Quantidade total de ocorrências (abundância)
    riqueza = n_distinct(species), # Número de espécies únicas (riqueza)
    precip_media_avg = mean(precipitacao_media, na.rm = TRUE), # Média da precipitação
    #precip_media_min = min(precipitacao_media, na.rm = TRUE), # Precipitação mínima
    #precip_media_max = max(precipitacao_media, na.rm = TRUE), # Precipitação máxima
    temp_media_avg = mean(temperatura_media, na.rm = TRUE), # Média da temperatura
    #temp_media_min = min(temperatura_media, na.rm = TRUE), # Temperatura mínima
    #temp_media_max = max(temperatura_media, na.rm = TRUE)  # Temperatura máxima
  ) %>%
  arrange(desc(abundancia)) # Ordenar pela abundância
str(tabela_resumida_2)
plot(tabela_resumida_2)

### Hipóteses
# Por serem aranhas semiaquaticas a precipitação irá influenciar na riqueza de espécies em comparação com a temperatura

library(dplyr)
library(ggplot2)
library(MASS)
# Análises exploratórias
# Teste de normalidade
shapiro.test(tabela_resumida_2$riqueza)


# Explorar relações entre as preditoras e a variável resposta
plot(riqueza~precip_media_avg, data= tabela_resumida_2, xlab="Precipitação média (mm)",
     ylab="Riqueza")

plot(riqueza~temp_media_avg, data= tabela_resumida_2, xlab="Temperatura média (ºC)",
     ylab="Riqueza")

# Gráfico de dispersão para abundância vs precipitação média
ggplot(tabela_resumida_2, aes(x = precip_media_avg, y = abundancia)) +
  geom_point() +
  geom_smooth(method = "glm", se = FALSE, color = "blue2") +
  labs(title = "Relação entre Precipitação Média e Abundância",
       x = "Precipitação Média",
       y = "Abundância")

# Gráfico de dispersão para riqueza vs precipitação média com representação dos páises
library(viridis)
ggplot(tabela_resumida_2, aes(x = precip_media_avg, y = riqueza, colour = country)) +
  geom_point() +
  geom_smooth(method = "glm", se = FALSE, color = "#1f78b4") +
  labs(title = "Relação entre Precipitação Média e Riqueza",
       x = "Precipitação Média",
       y = "Riqueza")+
  scale_color_manual(values = c(
    "black", "#e31a1c", "#33a02c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#fb9a99",
    "#b2df8a", "#fdbf6f", "#cab2d6", "#ffff99", "#ff33cc", "#66c2a5", "#fc8d62", "#8da0cb",
    "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3", "#1b9e77", "#d95f02", "blue3",
    "#e7298a", "#66a61e"
  ))+
  theme_dark()

# Gráfico 2 utilizando randomcoloR
library(randomcoloR)
# Gere uma paleta com X cores distintas
paleta_cores <- distinctColorPalette(26)
# print(paleta_cores)
ggplot(tabela_resumida_2, aes(x = precip_media_avg, y = riqueza, colour = country)) +
  geom_point() +
  geom_smooth(method = "glm", se = FALSE, color = "#1f78b4") +
  labs(title = "Relação entre Precipitação Média e Riqueza",
       x = "Precipitação Média",
       y = "Riqueza")+
  scale_color_manual(paleta_cores)+
  theme_dark()

# Modelo 1: Considerando a não normalidade dos dados
modelo1_negbinom <- glm.nb(riqueza ~ precip_media_avg, data = tabela_resumida_2)

#Modelo 2: Com interação
modelo2_negInt <- glm.nb(riqueza ~ precip_media_avg + temp_media_avg + 
                          precip_media_avg*temp_media_avg , data = tabela_resumida_2)

# Modelo 3: duas variáveis
modelo3_neg2var <- glm.nb(riqueza ~ precip_media_avg + temp_media_avg, data = tabela_resumida_2)

# Modelo 4: só temperatura
modelo4_negTemp <- glm.nb(riqueza ~ temp_media_avg, data = tabela_resumida_2)

# Modelo nulo: só riqueza
modelo_nulo <- glm.nb(riqueza ~ 1, data = tabela_resumida_2)

#Verificando o resíduo dos modelos
library(performance)

# Avaliação dos modelos glm.nb 
check_model(modelo_nulo)

#Residuos
check_residuals(modelo1_negbinom)
check_residuals(modelo2_negInt)
check_residuals(modelo3_neg2var)
check_residuals(modelo4_negTemp)
check_residuals(modelo_nulo)
# Outliers
check_outliers(modelo1_negbinom)
check_outliers(modelo2_negInt)
check_outliers(modelo3_neg2var)
check_outliers(modelo4_negTemp)
check_outliers(modelo_nulo)

library(DHARMa)
simulateResiduals(modelo1_negbinom, plot = T)
simulateResiduals(modelo_nulo, plot = T)
#Comparação entre todos modelos
# Escolha do modelo
# # Comparação usando AIC
AIC(modelo1_negbinom, modelo2_negInt, modelo3_neg2var, modelo4_negTemp)
AIC(modelo_negbinom, modelo_nulo)

library(car)
Anova(modelo3_neg2var)
Anova(modelo1_negbinom)
Anova(modelo_nulo)
anova(modelo_nulo, modelo1_negbinom)
check_model(modelo1_negbinom)
check_model(modelo3_neg2var)

# Gráfico Final
# Gráfico de dispersão com a linha de regressão com indice de temperatura
# Gráfico de predição para o modelo com interação e sem
# Carregar pacotes
library(sjPlot)
library(ggplot2)

#plot_model(modelo3_neg2var, type = "pred", terms = c("precip_media_avg", "temp_media_avg"))
plot_model(modelo1_negbinom, type = "pred", terms = c("precip_media_avg"), colors = "#1f78b4")
plot_model(modelo1_negbinom, type = "resid", terms = c("precip_media_avg"))

# Criar o gráfico de predição com personalização
plot_model(modelo1_negbinom, type = "pred", terms = c("precip_media_avg"),
           colors = "#1f78b4") +
  labs(title = "Predição da Riqueza de Espécies pela Precipitação Média",
       x = "Precipitação Média (mm)",
       y = "Riqueza de Espécies") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )

# Gráfico de dispersão
library(ggplot2)
ggplot(tabela_resumida_2, aes(x = precip_media_avg, y = riqueza)) + #color = temp_media_avg)) + # Se a temperatura fosse importante 
  geom_point() +
  geom_smooth(method = "glm.nb", formula = y ~ x, se = FALSE) +
  labs(title = "Relação entre Precipitação Média e Riqueza de Espécies",
       x = "Precipitação Média (mm)",
       y = "Riqueza de Espécies")

# Gráfico de dispersão com linha de ajuste personalizada
ggplot(tabela_resumida_2, aes(x = precip_media_avg, y = riqueza)) +
  geom_point(color = "red3") +
  geom_smooth(method = "glm.nb", formula = y ~ x, se = FALSE, color = "orange", size = 0.5) +
  labs(title = "Relação entre Precipitação Média e Riqueza de Espécies",
       x = "Precipitação Média (mm)",
       y = "Riqueza de Espécies")+
  theme_get()

#
 























# # Resumo do modelo
# summary(modelo_expandido)
# check_outliers(modelo_expandido)
# 
# # Gráfico de resíduos para verificar homocedasticidade
# plot(modelo_expandido$residuals ~ fitted(modelo_expandido))
# abline(h = 0, col = "red")
# 
# # Teste de normalidade dos resíduos
# shapiro.test(resid(modelo_expandido))
# 
# # Teste de homocedasticidade
# check_heteroscedasticity(modelo_expandido)
# 
# # Avaliação modelo 3: Resumo do modelo
# summary(modelo_interacao)
# check_outliers(modelo_interacao)
# 
# #Comparação entre todos modelos
# anova(modelo_simples, modelo_expandido, modelo_interacao)
# # O modelo simples se mostrou melhor, mas os dados não são normais

write.csv(dados_df_naremov, "dados_gerais.csv", row.names = FALSE)

library(openxlsx)
write.xlsx(dados_df_naremov, "dados_gerais.xlsx")



# Criando modelos:
# Modelo 1:
# modelo_simples <- lm(riqueza ~ precip_media_avg, data = tabela_resumida_geral)
# 
# # Modelo 2:
# # Ajustando o modelo linear com precipitação média e temperatura média
# modelo_expandido <- lm(riqueza ~ precip_media_avg + temp_media_avg, data = tabela_resumida_geral)
# 
# # Modelo 3:  com interação entre precipitação média e temperatura média
# modelo_interacao <- lm(riqueza ~ precip_media_avg * temp_media_avg, data = tabela_resumida_geral)







# # Definindo as três categorias de ambientes
# ambientes <- c("mata", "urbano", "aquatico")
# 
# # Definindo a probabilidade de atribuição para espécies com frequência >= 400
# set.seed(42) # Para reprodutibilidade
# frequencia_especies$habitat <- ifelse(
#   frequencia_especies$frequencia >= 400,
#   # Probabilidades ajustadas para espécies mais frequentes
#   sample(ambientes, size = nrow(frequencia_especies), replace = TRUE, prob = c(0.1, 0.3, 0.6)),
#   # Probabilidades padrão para espécies menos frequentes
#   sample(ambientes, size = nrow(frequencia_especies), replace = TRUE, prob = c(0.5, 0.4, 0.1))
# )
# 
# # Verificando a atribuição de habitat
# table(frequencia_especies$habitat)
# table(frequencia_especies$habitat, frequencia_especies$frequencia >= 400)

# 
# library(ggplot2)
# 
# ggplot(frequencia_especies, aes(x = habitat, y = frequencia, fill = habitat)) +
#   geom_boxplot() +
#   labs(title = "Distribuição da Frequência por Habitat",
#        x = "Habitat",
#        y = "Frequência de Ocorrências") +
#   theme_minimal()
# 
# 
# ggplot(frequencia_especies, aes(x = habitat, y = frequencia)) +
#   geom_jitter(aes(color = habitat), width = 0.2, size = 2) +
#   geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE, color = "black") +
#   labs(title = "Relação entre Habitat e Frequência de Ocorrências",
#        x = "Habitat",
#        y = "Frequência de Ocorrências") +
#   theme_minimal()
# 
# 
# ##Scattplot relacionando Altitude com riqueza total
# plot(frequencia_especies$frequencia, frequencia_especies$habitat, xlab= "Altitude (m)", ylab="Riqueza total no local")
# 
# # Teste de homogeneidade 
# bartlett.test(riqueza~country, data= tabela_resumida_geral) #test de barlett para a riqueza total entre niveis de protecao
# # #Teste de levenne
# library(car)
# leveneTest(frequencia ~ as.factor(habitat), data = frequencia_especies)
# #O teste de Levene sugere que a premissa de homocedasticidade é atendida, enquanto o teste de Bartlett não.
# #porém o correto é fazer com os residuos do modelo
# shapiro.test(resid(lm(frequencia ~ habitat, data = frequencia_especies)))#test de normalidade, p maior a 0.05 significa que é normal
# 
# 
# #Avalião grafica de distribuição de residuos 
# modelo_simples<-lm(frequencia~habitat, frequencia_especies)
# names(modelo_simples)
# modelo_simples$residuals
# #Gráfico qqplot
# qqnorm(modelo_simples$residuals)# comparar com distribuiÃ§Ã£o normal
# qqline(modelo_simples$residuals)#comparar ocm ditribuiÃ§Ã£o normal
# plot(modelo_simples)
# 
# # Transformando a variável frequencia em log
# frequencia_especies$log_frequencia <- log(frequencia_especies$frequencia + 1)
# modelo_lm_log <- lm(log_frequencia ~ habitat, data = frequencia_especies)
# shapiro.test(resid(modelo_lm_log))
# plot(modelo_lm_log)
# 
# # Gráfico
# # library(ggplot2)
# # ggplot(frequencia_especies, aes(x = habitat, y = frequencia, fill = habitat)) +
# #   geom_boxplot() +
# #   labs(title = "Distribuição da Frequência por Habitat",
# #        x = "Habitat",
# #        y = "Frequência de Ocorrências") +
# #   theme_minimal()
# 
# # Caminha 1 - assumir homogeneidade
# modelo_simples <- lm(frequencia ~ habitat, data = frequencia_especies)
# summary(modelo_simples)
# library(performance)
# check_model(modelo_simples)
# 
# modelo_glm <- glm(frequencia ~ habitat, family = quasipoisson(link = "log"), data = frequencia_especies)
# summary(modelo_glm)
# check_model(modelo_glm)
