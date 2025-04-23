install.packages("stringr")
library(stringr)

# Criar uma coluna separando gêneros e espécies
library(dplyr)
tabela1 <- dados_limpos %>%
  mutate(genus = word(species, 1))  # Extrai o primeiro nome (gênero)

# Filtrar dados por um gênero específico
genus_trechalea <- tabela1 %>%
  filter(genus == "Trechalea")

# Listar gêneros presentes nos dados
generos_unicos <- unique(tabela1$genus)
print(generos_unicos)


library(ggplot2)
library(viridis)

# Mapa de distribuição por gênero com cores por espécies
ggplot(data = tabela1) +
  geom_polygon(data = mapa_mundial, aes(x = long, y = lat, group = group), fill = "lightgrey", color = NA) +
  geom_point(aes(x = lon, y = lat, color = species), size = 0.8, alpha = 0.6, show.legend = F) +  # Cor por espécie
  facet_wrap(~ genus) +  # Faceta os mapas por gênero
  labs(
    title = "Distribuição das ocorrências por Gêneros",
    x = "Longitude",
    y = "Latitude",
    color = "Espécies"
  ) +
  coord_sf(xlim = limites_lon, ylim = limites_lat, expand = FALSE) +
  theme_dark() +
  theme(
    strip.text = element_text(face = "bold"),  # Negrito nos títulos dos facetas
    legend.position = "bottom"  # Posiciona a legenda abaixo
  ) +
  scale_color_manual(values = custom_colors)  # Cores personalizadas para espécies

ggsave(
  filename = "mapa_genus2.png",
  plot = mapa_genus2,
  device = "png",
  scale = 2,
  bg = "white"
)




# Outros gráficos



ggplot(freq_sp_localidade, aes(x = country, y = frequencia, fill = species)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Distribuição das espécies de Trechaleidae por País",
       x = "Países",
       y = "Frequência",
       fill = "Espécies") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# Gráfico para comparar a distribuição entre os países 
grafrico_paises <- ggplot(freq_sp_localidade, aes(x = country, y = frequencia, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Comparação do número de ocorrência de espécies entre países",
       x = "Países",
       y = "Nº de ocorrências",
       fill = "Espécies") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

####
ggplot(freq_sp_localidade, aes(x = reorder(country, -frequencia), y = frequencia, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(trans = "sqrt")+
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Comparação do número de ocorrência de espécies entre países",
       x = "Países",
       y = "Nº de ocorrências",
       fill = "Espécies") +
  theme_light() +
  theme(
    # Ajuste do eixo X
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    # Ajuste da legenda
    legend.position = "bottom",
    legend.text = element_text(size = 8),  # Texto da legenda menor
    legend.title = element_text(size = 10, face = "bold"),
    # Ajuste do título
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
## 
ggplot(freq_sp_localidade, aes(x = reorder(country, -frequencia), y = frequencia, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(trans = scales::modulus_trans(p = 0.1), labels = scales::label_number(accuracy = 1)) +  
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Comparação do número de ocorrência de espécies entre países",
       x = "Países",
       y = "Log (Nº de ocorrências)",  # Atualizando rótulo para indicar escala logarítmica
       fill = "Espécies") +
  theme_light() +
  theme(
    # Ajuste do eixo X
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    # Ajuste da legenda
    legend.position = "bottom",
    legend.text = element_text(size = 8),  # Texto da legenda menor
    legend.title = element_text(size = 10, face = "bold"),
    # Ajuste do título
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

###

grafico3 <- ggplot(freq_sp_localidade, aes(x = reorder(country, -frequencia), y = frequencia, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "turbo") +
  scale_y_continuous(
    breaks = c(seq(1, 30, by = 30), seq(50, max(freq_sp_localidade$frequencia), by = 50))  # Combina as duas sequências
  ) +
  labs(title = "Comparação do número de ocorrência de espécies entre países",
       x = "Países",
       y = "Nº de ocorrências",
       fill = "Espécies") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


ggsave(
  filename = "grafico3.png",
  plot = grafico3,
  device = "png",
  scale = 2,
  bg = "white",width = 8, height = 5, dpi = 300
)


###

ggplot(freq_sp_localidade, aes(x = country, y = species, size = frequencia, color = frequencia)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 15)) +
  scale_color_viridis_c(option = "turbo") +
  labs(title = "Frequência de Espécies de Trechaleidae por País",
       x = "Países",
       y = "Espécies",
       size = "Frequência",
       color = "Frequência") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

ggplot(freq_sp_localidade, aes(x = country, y = frequencia, fill = country)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Dispersão das Frequências por País",
       x = "Países",
       y = "Frequência") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

ggplot(freq_sp_localidade, aes(x = reorder(country, -frequencia), y = frequencia, fill = species)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_viridis_d(option = "turbo", direction = -1) +
  labs(title = "Distribuição de Ocorrências de Espécies por País",
       x = "Países",
       y = "Frequência",
       fill = "Espécies") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"))



