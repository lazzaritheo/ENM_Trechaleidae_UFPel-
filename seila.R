
library(openxlsx)
dados_GBIF <- read.xlsx("tabelas_dados/dados_brutosGBIF.xlsx")

library(dplyr)
occ_sp_colombia <- dados_GBIF %>%
  filter(country == "Colombia") %>%  # Filtra as ocorrências apenas para a Colômbia
  group_by(species)  # Agrupa as espécies que ocorrem na Colômbia
