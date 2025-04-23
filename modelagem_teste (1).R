######################################################  script criado em conjunto com o Gustavo Reis  ## ####################################################

#           MODELAGEM TESTE 1: Estudo dos resultados
## Carregamento dos pacotes:
install.packages("dismo", "raster", "dplyr", "tidyr", "usdm", "ggplot2", "mapview", "SSDM")
library(dismo)
library(raster)
library(dplyr)
library(tidyr)
library(usdm)
library(ggplot2)
library(mapview)
library(SSDM)
#####              Passo 1: Carregar variáveis ambientais

bio_raw <- raster::getData("worldclim", var = "bio", res = 5)


#####              Passo 2: Carregar ocorrências

#Baixar planilha em formato CSV:
ocorrencias_ssp <- read.csv("dadostrechaleidae/ocorrencia_sp_Trechaleidae.csv")


####   separação das ocorrencias por espécies
#1Paradossenus longipes:
Sp1longipes <- ocorrencias_ssp [ocorrencias_ssp$especie=="Paradossenus longipes",]
Sp1longipes <- Sp1longipes[,c ("longitude", "latitude")]

#2Paratrechalea azul
Sp2azul <- ocorrencias_ssp [ocorrencias_ssp$especie=="Paratrechalea azul",]
Sp2azul <- Sp2azul[,c ("longitude", "latitude")]

#3Paratrechalea ornata
Sp3ornata <- ocorrencias_ssp [ocorrencias_ssp$especie=="Paratrechalea azul",]
Sp3ornata <- Sp3ornata[,c ("longitude", "latitude")]

#4Paratrechalea galianoae
Sp4galianoae <- read.csv("dadostrechaleidae/Sp4galianoae.csv")


#####              Passo 3: Recorte das variáveis

#Shape para o Brasil:
brasil <- raster::shapefile ("./shape_brasil/BR_UF_2020.shp")
bio_raw <- raster::crop(bio_raw, brasil)
bio_raw <- raster::mask(bio_raw, brasil)
plot(bio_raw[[1]])

points(Sp1longipes, col="red", pch=5)
points(Sp2azul, col="blue", pch=3)
points(Sp3ornata, col="black", pch=16)
points(Sp4galianoae, col="black", pch=16)



#####   exportação??
raster::writeRaster(
  bio_raw,
  filename = paste0("bio_BR/", names(bio_raw)),
  format = "GTiff",
  bylayer = TRUE,
  overwrite = TRUE
)


#Passo 4: Analise de multicolinearidades considerando apenas o Brasil

#Sp1longipes:
bio_val_sp1 <- raster::extract(bio_sp1, sp1)
bio_vif_sp1longipes <- usdm::vifstep(bio_val_sp1longipes)
bio_vif_sp1longipes
bio_sp1longipes <- usdm::exclude(bio_raw, bio_vif_sp1longipes)
plot(bio_sp1longipes)

#Sp2azul:
bio_val_sp2azul <- raster::extract(bio_raw, Sp2azul)
bio_vif_sp2azul <- usdm::vifstep(bio_val_sp2azul)
bio_vif_sp2azul
bio_sp2azul <- usdm::exclude(bio_raw, bio_vif_sp2azul)
plot(bio_sp2azul)

#Sp3ornata
bio_val_sp3ornata <- raster::extract(bio_raw, Sp3ornata)
bio_vif_sp3ornata <- usdm::vifstep(bio_val_sp3ornata)
bio_vif_sp3ornata
bio_sp3ornata <- usdm::exclude(bio_raw, bio_vif_sp3ornata)
plot(bio_sp3ornata)


#Sp4galianoae
bio_val_sp4galianoae <- raster::extract(bio_raw, Sp4galianoae)
bio_vif_sp4galianoae <- usdm::vifstep(bio_val_sp4galianoae)
bio_vif_sp4galianoae
bio_sp4galianoae <- usdm::exclude(bio_raw, bio_vif_sp4galianoae)
plot(bio_sp4galianoae)

######Criação de paleta de cores para o mapa

color <- colorRampPalette(c("#3E49BB",
                            "#3498DB",
                            "green4",
                            "yellow",
                            "orange",
                            "red",
                            "darkred"))

color2 <- colorRampPalette(c( "#3E49BB",
                              "#3498DB",
                              "yellow3",
                             "orange",
                            "tomato",
                            "darkred"))

color3 <- colorRampPalette(c( "green4",
                              "yellow3",
                              "orange",
                              "red",
                              "darkred"))



######                  Passo 5: Modelagem Brasil

#modelo Paradossenus longipes

modelo_longipes <- modelling("GLM", Occurrences = Sp1longipes, 
                          Env = bio_sp1longipes, Xcol = "longitude", Ycol = "latitude", verbose = TRUE)

modelo_longipes@evaluation
 
mapa_mod.longipes <- plot(modelo_longipes@projection, col=color(200))
gui(mapa_mod.longipes)

##:::tentativa de modificar os mapas colocando legendas e tudo mais::::
mapa_mod.longipes <- legend(xlab = "Longitude", ylab = "Latitude")


ensemble_BR_longipes <- ensemble_modelling(c("GLM", "GAM", "RF"), Occurrences = Sp1longipes,
                                  Env = bio_sp1longipes, rep = 5, Xcol = "longitude", Ycol = "latitude",
                                  ensemble.thresh = 0.75, verbose = TRUE)

plot(ensemble_BR_longipes@projection, col= color2(200))

ensemble_BR_longipes <- plot(ensemble_BR_longipes@projection, col= color2(200))
gui(ensemble_BR_longipes)

#modelo Paratrechalea azul
modelo_azul <- modelling("GLM", Occurrences = Sp2azul, 
                             Env = bio_sp2azul, Xcol = "longitude", Ycol = "latitude", verbose = TRUE)
modelo_azul
modelo_azul@evaluation

plot(modelo_azul@projection, col=color(200))

ensemble_BR_azul <- ensemble_modelling(c("GLM", "GAM", "RF"), Occurrences = Sp2azul,
                                           Env = bio_sp2azul, rep = 5, Xcol = "longitude", Ycol = "latitude",
                                           ensemble.thresh = 0.75, verbose = TRUE)
plot(ensemble_BR_azul@projection, col= color(200))

#modelo Paratrechalea ornata
modelo_ornata <- modelling("GLM", Occurrences = Sp3ornata, 
                         Env = bio_sp3ornata, Xcol = "longitude", Ycol = "latitude", verbose = TRUE)

modelo_ornata@evaluation

plot(modelo_ornata@projection, col=color(200))

ensemble_BR_ornata <- ensemble_modelling(c("GLM", "GAM", "RF"), Occurrences = Sp3ornata,
                                       Env = bio_sp3ornata, rep = 5, Xcol = "longitude", Ycol = "latitude",
                                       ensemble.thresh = 0.75, verbose = TRUE)
plot(ensemble_BR_ornata@projection, col= color(200))

#modelo Paratrechalea galianoae
modelo_galianoae <- modelling("GLM", Occurrences = Sp4galianoae, 
                           Env = bio_sp4galianoae, Xcol = "longitude", Ycol = "latitude", verbose = TRUE)

modelo_galianoae@evaluation

plot(modelo_galianoae@projection, col=color(200))

ensemble_BR_galianoae <- ensemble_modelling(c("GLM", "GAM", "RF"), Occurrences = Sp4galianoae,
                                         Env = bio_sp4galianoae, rep = 5, Xcol = "longitude", Ycol = "latitude",
                                         ensemble.thresh = 0.75, verbose = TRUE)
plot(ensemble_BR_galianoae@projection, col= color(200))



#####           Passo 6: Modelagem Rio Grande do Sul

#Shapefile do RS:

#Shape para o estado do Rio Grande do Sul:
RS <- raster::shapefile ("./shape_RS/estado_rs_4326_20081217_1506.shp")
bio_rs <- raster::crop(bio_sp1longipes, RS)
bio_rs <- raster::mask(bio_rs, RS)
plot(bio_rs[[1]])

#modelo RS Paradossenus longipes:
modelo_RS_longipes <- modelling("GLM", Occurrences = Sp1longipes, 
                          Env = bio_rs, Xcol = "longitude", Ycol = "latitude", verbose = TRUE)

modelo_RS_longipes@evaluation
plot(modelo_RS_longipes@projection, col=color(200))
points(Sp1longipes, pch=3)

ensemble_RS_longipes <- ensemble_modelling(c("GLM", "GAM", "RF"), Occurrences = Sp1longipes,
                                     Env = bio_rs, rep = 5, Xcol = "longitude", Ycol = "latitude",
                                     ensemble.thresh = 0.75, verbose = TRUE)
plot(ensemble_RS_longipes@projection, col=color(200))

ensemble_RS_longipes@algorithm.evaluation

#modelo RS Paratrechalea azul
modelo_RS_azul <- modelling("GLM", Occurrences = Sp2azul, 
                                Env = bio_rs, Xcol = "longitude", Ycol = "latitude", verbose = TRUE)

modelo_RS_azul@evaluation
plot(modelo_RS_azul@projection, col=color(200))
points(Sp2azul, pch=3)

ensemble_RS_azul <- ensemble_modelling(c("GLM", "GAM", "RF"), Occurrences = Sp2azul,
                                           Env = bio_rs, rep = 5, Xcol = "longitude", Ycol = "latitude",
                                           ensemble.thresh = 0.75, verbose = TRUE)
plot(ensemble_RS_azul@projection, col=color(200))
points(Sp2azul, pch=10)
ensemble_RS_azul@algorithm.evaluation

#modelo RS Paratrechalea ornata
modelo_RS_ornata <- modelling("GLM", Occurrences = Sp3ornata, 
                            Env = bio_rs, Xcol = "longitude", Ycol = "latitude", verbose = TRUE)

modelo_RS_ornata@evaluation
modelo_RS_ornata@variable.importance
plot(modelo_RS_ornata@projection, col=color(200))
points(Sp3ornata, pch=2)

ensemble_RS_ornata <- ensemble_modelling(c("GLM", "GAM", "RF"), Occurrences = Sp3ornata,
                                       Env = bio_rs, rep = 5, Xcol = "longitude", Ycol = "latitude",
                                       ensemble.thresh = 0.75, verbose = TRUE)

plot(ensemble_RS_ornata@projection, col=color(200))
points(Sp3ornata, pch=10)
ensemble_RS_ornata@algorithm.evaluation

#modelo RS P.galianoae
modelo_RS_galianoae <- modelling("GLM", Occurrences = Sp4galianoae, 
                              Env = bio_rs, Xcol = "longitude", Ycol = "latitude", verbose = TRUE)

modelo_RS_galianoae@evaluation
modelo_RS_galianoae@variable.importance
plot(modelo_RS_galianoae@projection, col=color(200))
points(Sp4galianoae, pch=2)

ensemble_RS_galianoae <- ensemble_modelling(c("GLM", "GAM", "RF"), Occurrences = Sp4galianoae,
                                         Env = bio_rs, rep = 5, Xcol = "longitude", Ycol = "latitude",
                                         ensemble.thresh = 0.75, verbose = TRUE)

plot(ensemble_RS_galianoae@projection, col=color(200))
points(Sp4galianoae, pch=10)#as coordenadas estão erradas?
ensemble_RS_galianoae@algorithm.evaluation




############################################################################             PROBLEMA COM JAVA E MAXENT                    ###### #######################################################################


Sys.setenv(JAVA_HOME="C:/Users/Usuario/AppData/LocalLow/Oracle/Java/jre1.8.0_361_x64")

Sys.setenv(JAVA_HOME="C:/Users/Usuario/AppData/Local/R/win-library/4.3/rJava/libs/x64/rJava.dll")

library(rJava)
install.packages("rJava")


