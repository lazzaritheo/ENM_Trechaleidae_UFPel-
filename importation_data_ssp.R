##################################################################
######                           CREATE HEADER
##################################################################                           
### Script built to obtain occurrence data (GBIF, review)

# Packages:
install.packages("rgbif")
install.packages("dismo")
library(dismo)
library(rgbif)
library(dplyr)

#  - Import data from GBIF:

# The data below refer to the Trechaleidae Family

# Genus Amapalea ----------------------------------------------------------
# Specie 1. Amapalea brasiliana (Silva & Lise, 2006)

A_brasiliana <- dismo::gbif(genus = "Amapalea", 
                                     download = TRUE, 
                                     geo = TRUE, removeZeros = TRUE)
##  *Specie without coordinates
# - Data filtering:
Barrisca <- Barrisca [ ,c("lon","lat","year", "acceptedScientificName","country")]
Barrisca<-na.omit(Barrisca)
A_brasiliana<- unique(A_brasiliana) 

# Genus Barrisca ----------------------------------------------------------

Barrisca_raw <- dismo::gbif(genus = "Barrisca",
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

Barrisca <- Barrisca_raw[,c("lon","lat","year", "acceptedScientificName","country")]
Barrisca<-na.omit(Barrisca[,c("lon","lat")])
A_brasiliana<- unique(A_brasiliana)

# Specie 1. Barrisca kochalkai (Platão, 1978)

kochalkai <- dismo::gbif(genus = "Barrisca", 
                          species = "kochalkai", 
                          download = TRUE, 
                          geo = TRUE, removeZeros = TRUE)
# - Data filtering:
kochalkai <- kochalkai[,c("lon","lat","acceptedScientificName")]
kochalkai<-na.omit(kochalkai)
kochalkai<- unique(kochalkai) 

# Specie 2. Barrisca nannella (Chamberlin e Ivie, 1936)

nannella <- dismo::gbif(genus = "Barrisca", 
                         species = "nannella*", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)
# - Data filtering:
nannella <- nannella[,c("lon","lat","acceptedScientificName")]
nannella<-na.omit(nannella)
nannella<- unique(nannella) 

# Genus Caricelea ---------------------------------------------------------
Caricelea_raw <- dismo::gbif(genus = "Caricelea", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 1. Caricelea apurimac (Silva & Lise, 2009)

apurimac <- dismo::gbif(genus = "Caricelea", 
                        species = "apurimac", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 2. Caricelea camisea (Silva & Lise, 2009)

camisea <- dismo::gbif(genus = "Caricelea", 
                        species = "camisea", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 3. Caricelea wayrapata (Silva & Lise, 2007)

wayrapata <- dismo::gbif(genus = "Caricelea", 
                        species = "wayrapata*", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Genus Cupiennius --------------------------------------------------------

Cupiennius_raw <- dismo::gbif(genus = "Cupiennius", 
                           download = TRUE, 
                           geo = TRUE, removeZeros = TRUE)


# Specie 1. Cupiennius bimaculatus 	(Taczanowski, 1874)

bimaculatus <- dismo::gbif(genus = "Cupiennius", 
                         species = "bimaculatus", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)
# - Data filtering:
bimaculatus <- bimaculatus[,c("lon","lat")]
bimaculatus<-na.omit(bimaculatus)
bimaculatus<- unique(bimaculatus)

# Specie 2. Cupiennius chiapanensis (Medina, 2006)

chiapanensis <- dismo::gbif(genus = "Cupiennius", 
                           species = "chiapanensis", 
                           download = TRUE, 
                           geo = TRUE, removeZeros = TRUE)
# - Data filtering:
chiapanensis <- chiapanensis[,c("lon","lat")]
chiapanensis<-na.omit(chiapanensis)
chiapanensis<- unique(chiapanensis)

# Specie 3. Cupiennius coccineus (F. O. Pickard-Cambridge, 1901)

coccineus <- dismo::gbif(genus = "Cupiennius", 
                            species = "coccineus", 
                            download = TRUE, 
                            geo = TRUE, removeZeros = TRUE)

# - Data filtering:
coccineus <- coccineus[,c("lon","lat")]
coccineus<-na.omit(coccineus)
coccineus<- unique(coccineus)

# Specie 4. Cupiennius cubae (Strand, 1909)

cubae <- dismo::gbif(genus = "Cupiennius", 
                         species = "cubae", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)
# - Data filtering:
C_cubae <- cubae[,c("lon","lat")]
C_cubae<-na.omit(C_cubae)
C_cubae<- unique(C_cubae)


# Specie 5. Cupiennius foliatus (F. O. Pickard-Cambridge, 1901)

foliatus <- dismo::gbif(genus = "Cupiennius", 
                     species = "foliatus", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)
# - Data filtering:
foliatus <- foliatus[,c("lon","lat")]
foliatus<-na.omit(foliatus)
foliatus<- unique(foliatus)
rm(foliatus)

# Specie 6. Cupiennius getazi (Simon, 1891)

getazi <- dismo::gbif(genus = "Cupiennius", 
                        species = "getazi*", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)
# - Data filtering:
getazi <- getazi[,c("lon","lat")]
getazi<-na.omit(getazi)
getazi<- unique(getazi)
rm(getazi)

# Specie 7. Cupiennius granadensis (Keyserling, 1877)

granadensis <- dismo::gbif(genus = "Cupiennius", 
                      species = "granadensis", 
                      download = TRUE, 
                      geo = TRUE, removeZeros = TRUE)
# - Data filtering:
granadensis <- granadensis[,c("lon","lat")]
granadensis<-na.omit(granadensis)
granadensis<- unique(granadensis)
rm(granadensis)

# Specie 8. Cupiennius oculifer (Karsch, 1879)

oculifer <- dismo::gbif(genus = "Cupiennius", 
                           species = "oculifer", 
                           download = TRUE, 
                           geo = TRUE, removeZeros = TRUE)

# Specie 9. Cupiennius remedius (Barth & Cordes, 1998)

remedius <- dismo::gbif(genus = "Cupiennius", 
                        species = "remedius", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 10. Cupiennius salei (Keyserling, 1877)

salei <- dismo::gbif(genus = "Cupiennius", 
                        species = "salei", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)
# - Data filtering:
salei <- salei[,c("lon","lat")]
salei <-na.omit(salei)
salei<- unique(salei)

# Specie 11. Cupiennius valentinei (Petrunkevitch, 1925)

valentinei <- dismo::gbif(genus = "Cupiennius", 
                        species = "valentinei", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 12. Cupiennius vodou (Brescovit & Polotow, 2005)

vodou <- dismo::gbif(genus = "Cupiennius", 
                        species = "vodou", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Genus Dossenus ----------------------------------------------------------

Dossenus_raw <- dismo::gbif(genus = "Dossenus",
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)
# Specie 1. Dossenus vodou 

guapore <- dismo::gbif(genus = "Dossenus", 
                     species = "guapore", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 2. Dossenus marginatus 

marginatus <- dismo::gbif(genus = "Dossenus", 
                     species = "marginatus*", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)
# - Data filtering:
marginatus <- marginatus[,c("lon","lat")]
marginatus<-na.omit(marginatus)
marginatus<- unique(marginatus)


# Specie 3. Dossenus paraensis 

paraensis <- dismo::gbif(genus = "Dossenus", 
                     species = "paraensis", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Genus Dyrines -----------------------------------------------------------

Dyrines_raw <- dismo::gbif(genus = "Dyrines", 
                          download = TRUE, 
                          geo = TRUE, removeZeros = TRUE)
# Specie 1. Dyrines brescoviti 

brescoviti <- dismo::gbif(genus = "Dyrines", 
                         species = "brescoviti",
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 2. Dyrines ducke 

ducke <- dismo::gbif(genus = "Dyrines",                         
                        species = "ducke", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 3. Dyrines huanuco 

huanuco <- dismo::gbif(genus = "Dyrines", 
                       species = "huanuco", 
                      download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 4. Dyrines striatipes 

striatipes <- dismo::gbif(genus = "Dyrines", 
                      species = "striatipes*", 
                     download = TRUE, 
                   geo = TRUE, removeZeros = TRUE)


# Genus Enna --------------------------------------------------------------
Enna_raw <- dismo::gbif(genus = "Enna", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 1. Enna baeza 

baeza <- dismo::gbif(genus = "Enna", 
                       species = "baeza", 
                      download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 2. Enna bartica 

bartica <- dismo::gbif(genus = "Enna", 
                        species = "bartica", 
                       download = TRUE, 
                      geo = TRUE, removeZeros = TRUE)

# Specie 3. Enna bonaldoi 

bonaldoi <- dismo::gbif(genus = "Enna", 
                        species = "bonaldoi", 
                      download = TRUE, 
                      geo = TRUE, removeZeros = TRUE)


# Specie 4. Enna caliensis 

caliensis <- dismo::gbif(genus = "Enna", 
                        species = "caliensis", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)


# Specie 5. Enna caparao 

caparao <- dismo::gbif(genus = "Enna", 
                        species = "caparao", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 6. Enna caricoi 

caricoi <- dismo::gbif(genus = "Enna", 
                        species = "caricoi", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 7. Enna carinata 

carinata <- dismo::gbif(genus = "Enna", 
                        species = "carinata", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)


# Specie 8. Enna chickeringi 

chickeringi <- dismo::gbif(genus = "Enna", 
                        species = "chickeringi", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)



# Specie 9. Enna colonche 

colonche <- dismo::gbif(genus = "Enna", 
                        species = "colonche", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)


# Specie 10. Enna eberhardi 

eberhardi <- dismo::gbif(genus = "Enna", 
                        species = "eberhardi", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)


# Specie 11. Enna echarate 

echarate <- dismo::gbif(genus = "Enna", 
                        species = "echarate", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)


# Specie 12. Enna estebanensis 

estebanensis <- dismo::gbif(genus = "Enna", 
                        species = "estebanensis", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)



# Specie 13. Enna frijoles 

frijoles <- dismo::gbif(genus = "Enna", 
                        species = "frijoles", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)


# Specie 14. Enna gloriae 

gloriae <- dismo::gbif(genus = "Enna", 
                        species = "gloriae", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 15. Enna hara 

hara <- dismo::gbif(genus = "Enna", 
                        species = "hara", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 16. Enna huanuco 

huanuco <- dismo::gbif(genus = "Enna", 
                        species = "huanuco", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 17. Enna igarape 

igarape <- dismo::gbif(genus = "Enna", 
                        species = "igarape", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 18. Enna jullieni 

jullieni <- dismo::gbif(genus = "Enna", 
                        species = "jullieni", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 19. Enna junin 

junin <- dismo::gbif(genus = "Enna", 
                        species = "junin", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 20. Enna kuyuwiniensis 

kuyuwiniensis <- dismo::gbif(genus = "Enna", 
                        species = "kuyuwiniensis", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 21. Enna maya 

maya <- dismo::gbif(genus = "Enna", 
                        species = "maya", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 22. Enna meridionalis 

meridionalis <- dismo::gbif(genus = "Enna", 
                        species = "meridionalis", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 23. Enna minor 

minor <- dismo::gbif(genus = "Enna", 
                        species = "minor", 
                        download = TRUE, 
                        geo = TRUE, removeZeros = TRUE)

# Specie 24. Enna moyobamba 

moyobamba <- dismo::gbif(genus = "Enna", 
                     species = "moyobamba", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 25. Enna nesiotes 

nesiotes <- dismo::gbif(genus = "Enna", 
                     species = "nesiotes", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 26. Enna osaensis 

osaensis <- dismo::gbif(genus = "Enna", 
                     species = "osaensis", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 27. Enna paraensis 

paraensis <- dismo::gbif(genus = "Enna", 
                     species = "paraensis", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 28. Enna pecki 

pecki <- dismo::gbif(genus = "Enna", 
                         species = "pecki", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 29. Enna redundans 

redundans <- dismo::gbif(genus = "Enna", 
                         species = "redundans", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 30. Enna rioja 

rioja <- dismo::gbif(genus = "Enna", 
                         species = "rioja", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 31. Enna riotopo 

riotopo <- dismo::gbif(genus = "Enna", 
                         species = "riotopo", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 32. Enna rothi 

rothi <- dismo::gbif(genus = "Enna", 
                         species = "rothi", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 33. Enna segredo 

segredo <- dismo::gbif(genus = "Enna", 
                     species = "segredo", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 34. Enna silvae 

silvae <- dismo::gbif(genus = "Enna", 
                     species = "silvae", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 35. Enna triste 

triste <- dismo::gbif(genus = "Enna", 
                     species = "triste", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 36. Enna trivittata 

trivittata <- dismo::gbif(genus = "Enna", 
                     species = "trivittata", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 37. Enna velox 

velox <- dismo::gbif(genus = "Enna", 
                     species = "velox*", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)

# Specie 38. Enna venezuelana 

venezuelana <- dismo::gbif(genus = "Enna", 
                     species = "venezuelana", 
                     download = TRUE, 
                     geo = TRUE, removeZeros = TRUE)
# Specie 39. Enna xingu 

xingu <- dismo::gbif(genus = "Enna", 
                           species = "xingu", 
                           download = TRUE, 
                           geo = TRUE, removeZeros = TRUE)

# Specie 40. Enna zurqui 

zurqui <- dismo::gbif(genus = "Enna", 
                           species = "zurqui", 
                           download = TRUE, 
                           geo = TRUE, removeZeros = TRUE)

# Genus Heidrunea ---------------------------------------------------------
Heidrunea_raw <- dismo::gbif(genus = "Heidrunea",
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)


# Specie 1. Heidrunea arijana 

arijana <- dismo::gbif(genus = "Heidrunea", 
                           species = "arijana", 
                           download = TRUE, 
                           geo = TRUE, removeZeros = TRUE)

# Specie 2. Heidrunea irmleri 

irmleri <- dismo::gbif(genus = "Heidrunea", 
                           species = "irmleri*", 
                           download = TRUE, 
                           geo = TRUE, removeZeros = TRUE)

# Specie 3. Heidrunea lobrita 

lobrita <- dismo::gbif(genus = "Heidrunea", 
                       species = "lobrita", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)

# Genus Hesydrus ----------------------------------------------------------

Hesydrus_raw <- dismo::gbif(genus = "Hesydrus", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 1. Hesydrus aurantius 

aurantius <- dismo::gbif(genus = "Hesydrus", 
                       species = "aurantius", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)

# Specie 2. Hesydrus canar 

canar <- dismo::gbif(genus = "Hesydrus", 
                         species = "canar", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 3. Hesydrus caripito 

caripito <- dismo::gbif(genus = "Hesydrus", 
                         species = "caripito", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 4. Hesydrus chanchamayo 

chanchamayo <- dismo::gbif(genus = "Hesydrus", 
                         species = "chanchamayo", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 5. Hesydrus habilis 

habilis <- dismo::gbif(genus = "Hesydrus", 
                         species = "habilis", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 6. Hesydrus palustris 

palustris <- dismo::gbif(genus = "Hesydrus", 
                         species = "palustris*", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 7. Hesydrus yacuiba 

yacuiba <- dismo::gbif(genus = "Hesydrus", 
                         species = "yacuiba", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Genus Neoctenus ---------------------------------------------------------

Neoctenus_raw <- dismo::gbif(genus = "Neoctenus",
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)


# Specie 1. Neoctenus comosus 

comosus <- dismo::gbif(genus = "Neoctenus", 
                       species = "comosus*", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)
# Specie 2. Neoctenus eximius 

eximius <- dismo::gbif(genus = "Neoctenus", 
                       species = "eximius", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)

# Specie 3. Neoctenus finneganae 

finneganae <- dismo::gbif(genus = "Neoctenus", 
                       species = "finneganae", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)

# Specie 4. Neoctenus peruvianus 

peruvianus <- dismo::gbif(genus = "Neoctenus", 
                       species = "peruvianus", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)

# Genus Paradossenus ------------------------------------------------------

Paradossenus_raw <- dismo::gbif(genus = "Paradossenus", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)

# Specie 1. Paradossenus acanthocymbium 

acanthocymbium <- dismo::gbif(genus = "Paradossenus", 
                          species = "acanthocymbium", 
                          download = TRUE, 
                          geo = TRUE, removeZeros = TRUE)

# Specie 2. Paradossenus benicito 

benicito <- dismo::gbif(genus = "Paradossenus", 
                              species = "benicito", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)

# Specie 3. Paradossenus caricoi 

caricoi <- dismo::gbif(genus = "Paradossenus", 
                              species = "caricoi", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)

# Specie 4. Paradossenus corumba 

corumba <- dismo::gbif(genus = "Paradossenus", 
                              species = "corumba", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 5. Paradossenus isthmus 

isthmus <- dismo::gbif(genus = "Paradossenus", 
                              species = "isthmus", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)

# Specie 6. Paradossenus longipes 

longipes <- dismo::gbif(genus = "Paradossenus", 
                              species = "longipes*", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 7. Paradossenus makuxi 

makuxi <- dismo::gbif(genus = "Paradossenus", 
                              species = "makuxi", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 8. Paradossenus minimus 

minimus <- dismo::gbif(genus = "Paradossenus", 
                              species = "minimus", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 9. Paradossenus pozo 

pozo <- dismo::gbif(genus = "Paradossenus", 
                              species = "pozo", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 10. Paradossenus pulcher 

pulcher <- dismo::gbif(genus = "Paradossenus", 
                              species = "pulcher", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 11. Paradossenus sabana 

sabana <- dismo::gbif(genus = "Paradossenus", 
                              species = "sabana", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)

# Specie 12. Paradossenus santaremensis 

santaremensis <- dismo::gbif(genus = "Paradossenus", 
                              species = "santaremensis", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 13. Paradossenus tocantins 

tocantins <- dismo::gbif(genus = "Paradossenus", 
                              species = "tocantins", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)


# Genus Paratrechalea -----------------------------------------------------

Paratrechalea_raw <- dismo::gbif(genus = "Paratrechalea", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)
# Specie 1. Paratrechalea azul 

azul <- dismo::gbif(genus = "Paratrechalea", 
                              species = "azul", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)

# Specie 2. Paratrechalea galianoae 

galianoae <- dismo::gbif(genus = "Paratrechalea", 
                              species = "galianoae", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 3. Paratrechalea julyae 

julyae <- dismo::gbif(genus = "Paratrechalea", 
                              species = "julyae", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 4. Paratrechalea longigaster 

longigaster <- dismo::gbif(genus = "Paratrechalea", 
                              species = "longigaster", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 5. Paratrechalea murphyi 

murphyi <- dismo::gbif(genus = "Paratrechalea", 
                              species = "murphyi", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 6. Paratrechalea ornata 

ornata <- dismo::gbif(genus = "Paratrechalea", 
                              species = "ornata*", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 7. Paratrechalea saopaulo 

saopaulo <- dismo::gbif(genus = "Paratrechalea", 
                              species = "saopaulo", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)
# Specie 8. Paratrechalea wygodzinskyi 

wygodzinskyi <- dismo::gbif(genus = "Paratrechalea", 
                              species = "wygodzinskyi", 
                              download = TRUE, 
                              geo = TRUE, removeZeros = TRUE)

# Genus Rhoicinus ---------------------------------------------------------
Rhoicinus_raw <- dismo::gbif(genus = "Rhoicinus", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)
# Sp 1. Rhoicinus andinus 

andinus <- dismo::gbif(genus = "Rhoicinus", 
                            species = "andinus", 
                            download = TRUE, 
                            geo = TRUE, removeZeros = TRUE)

# Specie 2. Rhoicinus fuscus 

fuscus <- dismo::gbif(genus = "Rhoicinus", 
                       species = "fuscus", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)

# Specie 3. Rhoicinus gaujoni 

gaujoni <- dismo::gbif(genus = "Rhoicinus", 
                       species = "gaujoni*", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)
# Specie 4. Rhoicinus lugato 

lugato <- dismo::gbif(genus = "Rhoicinus", 
                       species = "lugato", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)
# Specie 5. Rhoicinus rothi 

rothi <- dismo::gbif(genus = "Rhoicinus", 
                       species = "rothi", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)
# Specie 6. Rhoicinus schlingeri 

schlingeri <- dismo::gbif(genus = "Rhoicinus", 
                       species = "schlingeri", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)
# Specie 7. Rhoicinus urucu 

urucu <- dismo::gbif(genus = "Rhoicinus", 
                       species = "urucu", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)
# Specie 8. Rhoicinus wallsi 

wallsi <- dismo::gbif(genus = "Rhoicinus", 
                       species = "wallsi", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)
# Specie 9. Rhoicinus wapleri 

wapleri <- dismo::gbif(genus = "Rhoicinus", 
                       species = "wapleri", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)
# Specie 10. Rhoicinus weyrauchi 

weyrauchi <- dismo::gbif(genus = "Rhoicinus", 
                       species = "weyrauchi", 
                       download = TRUE, 
                       geo = TRUE, removeZeros = TRUE)

# Genus Shinobius ---------------------------------------------------------
Shinobius_raw <- dismo::gbif(genus = "Shinobius",
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)

# Specie 1. Shinobius cona

cona <- dismo::gbif(genus = "Shinobius", 
                         species = "cona", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)

# Specie 2. Shinobius orientalis

orientalis <- dismo::gbif(genus = "Shinobius", 
                    species = "orientalis*", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)


# Genus Syntrechalea ------------------------------------------------------
Syntrechalea_raw <- dismo::gbif(genus = "Syntrechalea", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)

# Specie 1. Syntrechalea adis

adis <- dismo::gbif(genus = "Syntrechalea", 
                          species = "adis", 
                          download = TRUE, 
                          geo = TRUE, removeZeros = TRUE)

# Specie 2. Syntrechalea adis

adis <- dismo::gbif(genus = "Syntrechalea", 
                    species = "boliviensis", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)
# Specie 3. Syntrechalea brasilia

brasilia <- dismo::gbif(genus = "Syntrechalea", 
                    species = "brasilia", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)
# Specie 4. Syntrechalea caballero

caballero <- dismo::gbif(genus = "Syntrechalea", 
                    species = "caballero", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)
# Specie 5. Syntrechalea caporiacco

caporiacco <- dismo::gbif(genus = "Syntrechalea", 
                    species = "caporiacco", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)
# Specie 6. Syntrechalea lomalinda

lomalinda <- dismo::gbif(genus = "Syntrechalea", 
                    species = "lomalinda", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)
# Specie 7. Syntrechalea napoensis

napoensis <- dismo::gbif(genus = "Syntrechalea", 
                    species = "napoensis", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)
# Specie 8. Syntrechalea neblina

neblina <- dismo::gbif(genus = "Syntrechalea", 
                    species = "neblina", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)
# Specie 9. Syntrechalea reimoseri

reimoseri <- dismo::gbif(genus = "Syntrechalea", 
                    species = "reimoseri", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)
# Specie 10. Syntrechalea robusta

robusta <- dismo::gbif(genus = "Syntrechalea", 
                    species = "robusta", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)
# Specie 11. Syntrechalea syntrechaloides

syntrechaloides <- dismo::gbif(genus = "Syntrechalea", 
                    species = "syntrechaloides", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)
# Specie 12. Syntrechalea tenuis

tenuis <- dismo::gbif(genus = "Syntrechalea", 
                    species = "tenuis*", 
                    download = TRUE, 
                    geo = TRUE, removeZeros = TRUE)

# Genus Trechalea ---------------------------------------------------------
Trechalea_raw <- dismo::gbif(genus = "Trechalea", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)
# Specie 1. Trechalea amazonica

amazonica <- dismo::gbif(genus = "Trechalea", 
                      species = "amazonica", 
                      download = TRUE, 
                      geo = TRUE, removeZeros = TRUE)
# Specie 2. Trechalea bucculenta

bucculenta <- dismo::gbif(genus = "Trechalea", 
                      species = "bucculenta", 
                      download = TRUE, 
                      geo = TRUE, removeZeros = TRUE)
# Specie 3. Trechalea connexa

connexa <- dismo::gbif(genus = "Trechalea", 
                      species = "connexa", 
                      download = TRUE, 
                      geo = TRUE, removeZeros = TRUE)
# Specie 4. Trechalea extensa

extensa <- dismo::gbif(genus = "Trechalea", 
                      species = "extensa", 
                      download = TRUE, 
                      geo = TRUE, removeZeros = TRUE)
# Specie 5. Trechalea gertschi

gertschi <- dismo::gbif(genus = "Trechalea", 
                      species = "gertschi", 
                      download = TRUE, 
                      geo = TRUE, removeZeros = TRUE)
# Specie 6. Trechalea longitarsis

longitarsis <- dismo::gbif(genus = "Trechalea", 
                      species = "longitarsis*", 
                      download = TRUE, 
                      geo = TRUE, removeZeros = TRUE)
# Specie 7. Trechalea tenuis

macconnelli <- dismo::gbif(genus = "Trechalea", 
                      species = "macconnelli", 
                      download = TRUE, 
                      geo = TRUE, removeZeros = TRUE)
# Specie 8. Trechalea paucispina

paucispina <- dismo::gbif(genus = "Trechalea", 
                      species = "paucispina", 
                      download = TRUE, 
                      geo = TRUE, removeZeros = TRUE)
# Specie 9. Trechalea tirimbina

tirimbina <- dismo::gbif(genus = "Trechalea", 
                      species = "tirimbina", 
                      download = TRUE, 
                      geo = TRUE, removeZeros = TRUE)


# Genus Trechaleoides -----------------------------------------------------

Trechaleoides_raw <- dismo::gbif(genus = "Trechaleoides",
                          download = TRUE, 
                          geo = TRUE, removeZeros = TRUE)

# Specie 1. Trechaleoides tirimbina

biocellata <- dismo::gbif(genus = "Trechaleoides", 
                         species = "biocellata", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)
# Specie 2. Trechaleoides keyserlingi

keyserlingi <- dismo::gbif(genus = "Trechaleoides", 
                         species = "keyserlingi*", 
                         download = TRUE, 
                         geo = TRUE, removeZeros = TRUE)






#             **All species with less than 10 occurrence points were removed



# PART 2 ------------------------------------------------------------------
library(dplyr)

# Filtrando pontos únicos de longitude e latitude para cada espécie
tabela_filtrada <- dados_raw %>%
  group_by(acceptedScientificName) %>%
  distinct(lon, lat, .keep_all = TRUE) %>%
  ungroup()

# Visualizando os primeiros dados da tabela filtrada
head(tabela_filtrada)





# Filtrando colunas 

Barrisca_data <- Barrisca_raw[,c("acceptedScientificName", "country","lon","lat","year")]
Barrisca_data <- Barrisca_data %>%
  filter(!is.na(lat) & !is.na(lon))

esquisser(Barrisca_data)

Caricelea_data <- Caricelea_raw[,c("acceptedScientificName", "country","lon","lat","year")]
Caricelea_data <- Caricelea_data %>%
  dplyr:::filter.data.frame(., unique(lat) & unique(lon))

Cupiennius_data <- Cupiennius_raw[,c("acceptedScientificName", "country","lon","lat","year")]
Cupiennius_data <- Cupiennius_data %>%
  filter(!is.na(lat) & !is.na(lon))

esquisser(Cupiennius_data)

# Seleção de colunas 
Dossenus_data <- Dossenus_raw[,c("acceptedScientificName","country", "lon","lat","year")]

# Remover NAs e garantir valores únicos para latitude e longitude
Dossenus_data <- Dossenus_data %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  distinct(lat, lon, .keep_all = TRUE)

#Filtrar generos

# Seleção de colunas 
Trechaleoides_data <- Trechaleoides_raw[,c("acceptedScientificName","country","lat","lon","year")]

# Remover NAs e garantir valores únicos para latitude e longitude
Trechaleoides_data <- Trechaleoides_data %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  distinct(lat, lon, .keep_all = TRUE)

rm(Trechaleoides_raw)

# Juntar todos os generos em um unico data.frame

dados_brutos <- rbind(Barrisca_data, Caricelea_data, Cupiennius_data, Dossenus_data,
                      Enna_data, Heidrunea_data, Hesydrus_data, Neoctenus_data, Paradossenus_data,
                      Paratrechalea_data, Rhoicinus_data, Shinobius_data, Syntrechalea_data,
                      Trechalea_data, Trechaleoides_data)

# Salvando o data.frame como arquivo .xlsx
library(openxlsx)

write.xlsx(dados_brutos, "dados_brutos.xlsx")

# Importando uma planilha específica do arquivo .xlsx
library(readxl)
dados_raw <- read_excel("dados_brutos.xlsx")

# Tabela de frequência das categorias
unique(dados_raw$acceptedScientificName)


# Criando o gráfico de barras
library(ggplot2)
barplot(frequencia_ssp, 
        main = "Frequência das Espécies", 
        xlab = "Espécies", 
        ylab = "Frequência", 
        las = 2, # Rotaciona os rótulos no eixo X
        col = "steelblue", 
        border = "black")

library(esquisse)
esquisser(dados_raw)


library(ggplot2)
# Gráfico relacionando países e ano das ocorrências (não sei se gostei desse gráfico)
ggplot(occ_Trechaleidae) +
 aes(x = year, y = country) +
 geom_boxplot(fill = "steelblue4") +
 theme_classic()

#  - Juntando espécies por número de ocorrências 



#### Pacotes que o gustavo disse para usar na filtragem dos dados 
install.packages("spThin", "CoordinateCleaner" )

#Outros pacotes que podem ser necessários para essa filtragem
library(countrycode)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sf)
library(spThin)


