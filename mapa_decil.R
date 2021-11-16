# Bibliotecas -------------------------------------------------------------
library(tidyverse)
library(sf)
library(geobr)
library(tmap)


# Bases --------------------------------------------------------------------

#Base Geral
idade_0_11_inter_cca <-read_csv("bases/idade_0_11_inter_cca.csv")
idade_0_11_inter_obito <-read_csv("bases/idade_0_11_inter_obito.csv")

idade_12_18_inter_adolesc <-read.csv("bases/idade_12_18_inter_adolesc.csv")
idade_12_18_inter_obito <-read.csv("bases/idade_12_18_inter_obito.csv")

#Base primeira onda
idade_0_11_1onda_cca <-read_csv("bases/idade_0_11_1onda_cca.csv")
idade_0_11_1onda_obito <-read_csv("bases/idade_0_11_1onda_obito.csv")

idade_12_18_1onda_adolesc <-read.csv("bases/idade_12_18_1onda_adolesc.csv")
idade_12_18_1onda_obito <-read.csv("bases/idade_12_18_1onda_obito.csv")


#Base segunda onda
idade_0_11_2onda_cca <- read_csv("bases/idade_0_11_2onda_cca.csv")
idade_0_11_2onda_obito <- read_csv("bases/idade_0_11_2onda_obito.csv")

idade_12_18_2onda_adolesc <-read.csv("bases/idade_12_18_2onda_adolesc.csv")
idade_12_18_2onda_obito <-read.csv("bases/idade_12_18_2onda_obito.csv")


# Mapas Crianças -------------------------------------------------------------------

#RR da letalidade geral em crianças
mapaRR_crianca <- idade_0_11_inter_obito %>% as.data.frame()
mapaRR_crianca$Decil <- rownames(mapaRR_crianca)
names(mapaRR_crianca)[1] <- "RR"
mapaRR_crianca[1,] <- c(1, 1)
mapaRR_crianca$Decil <- as.factor(mapaRR_crianca$Decil)

idade_0_11_inter_cca  <- idade_0_11_inter_cca  %>% dplyr::select(CO_MUN_RES, ID_MN_RESI, Decil)
idade_0_11_inter_cca  <- unique(idade_0_11_inter_cca )
idade_0_11_inter_cca$CO_MUN_RES <- as.character(idade_0_11_inter_cca$CO_MUN_RES)
mapaRR_crianca <- merge(mapaRR_crianca, idade_0_11_inter_cca , by = "Decil", all = T)

municipios <- geobr::read_municipality(year = 2019)
municipios$code_muni <- as.character(municipios$code_muni)
municipios$code_muni <- substr(municipios$code_muni,0,6)

mapaRR_crianca <- merge(municipios, mapaRR_crianca,  by.x = "code_muni", by.y = "CO_MUN_RES", all = T)
mapaRR_crianca$RR <- ifelse(is.na(mapaRR_crianca$RR), "0", mapaRR_crianca$RR)
mapaRR_crianca$RR <- as.numeric(mapaRR_crianca$RR) 


g1 <- ggplot(mapaRR_crianca) + 
  geom_sf(aes(fill=RR), color = NA) +
scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1) +
  labs(fill="Risco Relativo")


##RR da letalidade geral em crianças na primeira onda
mapa1_crianca <- idade_0_11_1onda_obito %>% as.data.frame()
mapa1_crianca$Decil <- rownames(mapa1_crianca)
names(mapa1_crianca)[1] <- "RR"
mapa1_crianca[1,] <- c(1, 1)
mapa1_crianca$Decil <- as.factor(mapa1_crianca$Decil)

idade_0_11_1onda_cca <- idade_0_11_1onda_cca %>% dplyr::select(CO_MUN_RES, ID_MN_RESI, Decil)
idade_0_11_1onda_cca <- unique(idade_0_11_1onda_cca)
idade_0_11_1onda_cca$CO_MUN_RES <- as.character(idade_0_11_1onda_cca$CO_MUN_RES)
mapa1_crianca <- merge(mapa1_crianca, idade_0_11_1onda_cca, by = "Decil", all = T)

municipios <- geobr::read_municipality(year = 2019)
municipios$code_muni <- as.character(municipios$code_muni)
municipios$code_muni <- substr(municipios$code_muni,0,6)

mapa1_crianca <- merge(municipios, mapa1_crianca,  by.x = "code_muni", by.y = "CO_MUN_RES", all = T)
mapa1_crianca$RR <- ifelse(is.na(mapa1_crianca$RR), "0", mapa1_crianca$RR)
mapa1_crianca$RR <- as.numeric(mapa1_crianca$RR) 

g2<-ggplot(mapa1_crianca) + 
  geom_sf(aes(fill=RR), color = NA) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1) +
  labs(fill="Risco Relativo")




##RR da letalidade geral em crianças na segunda onda
mapa2_crianca <- idade_0_11_2onda_obito %>% as.data.frame()
mapa2_crianca$Decil <- rownames(mapa2_crianca)
names(mapa2_crianca)[1] <- "RR"
mapa2_crianca[1,] <- c(1, 1)
mapa2_crianca$Decil <- as.factor(mapa2_crianca$Decil)

idade_0_11_2onda_cca <- idade_0_11_2onda_cca %>% dplyr::select(CO_MUN_RES, ID_MN_RESI, Decil)
idade_0_11_2onda_cca <- unique(idade_0_11_2onda_cca)
idade_0_11_2onda_cca$CO_MUN_RES <- as.character(idade_0_11_2onda_cca$CO_MUN_RES)
mapa2_crianca <- merge(mapa2_crianca, idade_0_11_2onda_cca, by = "Decil", all = T)

municipios <- geobr::read_municipality(year = 2019)
municipios$code_muni <- as.character(municipios$code_muni)
municipios$code_muni <- substr(municipios$code_muni,0,6)

mapa2_crianca <- merge(municipios, mapa2_crianca,  by.x = "code_muni", by.y = "CO_MUN_RES", all = T)
mapa2_crianca$RR <- ifelse(is.na(mapa2_crianca$RR), "0", mapa2_crianca$RR)
mapa2_crianca$RR <- as.numeric(mapa2_crianca$RR) 

g3<-ggplot(mapa2_crianca) + 
  geom_sf(aes(fill=RR), color = NA) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1) +
  labs(fill="Risco Relativo")







############ ADOLESCENTE ################
# Mapas Adolescente -------------------------------------------------------------------

#RR da letalidade geral em Adolescente
mapaRR_adolesc <- idade_12_18_inter_obito %>% as.data.frame()
mapaRR_adolesc$Decil <- rownames(mapaRR_adolesc)
names(mapaRR_adolesc)[1] <- "RR"
mapaRR_adolesc[1,] <- c(1, 1)
mapaRR_adolesc$Decil <- as.factor(mapaRR_adolesc$Decil)

idade_12_18_inter_adolesc  <- idade_12_18_inter_adolesc  %>% dplyr::select(CO_MUN_RES, ID_MN_RESI, Decil)
idade_12_18_inter_adolesc  <- unique(idade_12_18_inter_adolesc )
idade_12_18_inter_adolesc$CO_MUN_RES <- as.character(idade_12_18_inter_adolesc$CO_MUN_RES)
mapaRR_adolesc <- merge(mapaRR_adolesc, idade_12_18_inter_adolesc , by = "Decil", all = T)

municipios <- geobr::read_municipality(year = 2019)
municipios$code_muni <- as.character(municipios$code_muni)
municipios$code_muni <- substr(municipios$code_muni,0,6)

mapaRR_adolesc <- merge(municipios, mapaRR_adolesc,  by.x = "code_muni", by.y = "CO_MUN_RES", all = T)
mapaRR_adolesc$RR <- ifelse(is.na(mapaRR_adolesc$RR), "0", mapaRR_adolesc$RR)
mapaRR_adolesc$RR <- as.numeric(mapaRR_adolesc$RR) 


g4<-ggplot(mapaRR_adolesc) + 
  geom_sf(aes(fill=RR), color = NA) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1) +
  labs(fill="Risco Relativo")


##RR da letalidade geral em Adolescente na primeira onda
mapa1_adolesc <- idade_12_18_1onda_obito %>% as.data.frame()
mapa1_adolesc$Decil <- rownames(mapa1_adolesc)
names(mapa1_adolesc)[1] <- "RR"
mapa1_adolesc[1,] <- c(1, 1)
mapa1_adolesc$Decil <- as.factor(mapa1_adolesc$Decil)

idade_12_18_1onda_adolesc <- idade_12_18_1onda_adolesc %>% dplyr::select(CO_MUN_RES, ID_MN_RESI, Decil)
idade_12_18_1onda_adolesc <- unique(idade_12_18_1onda_adolesc)
idade_12_18_1onda_adolesc$CO_MUN_RES <- as.character(idade_12_18_1onda_adolesc$CO_MUN_RES)
mapa1_adolesc <- merge(mapa1_adolesc, idade_12_18_1onda_adolesc, by = "Decil", all = T)

municipios <- geobr::read_municipality(year = 2019)
municipios$code_muni <- as.character(municipios$code_muni)
municipios$code_muni <- substr(municipios$code_muni,0,6)

mapa1_adolesc <- merge(municipios, mapa1_adolesc,  by.x = "code_muni", by.y = "CO_MUN_RES", all = T)
mapa1_adolesc$RR <- ifelse(is.na(mapa1_adolesc$RR), "0", mapa1_adolesc$RR)
mapa1_adolesc$RR <- as.numeric(mapa1_adolesc$RR) 

g5<-ggplot(mapa1_adolesc) + 
  geom_sf(aes(fill=RR), color = NA) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1) +
  labs(fill="Risco Relativo")




##RR da letalidade geral em Adolescente na segunda onda
mapa2_adolesc <- idade_12_18_2onda_obito %>% as.data.frame()
mapa2_adolesc$Decil <- rownames(mapa2_adolesc)
names(mapa2_adolesc)[1] <- "RR"
mapa2_adolesc[1,] <- c(1, 1)
mapa2_adolesc$Decil <- as.factor(mapa2_adolesc$Decil)

idade_12_18_2onda_adolesc <- idade_12_18_2onda_adolesc %>% dplyr::select(CO_MUN_RES, ID_MN_RESI, Decil)
idade_12_18_2onda_adolesc <- unique(idade_12_18_2onda_adolesc)
idade_12_18_2onda_adolesc$CO_MUN_RES <- as.character(idade_12_18_2onda_adolesc$CO_MUN_RES)
mapa2_adolesc <- merge(mapa2_adolesc, idade_12_18_2onda_adolesc, by = "Decil", all = T)

municipios <- geobr::read_municipality(year = 2019)
municipios$code_muni <- as.character(municipios$code_muni)
municipios$code_muni <- substr(municipios$code_muni,0,6)

mapa2_adolesc <- merge(municipios, mapa2_adolesc,  by.x = "code_muni", by.y = "CO_MUN_RES", all = T)
mapa2_adolesc$RR <- ifelse(is.na(mapa2_adolesc$RR), "0", mapa2_adolesc$RR)
mapa2_adolesc$RR <- as.numeric(mapa2_adolesc$RR) 

g6<-ggplot(mapa2_adolesc) + 
  geom_sf(aes(fill=RR), color = NA) +
  scale_fill_distiller(type = "seq",
                       palette = "Reds",
                       direction = 1) +
  labs(fill="Risco Relativo")



## Figura ##
library(ggpubr)
ggpubr::ggarrange(g1,g2,g3,g4,g5,g6, labels = 
                    c("A","B","C","D","E","F"), ncol = 3, 
                  nrow = 2, legend = "right",common.legend = T)








# # Bases --------------------------------------------------------------------
# idade_0_11_1onda_obito <- read_csv("bases/idade_0_11_1onda_obito.csv")
# base_inter_decil <- read_csv("bases/base_inter_decil.csv")
# 
# 
# # Mapas -------------------------------------------------------------------
# #Criança primeira onda
# mapa1_crianca <- idade_0_11_1onda_obito %>% as.data.frame()
# mapa1_crianca$Decil <- rownames(mapa1_crianca)
# names(mapa1_crianca)[1] <- "RR"
# mapa1_crianca[1,] <- c(1, 1)
# mapa1_crianca$Decil <- as.factor(mapa1_crianca$Decil)
# 
# base_inter_decil <- base_inter_decil %>% dplyr::select(CO_MUN_RES, ID_MN_RESI, Decil)
# base_inter_decil$CO_MUN_RES <- as.character(base_inter_decil$CO_MUN_RES)
# mapa1_crianca <- merge(mapa1_crianca, base_inter_decil, by = "Decil", all = T)
# municipios <- brmap::brmap_municipio
# municipios$municipio_cod <- as.character(municipios$municipio_cod)
# municipios$municipio_cod <- substr(municipios$municipio_cod,0,6)
# 
# mapa1_crianca <- merge(municipios, mapa1_crianca,  by.x = "municipio_cod", by.y = "CO_MUN_RES", all = T)
# 
# ggplot(mapa1_crianca) + 
#   geom_sf(aes(fill = RR))+
#   scale_fill_distiller(type = "seq",
#                        palette = "Blues",
#                        direction = 1) +
#   labs(fill="Risco Relativo")+
#   theme(
#     panel.background = element_blank(),
#     panel.grid.major = element_line(color = "transparent"),
#     axis.text = element_blank(),
#     axis.ticks = element_blank())
# 
# 
