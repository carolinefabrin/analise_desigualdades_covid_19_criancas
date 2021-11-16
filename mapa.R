library(tidyverse)
library(plotly)
library(sf)
library(ggmap)
library(sunburstR)
library(leaflet)
library(treemap)
library(brmap)
library(ggpubr)


### Criação mapa da letalidade em crianças
#Criação da base geral: 
mapa_usuarios <- idade_0_11_regiao_obito[[2]][1]
mapa_usuarios$V2 <- idade_0_11_1onda_regiao_obito[[2]][1]
mapa_usuarios$V3 <- idade_0_11_2onda_regiao_obito[[2]][1]
mapa_usuarios$regiao_cod <- c(2,1,5,4,3)
mapa_usuarios$regiao_cod <- as.integer(mapa_usuarios$regiao_cod, c(2,1,5,4,3))

mapa_usuarios <- merge(brmap_regiao, mapa_usuarios,  by.x = "regiao_cod", by.y = "regiao_cod", all = T)

##Plot letalidade Geral em crianças
  ggplot(mapa_usuarios) +
  geom_sf(aes(fill = (V1)))+
    scale_fill_distiller(type = "seq",
                         palette = "Reds",
                         direction = 1) +
    labs(fill="Risco Relativo")+
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.ticks = element_blank())
    
  
  
  #Criação da base primeira onda: 
  mapa_usuarios_1 <- idade_0_11_1onda_regiao_obito[[2]][1]
  mapa_usuarios_1$regiao_cod <- c(2,1,5,4,3)
  mapa_usuarios_1$regiao_cod <- as.integer(mapa_usuarios_1$regiao_cod, c(2,1,5,4,3))
  
  mapa_usuarios_1 <- merge(brmap_regiao, mapa_usuarios_1,  by.x = "regiao_cod", by.y = "regiao_cod", all = T)
  
 
##Plot letalidade primeira onda em crianças 
 g2<-ggplot(mapa_usuarios_1) +
    geom_sf(aes(fill = (V1)))+
    scale_fill_distiller(type = "seq",
                         palette = "Reds",
                         direction = 1) +
   labs(fill="Risco Relativo")+
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "transparent"),
      axis.text = element_blank(),
      axis.ticks = element_blank())
      
    
  

 #Criação da base segunda onda: 
  mapa_usuarios_2 <-  idade_0_11_2onda_regiao_obito[[2]][1]
  mapa_usuarios_2$regiao_cod <- c(2,1,5,4,3)
  mapa_usuarios_2$regiao_cod <- as.integer(mapa_usuarios_2$regiao_cod, c(2,1,5,4,3))
  
  mapa_usuarios_2 <- merge(brmap_regiao, mapa_usuarios_2,  by.x = "regiao_cod", by.y = "regiao_cod", all = T)
  
  ##Plot letalidade segunda onda em crianças   
 g3<-ggplot(mapa_usuarios_2) +
    geom_sf(aes(fill = (V1)))+
    scale_fill_distiller(type = "seq",
                         palette = "Reds",
                         direction = 1) +
   labs(fill="Risco Relativo")+
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "transparent"),
      axis.text = element_blank(),
      axis.ticks = element_blank())
      


 
#### Criação mapa da letalidade em adolescentes
 
 #Criação da base geral: 
 mapa_usuarios_adoles <- idade_12_18_regiao_obito[[2]][1]
 mapa_usuarios_adoles$V2 <- idade_12_18_1onda_regiao_obito[[2]][1]
 mapa_usuarios_adoles$V3 <- idade_12_18_2onda_regiao_obito[[2]][1]
 mapa_usuarios_adoles$regiao_cod <- c(2,1,5,4,3)
 mapa_usuarios_adoles$regiao_cod <- as.integer(mapa_usuarios_adoles$regiao_cod, c(2,1,5,4,3))

 mapa_usuarios_adoles <- merge(brmap_regiao, mapa_usuarios_adoles,  by.x = "regiao_cod", by.y = "regiao_cod", all = T)
 
 ##Plot letalidade Geral em adolescentes

 g4<-ggplot( mapa_usuarios_adoles) +
   geom_sf(aes(fill = (V1)))+
   scale_fill_distiller(type = "seq",
                        palette = "Reds",
                        direction = 1) +
     labs(fill="Risco Relativo")+
   theme(
     panel.background = element_blank(),
     panel.grid.major = element_line(color = "transparent"),
     axis.text = element_blank(),
     axis.ticks = element_blank())
     
   
 
 #Criação da base primeira onda: 
 mapa_usuarios_adoles1 <- idade_12_18_1onda_regiao_obito[[2]][1]
 mapa_usuarios_adoles1$regiao_cod <- c(2,1,5,4,3)
 mapa_usuarios_adoles1$regiao_cod <- as.integer(mapa_usuarios_adoles1$regiao_cod, c(2,1,5,4,3))
 
 mapa_usuarios_adoles1 <- merge(brmap_regiao, mapa_usuarios_adoles1,  by.x = "regiao_cod", by.y = "regiao_cod", all = T)
 
 
 ##Plot letalidade primeira onda em adolescentes
 
g5<-ggplot(mapa_usuarios_adoles1) +
   geom_sf(aes(fill = (V1)))+
   scale_fill_distiller(type = "seq",
                        palette = "Reds",
                        direction = 1) +
     labs(fill="Risco Relativo")+
   theme(
     panel.background = element_blank(),
     panel.grid.major = element_line(color = "transparent"),
     axis.text = element_blank(),
     axis.ticks = element_blank())
     
   
 
 
 #Criação da base segunda onda: 
 mapa_usuarios_adoles2 <-  idade_12_18_2onda_regiao_obito[[2]][1]
 mapa_usuarios_adoles2$regiao_cod <- c(2,1,5,4,3)
 mapa_usuarios_adoles2$regiao_cod <- as.integer(mapa_usuarios_adoles2$regiao_cod, c(2,1,5,4,3))
 
 mapa_usuarios_adoles2 <- merge(brmap_regiao, mapa_usuarios_adoles2,  by.x = "regiao_cod", by.y = "regiao_cod", all = T)
 
 ##Plot letalidade segunda onda em adolescentes  


 g6<-ggplot(mapa_usuarios_adoles2) +
   geom_sf(aes(fill = (V1)))+
   scale_fill_distiller(type = "seq",
                        palette = "Reds",
                        direction = 1) +
   labs(fill="Risco Relativo") + 
 
   theme(
     panel.background = element_blank(),
     panel.grid.major = element_line(color = "transparent"),
     axis.text = element_blank(),
     axis.ticks = element_blank())
     
   
 
   
 ## Figura ##
  library(ggpubr)
  ggpubr::ggarrange(g1,g2,g3,g4,g5,g6, labels = 
                      c("A","B","C","D","E","F"), ncol = 3, 
                    nrow = 2, legend = "right",common.legend = T)
    
  