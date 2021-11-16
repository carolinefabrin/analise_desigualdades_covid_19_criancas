library(tidyverse)
library(plotly)
library(sf)
library(ggmap)
library(sunburstR)
library(leaflet)
library(treemap)
library(brmap)
library(ggpubr)
library(lmtest)
library(finalfit)
library(reshape2)
library(gridExtra)
library(cowplot)
library(kableExtra)
library(flextable)
library(lubridate)
library(reshape2)
library(pander)


## Criacao plot primeira onda em crianças
onda1_0_11_plot <- data.frame(idade_0_11_1onda_amostra[[2]][c(1,2,3)],
                              idade_0_11_1onda_raio_x[[2]][c(1,2,3)],
                              idade_0_11_1onda_tomo[[2]][c(1,2,3)],
                              idade_0_11_1onda_vent[[2]][c(1,2,3)],
                              idade_0_11_1onda_vent_inva[[2]][c(1,2,3)],
                              idade_0_11_1onda_vent_n_inva[[2]][c(1,2,3)],
                              idade_0_11_1onda_uti[[2]][c(1,2,3)],
                              idade_0_11_1onda_obito[[2]][c(1,2,3)])
names(onda1_0_11_plot) <- c("C.A.B*",
                            "C.A.B* 2,5%",
                            "C.A.B* 97,5",
                            "RX*",
                            "RX* 2,5%",
                            "RX* 97,5%",
                            "TOMO*",
                            "TOMO* 2,5%",
                            "TOMO* 97,5",
                            "S.VEN*",
                            "S.VEN* 2,5%",
                            "S.VEN* 97,5",
                            "INVA*",
                            "INVA* 2,5%",
                            "INVA* 97,5",
                            "N INV*",
                            "N INV* 2,5%",
                            "N INV* 97,5",
                            "UTI",
                            "UTI 2,5%",
                            "UTI 97,5",
                            "LETAL*",
                            "LETAL* 2,5%",
                            "LETAL* 97,5")

onda1_0_11_plot$Decil <- row.names(onda1_0_11_plot)
onda1_0_11_plot <- subset(onda1_0_11_plot, onda1_0_11_plot$Decil != "(Intercept)")
onda1_0_11_plot[10,] <- 1
onda1_0_11_plot$Decil <- ifelse(onda1_0_11_plot$Decil == 1, "Decil1", onda1_0_11_plot$Decil)

onda1_0_11_plot$Decil <- factor(onda1_0_11_plot$Decil, levels = c("Decil1",
                                                                  "Decil2",
                                                                  "Decil3",
                                                                  "Decil4",
                                                                  "Decil5",
                                                                  "Decil6",
                                                                  "Decil7",
                                                                  "Decil8",
                                                                  "Decil9",
                                                                  "Decil10"))
onda1_0_11_plot <- melt(onda1_0_11_plot, id.var = "Decil")

onda1_0_11_medias <- c("C.A.B*",
                       "RX*",
                       "TOMO*",
                       "S.VEN*",
                       "INVA*",
                       "N INV*",
                       "UTI",
                       "LETAL*")

onda1_0_11_plot_medias <- subset(onda1_0_11_plot, onda1_0_11_plot$variable %in% onda1_0_11_medias)

cc1 <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))
g1 <- ggplot (onda1_0_11_plot_medias, aes(x=value, y=1, color=Decil)) +
  geom_point(size = 6, alpha = 0.8)+
  facet_grid(variable ~.)+
  scale_colour_manual(values=cc1)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  xlab("Risco Relativo")+
  geom_vline(xintercept = 1)




## Criacao plot segunda onda em crianças
onda2_0_11_plot <- data.frame(idade_0_11_2onda_amostra[[2]][c(1,2,3)],
                              idade_0_11_2onda_raio_x[[2]][c(1,2,3)],
                              idade_0_11_2onda_tomo[[2]][c(1,2,3)],
                              idade_0_11_2onda_vent[[2]][c(1,2,3)],
                              idade_0_11_2onda_vent_inva[[2]][c(1,2,3)],
                              idade_0_11_2onda_vent_n_inva[[2]][c(1,2,3)],
                              idade_0_11_2onda_uti[[2]][c(1,2,3)],
                              idade_0_11_2onda_obito[[2]][c(1,2,3)])
names(onda2_0_11_plot) <- c("C.A.B*",
                            "C.A.B* 2,5%",
                            "C.A.B* 97,5",
                            "RX*",
                            "RX* 2,5%",
                            "RX* 97,5%",
                            "TOMO*",
                            "TOMO* 2,5%",
                            "TOMO* 97,5",
                            "S.VEN*",
                            "S.VEN* 2,5%",
                            "S.VEN* 97,5",
                            "INVA*",
                            "INVA* 2,5%",
                            "INVA* 97,5",
                            "N INV*",
                            "N INV* 2,5%",
                            "N INV* 97,5",
                            "UTI",
                            "UTI 2,5%",
                            "UTI 97,5",
                            "LETAL*",
                            "LETAL* 2,5%",
                            "LETAL* 97,5")

onda2_0_11_plot$Decil <- row.names(onda2_0_11_plot)
onda2_0_11_plot <- subset(onda2_0_11_plot, onda2_0_11_plot$Decil != "(Intercept)")
onda2_0_11_plot[10,] <- 1
onda2_0_11_plot$Decil <- ifelse(onda2_0_11_plot$Decil == 1, "Decil1", onda2_0_11_plot$Decil)

onda2_0_11_plot$Decil <- factor(onda2_0_11_plot$Decil, levels = c("Decil1",
                                                                  "Decil2",
                                                                  "Decil3",
                                                                  "Decil4",
                                                                  "Decil5",
                                                                  "Decil6",
                                                                  "Decil7",
                                                                  "Decil8",
                                                                  "Decil9",
                                                                  "Decil10"))
onda2_0_11_plot <- melt(onda2_0_11_plot, id.var = "Decil")

onda2_0_11_medias <- c("C.A.B*",
                       "RX*",
                       "TOMO*",
                       "S.VEN*",
                       "INVA*",
                       "N INV*",
                       "UTI",
                       "LETAL*")

onda2_0_11_plot_medias <- subset(onda2_0_11_plot, onda2_0_11_plot$variable %in% onda2_0_11_medias)

cc2 <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))
g2 <- ggplot (onda2_0_11_plot_medias, aes(x=value, y=1, color=Decil)) +
  geom_point(size = 6, alpha = 0.8)+
  facet_grid(variable ~.)+
  scale_colour_manual(values=cc2)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  xlab("Risco Relativo")+
  geom_vline(xintercept = 1)



## Criacao plot primeira onda em adolescente
onda1_12_18_plot <- data.frame(idade_12_18_1onda_amostra[[2]][c(1,2,3)],
                              idade_12_18_1onda_raio_x[[2]][c(1,2,3)],
                              idade_12_18_1onda_tomo[[2]][c(1,2,3)],
                              idade_12_18_1onda_vent[[2]][c(1,2,3)],
                              idade_12_18_1onda_vent_inva[[2]][c(1,2,3)],
                              idade_12_18_1onda_vent_n_inva[[2]][c(1,2,3)],
                              idade_12_18_1onda_uti[[2]][c(1,2,3)],
                              idade_12_18_1onda_obito[[2]][c(1,2,3)])
names(onda1_12_18_plot) <- c("C.A.B*",
                            "C.A.B* 2,5%",
                            "C.A.B* 97,5",
                            "RX*",
                            "RX* 2,5%",
                            "RX* 97,5%",
                            "TOMO*",
                            "TOMO* 2,5%",
                            "TOMO* 97,5",
                            "S.VEN*",
                            "S.VEN* 2,5%",
                            "S.VEN* 97,5",
                            "INVA*",
                            "INVA* 2,5%",
                            "INVA* 97,5",
                            "N INV*",
                            "N INV* 2,5%",
                            "N INV* 97,5",
                            "UTI",
                            "UTI 2,5%",
                            "UTI 97,5",
                            "LETAL*",
                            "LETAL* 2,5%",
                            "LETAL* 97,5")

onda1_12_18_plot$Decil <- row.names(onda1_12_18_plot)
onda1_12_18_plot <- subset(onda1_12_18_plot, onda1_12_18_plot$Decil != "(Intercept)")
onda1_12_18_plot[10,] <- 1
onda1_12_18_plot$Decil <- ifelse(onda1_12_18_plot$Decil == 1, "Decil1", onda1_12_18_plot$Decil)

onda1_12_18_plot$Decil <- factor(onda1_12_18_plot$Decil, levels = c("Decil1",
                                                                  "Decil2",
                                                                  "Decil3",
                                                                  "Decil4",
                                                                  "Decil5",
                                                                  "Decil6",
                                                                  "Decil7",
                                                                  "Decil8",
                                                                  "Decil9",
                                                                  "Decil10"))
onda1_12_18_plot <- melt(onda1_12_18_plot, id.var = "Decil")

onda1_12_18_medias <- c("C.A.B*",
                       "RX*",
                       "TOMO*",
                       "S.VEN*",
                       "INVA*",
                       "N INV*",
                       "UTI",
                       "LETAL*")

onda1_12_18_plot_medias <- subset(onda1_12_18_plot, onda1_12_18_plot$variable %in% onda1_12_18_medias)

cc3 <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))
g3 <- ggplot (onda1_12_18_plot_medias, aes(x=value, y=1, color=Decil)) +
  geom_point(size = 6, alpha = 0.8)+
  facet_grid(variable ~.)+
  scale_colour_manual(values=cc3)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  xlab("Risco Relativo")+
  geom_vline(xintercept = 1)





## Criacao plot segunda onda onda em adolescente
onda2_12_18_plot <- data.frame(idade_12_18_2onda_amostra[[2]][c(1,2,3)],
                               idade_12_18_2onda_raio_x[[2]][c(1,2,3)],
                               idade_12_18_2onda_tomo[[2]][c(1,2,3)],
                               idade_12_18_2onda_vent[[2]][c(1,2,3)],
                               idade_12_18_2onda_vent_inva[[2]][c(1,2,3)],
                               idade_12_18_2onda_vent_n_inva[[2]][c(1,2,3)],
                               idade_12_18_2onda_uti[[2]][c(1,2,3)],
                               idade_12_18_2onda_obito[[2]][c(1,2,3)])
names(onda2_12_18_plot) <- c("C.A.B*",
                             "C.A.B* 2,5%",
                             "C.A.B* 97,5",
                             "RX*",
                             "RX* 2,5%",
                             "RX* 97,5%",
                             "TOMO*",
                             "TOMO* 2,5%",
                             "TOMO* 97,5",
                             "S.VEN*",
                             "S.VEN* 2,5%",
                             "S.VEN* 97,5",
                             "INVA*",
                             "INVA* 2,5%",
                             "INVA* 97,5",
                             "N INV*",
                             "N INV* 2,5%",
                             "N INV* 97,5",
                             "UTI",
                             "UTI 2,5%",
                             "UTI 97,5",
                             "LETAL*",
                             "LETAL* 2,5%",
                             "LETAL* 97,5")

onda2_12_18_plot$Decil <- row.names(onda2_12_18_plot)
onda2_12_18_plot <- subset(onda2_12_18_plot, onda2_12_18_plot$Decil != "(Intercept)")
onda2_12_18_plot[10,] <- 1
onda2_12_18_plot$Decil <- ifelse(onda2_12_18_plot$Decil == 1, "Decil1", onda2_12_18_plot$Decil)

onda2_12_18_plot$Decil <- factor(onda2_12_18_plot$Decil, levels = c("Decil1",
                                                                    "Decil2",
                                                                    "Decil3",
                                                                    "Decil4",
                                                                    "Decil5",
                                                                    "Decil6",
                                                                    "Decil7",
                                                                    "Decil8",
                                                                    "Decil9",
                                                                    "Decil10"))
onda2_12_18_plot <- melt(onda2_12_18_plot, id.var = "Decil")

onda2_12_18_medias <- c("C.A.B*",
                        "RX*",
                        "TOMO*",
                        "S.VEN*",
                        "INVA*",
                        "N INV*",
                        "UTI",
                        "LETAL*")

onda2_12_18_plot_medias <- subset(onda2_12_18_plot, onda2_12_18_plot$variable %in% onda2_12_18_medias)

cc4 <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))
g4 <- ggplot (onda2_12_18_plot_medias, aes(x=value, y=1, color=Decil)) +
  geom_point(size = 6, alpha = 0.8)+
  facet_grid(variable ~.)+
  scale_colour_manual(values=cc4)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  xlab("Risco Relativo")+
  geom_vline(xintercept = 1)




#Juntando os graficos
ggpubr::ggarrange(g1,g2,
                  labels = c("A", "B"), ncol = 2, 
                  legend = "none")



ggpubr::ggarrange(g3,g4,
                  labels = c("C", "D"), ncol = 2, 
                  legend = "bottom",common.legend = T)



######## ANALISE REGIONAL ############
## Criacao plot primeira onda em crianças
onda1_regiao_0_11_plot <- data.frame(idade_0_11_1onda_regiao_amostra[[2]][c(1,2,3)],
                              idade_0_11_1onda_regiao_raio_x[[2]][c(1,2,3)],
                              idade_0_11_1onda_regiao_tomo[[2]][c(1,2,3)],
                              idade_0_11_1onda_regiao_vent[[2]][c(1,2,3)],
                              idade_0_11_1onda_regiao_vent_inva[[2]][c(1,2,3)],
                              idade_0_11_1onda_regiao_vent_n_inva[[2]][c(1,2,3)],
                              idade_0_11_1onda_regiao_uti[[2]][c(1,2,3)],
                              idade_0_11_1onda_regiao_obito[[2]][c(1,2,3)])
names(onda1_regiao_0_11_plot) <- c("C.A.B*",
                            "C.A.B* 2,5%",
                            "C.A.B* 97,5",
                            "RX*",
                            "RX* 2,5%",
                            "RX* 97,5%",
                            "TOMO*",
                            "TOMO* 2,5%",
                            "TOMO* 97,5",
                            "S.VEN*",
                            "S.VEN* 2,5%",
                            "S.VEN* 97,5",
                            "INVA*",
                            "INVA* 2,5%",
                            "INVA* 97,5",
                            "N INV*",
                            "N INV* 2,5%",
                            "N INV* 97,5",
                            "UTI",
                            "UTI 2,5%",
                            "UTI 97,5",
                            "LETAL*",
                            "LETAL* 2,5%",
                            "LETAL* 97,5")

onda1_regiao_0_11_plot$Regiao <- row.names(onda1_regiao_0_11_plot)
onda1_regiao_0_11_plot <- subset(onda1_regiao_0_11_plot, onda1_regiao_0_11_plot$Regiao != "(Intercept)")
onda1_regiao_0_11_plot[5,] <- 1
onda1_regiao_0_11_plot$Regiao <- ifelse(onda1_regiao_0_11_plot$Regiao == 1, "REGIAONORDESTE", onda1_regiao_0_11_plot$Regiao)

onda1_regiao_0_11_plot$Regiao <- factor(onda1_regiao_0_11_plot$Regiao, levels = c("REGIAONORDESTE",
                                                                                 "REGIAONORTE",
                                                                                 "REGIAOCENTRO-OESTE",
                                                                                 "REGIAOSUL",
                                                                                 "REGIAOSUDESTE"))	
                                                                      

onda1_regiao_0_11_plot <- melt(onda1_regiao_0_11_plot, id.var = "Regiao")

onda1_regiao_0_11_medias <- c("C.A.B*",
                       "RX*",
                       "TOMO*",
                       "S.VEN*",
                       "INVA*",
                       "N INV*",
                       "UTI",
                       "LETAL*")

onda1_regiao_0_11_plot_medias <- subset(onda1_regiao_0_11_plot, onda1_regiao_0_11_plot$variable %in% onda1_regiao_0_11_medias)

cc5 <- scales::seq_gradient_pal("black", "grey100", "Lab")(seq(0,1,length.out=6))
g5 <- ggplot (onda1_regiao_0_11_plot_medias, aes(x=value, y=1, color=Regiao)) +
  geom_point(size = 6, alpha = 0.8)+
  facet_grid(variable ~.)+
  scale_colour_manual(values=cc5,
                      name = " ",
                      labels = c("Nordeste", "Norte", "Centro-Oeste", "Sul", "Sudeste"),
                      guide = "legend")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  xlab("Risco Relativo")+
  geom_vline(xintercept = 1)



  ## Criacao plot SEGUNDA onda em crianças
  onda2_regiao_0_11_plot <- data.frame(idade_0_11_2onda_regiao_amostra[[2]][c(1,2,3)],
                                       idade_0_11_2onda_regiao_raio_x[[2]][c(1,2,3)],
                                       idade_0_11_2onda_regiao_tomo[[2]][c(1,2,3)],
                                       idade_0_11_2onda_regiao_vent[[2]][c(1,2,3)],
                                       idade_0_11_2onda_regiao_vent_inva[[2]][c(1,2,3)],
                                       idade_0_11_2onda_regiao_vent_n_inva[[2]][c(1,2,3)],
                                       idade_0_11_2onda_regiao_uti[[2]][c(1,2,3)],
                                       idade_0_11_2onda_regiao_obito[[2]][c(1,2,3)])
  names(onda2_regiao_0_11_plot) <- c("C.A.B*",
                                     "C.A.B* 2,5%",
                                     "C.A.B* 97,5",
                                     "RX*",
                                     "RX* 2,5%",
                                     "RX* 97,5%",
                                     "TOMO*",
                                     "TOMO* 2,5%",
                                     "TOMO* 97,5",
                                     "S.VEN*",
                                     "S.VEN* 2,5%",
                                     "S.VEN* 97,5",
                                     "INVA*",
                                     "INVA* 2,5%",
                                     "INVA* 97,5",
                                     "N INV*",
                                     "N INV* 2,5%",
                                     "N INV* 97,5",
                                     "UTI",
                                     "UTI 2,5%",
                                     "UTI 97,5",
                                     "LETAL*",
                                     "LETAL* 2,5%",
                                     "LETAL* 97,5")
  
  onda2_regiao_0_11_plot$Regiao <- row.names(onda2_regiao_0_11_plot)
  onda2_regiao_0_11_plot <- subset(onda2_regiao_0_11_plot, onda2_regiao_0_11_plot$Regiao != "(Intercept)")
  onda2_regiao_0_11_plot[5,] <- 1
  onda2_regiao_0_11_plot$Regiao <- ifelse(onda2_regiao_0_11_plot$Regiao == 1, "REGIAONORDESTE", onda2_regiao_0_11_plot$Regiao)
  
  onda2_regiao_0_11_plot$Regiao <- factor(onda2_regiao_0_11_plot$Regiao, levels = c("REGIAONORDESTE",
                                                                                    "REGIAONORTE",
                                                                                    "REGIAOCENTRO-OESTE",
                                                                                    "REGIAOSUL",
                                                                                    "REGIAOSUDESTE"))	
  
  
  onda2_regiao_0_11_plot <- melt(onda2_regiao_0_11_plot, id.var = "Regiao")
  
  onda2_regiao_0_11_medias <- c("C.A.B*",
                                "RX*",
                                "TOMO*",
                                "S.VEN*",
                                "INVA*",
                                "N INV*",
                                "UTI",
                                "LETAL*")
  
  onda2_regiao_0_11_plot_medias <- subset(onda2_regiao_0_11_plot, onda2_regiao_0_11_plot$variable %in% onda2_regiao_0_11_medias)
  
  cc6 <- scales::seq_gradient_pal("black", "grey100", "Lab")(seq(0,1,length.out=6))
  g6 <- ggplot (onda2_regiao_0_11_plot_medias, aes(x=value, y=1, color=Regiao)) +
    geom_point(size = 6, alpha = 0.8)+
    facet_grid(variable ~.)+
    scale_colour_manual(values=cc6,
                        name = " ",
                        labels = c("Nordeste", "Norte", "Centro-Oeste", "Sul", "Sudeste"),
                        guide = "legend")+
    theme_bw()+
    theme(panel.grid = element_blank())+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    xlab("Risco Relativo")+
    geom_vline(xintercept = 1)
  
  

  ## Criacao plot primeira onda em adolescente
  onda1_regiao_12_18_plot <- data.frame(idade_12_18_1onda_regiao_amostra[[2]][c(1,2,3)],
                                       idade_12_18_1onda_regiao_raio_x[[2]][c(1,2,3)],
                                       idade_12_18_1onda_regiao_tomo[[2]][c(1,2,3)],
                                       idade_12_18_1onda_regiao_vent[[2]][c(1,2,3)],
                                       idade_12_18_1onda_regiao_vent_inva[[2]][c(1,2,3)],
                                       idade_12_18_1onda_regiao_vent_n_inva[[2]][c(1,2,3)],
                                       idade_12_18_1onda_regiao_uti[[2]][c(1,2,3)],
                                       idade_12_18_1onda_regiao_obito[[2]][c(1,2,3)])
  names(onda1_regiao_12_18_plot) <- c("C.A.B*",
                                     "C.A.B* 2,5%",
                                     "C.A.B* 97,5",
                                     "RX*",
                                     "RX* 2,5%",
                                     "RX* 97,5%",
                                     "TOMO*",
                                     "TOMO* 2,5%",
                                     "TOMO* 97,5",
                                     "S.VEN*",
                                     "S.VEN* 2,5%",
                                     "S.VEN* 97,5",
                                     "INVA*",
                                     "INVA* 2,5%",
                                     "INVA* 97,5",
                                     "N INV*",
                                     "N INV* 2,5%",
                                     "N INV* 97,5",
                                     "UTI",
                                     "UTI 2,5%",
                                     "UTI 97,5",
                                     "LETAL*",
                                     "LETAL* 2,5%",
                                     "LETAL* 97,5")
  
  onda1_regiao_12_18_plot$Regiao <- row.names(onda1_regiao_12_18_plot)
  onda1_regiao_12_18_plot <- subset(onda1_regiao_12_18_plot, onda1_regiao_12_18_plot$Regiao != "(Intercept)")
  onda1_regiao_12_18_plot[5,] <- 1
  onda1_regiao_12_18_plot$Regiao <- ifelse(onda1_regiao_12_18_plot$Regiao == 1, "REGIAONORDESTE", onda1_regiao_12_18_plot$Regiao)
  
  onda1_regiao_12_18_plot$Regiao <- factor(onda1_regiao_12_18_plot$Regiao, levels = c("REGIAONORDESTE",
                                                                                    "REGIAONORTE",
                                                                                    "REGIAOCENTRO-OESTE",
                                                                                    "REGIAOSUL",
                                                                                    "REGIAOSUDESTE"))	
  
  
  onda1_regiao_12_18_plot <- melt(onda1_regiao_12_18_plot, id.var = "Regiao")
  
  onda1_regiao_12_18_medias <- c("C.A.B*",
                                "RX*",
                                "TOMO*",
                                "S.VEN*",
                                "INVA*",
                                "N INV*",
                                "UTI",
                                "LETAL*")
  
  onda1_regiao_12_18_plot_medias <- subset(onda1_regiao_12_18_plot, onda1_regiao_12_18_plot$variable %in% onda1_regiao_12_18_medias)
  
  cc7 <- scales::seq_gradient_pal("black", "grey100", "Lab")(seq(0,1,length.out=6))
  g7 <- ggplot (onda1_regiao_12_18_plot_medias, aes(x=value, y=1, color=Regiao)) +
    geom_point(size = 6, alpha = 0.8)+
    facet_grid(variable ~.)+
    scale_colour_manual(values=cc7,
                        name = " ",
                        labels = c("Nordeste", "Norte", "Centro-Oeste", "Sul", "Sudeste"),
                        guide = "legend")+
    theme_bw()+
    theme(panel.grid = element_blank())+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    xlab("Risco Relativo")+
    geom_vline(xintercept = 1)
  
  
  
  ## Criacao plot segunda onda em adolescente 
  onda2_regiao_12_18_plot <- data.frame(idade_12_18_2onda_regiao_amostra[[2]][c(1,2,3)],
                                        idade_12_18_2onda_regiao_raio_x[[2]][c(1,2,3)],
                                        idade_12_18_2onda_regiao_tomo[[2]][c(1,2,3)],
                                        idade_12_18_2onda_regiao_vent[[2]][c(1,2,3)],
                                        idade_12_18_2onda_regiao_vent_inva[[2]][c(1,2,3)],
                                        idade_12_18_2onda_regiao_vent_n_inva[[2]][c(1,2,3)],
                                        idade_12_18_2onda_regiao_uti[[2]][c(1,2,3)],
                                        idade_12_18_2onda_regiao_obito[[2]][c(1,2,3)])
  names(onda2_regiao_12_18_plot) <- c("C.A.B*",
                                      "C.A.B* 2,5%",
                                      "C.A.B* 97,5",
                                      "RX*",
                                      "RX* 2,5%",
                                      "RX* 97,5%",
                                      "TOMO*",
                                      "TOMO* 2,5%",
                                      "TOMO* 97,5",
                                      "S.VEN*",
                                      "S.VEN* 2,5%",
                                      "S.VEN* 97,5",
                                      "INVA*",
                                      "INVA* 2,5%",
                                      "INVA* 97,5",
                                      "N INV*",
                                      "N INV* 2,5%",
                                      "N INV* 97,5",
                                      "UTI",
                                      "UTI 2,5%",
                                      "UTI 97,5",
                                      "LETAL*",
                                      "LETAL* 2,5%",
                                      "LETAL* 97,5")
  
  onda2_regiao_12_18_plot$Regiao <- row.names(onda2_regiao_12_18_plot)
  onda2_regiao_12_18_plot <- subset(onda2_regiao_12_18_plot, onda2_regiao_12_18_plot$Regiao != "(Intercept)")
  onda2_regiao_12_18_plot[5,] <- 1
  onda2_regiao_12_18_plot$Regiao <- ifelse(onda2_regiao_12_18_plot$Regiao == 1, "REGIAONORDESTE", onda2_regiao_12_18_plot$Regiao)
  
  onda2_regiao_12_18_plot$Regiao <- factor(onda2_regiao_12_18_plot$Regiao, levels = c("REGIAONORDESTE",
                                                                                      "REGIAONORTE",
                                                                                      "REGIAOCENTRO-OESTE",
                                                                                      "REGIAOSUL",
                                                                                      "REGIAOSUDESTE"))	
  
  
  onda2_regiao_12_18_plot <- melt(onda2_regiao_12_18_plot, id.var = "Regiao")
  
  onda2_regiao_12_18_medias <- c("C.A.B*",
                                 "RX*",
                                 "TOMO*",
                                 "S.VEN*",
                                 "INVA*",
                                 "N INV*",
                                 "UTI",
                                 "LETAL*")
  
  onda2_regiao_12_18_plot_medias <- subset(onda2_regiao_12_18_plot, onda2_regiao_12_18_plot$variable %in% onda2_regiao_12_18_medias)
  
  cc8 <- scales::seq_gradient_pal("black", "grey100", "Lab")(seq(0,1,length.out=6))
  g8 <- ggplot (onda2_regiao_12_18_plot_medias, aes(x=value, y=1, color=Regiao)) +
    geom_point(size = 6, alpha = 0.8)+
    facet_grid(variable ~.)+
    scale_colour_manual(values=cc8,
                        name = " ",
                        labels = c("Nordeste", "Norte", "Centro-Oeste", "Sul", "Sudeste"),
                        guide = "legend")+
    theme_bw()+
    theme(panel.grid = element_blank())+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    xlab("Risco Relativo")+
    geom_vline(xintercept = 1)
   
  
  #Juntando os graficos
  ggpubr::ggarrange(g5,g6,
                    labels = c("A", "B"), ncol = 2, 
                    legend = "none")
  
  
  ggpubr::ggarrange(g7,g8,
                    labels = c("C","D"), ncol = 2, 
                    legend = "bottom",common.legend = T)
  
                   
  