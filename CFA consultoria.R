

Discre <- '
Discrepancia =~ p01 + p02 + p03 + p04 + p05 + p06 + p07 + p08 + p09 + p10 + p11 + p12 + p13 + p14 + p15
Dilemas =~ p16 + p17 + p18 + p19 + p20 + p21 + p22 + p23 + p24 + p25 + p26 + p27 + p28
Heuris =~ p29 + p30 + p31 + p32 + p33 + p34 + p35 + p36 + p37
Afront =~ p38 + p39 + p40 + p41 + p42 + p43 + p44 + p45 + p46
Creati =~ p47 + p48 + p49 + p50 + p51 + p52 + p53 + p54 + p55 + p56 + p57 + p58 + p59
'

#Análisis Factorial Confirmatorio para la segunda dimensionalidad.
library(lavaan)

CFAdiscre <- cfa(Discre,
                 orthogonal = FALSE, 
                 data = discrecional[, 13:71],
                 ordered = names(discrecional[, 13:71]))
fitMeasures(CFAdiscre)
library(semPlot)
install.packages("OpenMx")
semPaths(CFAdiscre, intercepts = FALSE,edge.label.cex=0.8, 
         optimizeLatRes = TRUE, groups = "lat",pastel = TRUE, 
         exoVar = FALSE, sizeInt=2.5,edge.color ="black",esize = 3.5,
         label.prop=0.8,sizeLat = 3.5,"std", layout="circle2")
# Construccion de indices haciendo ACP

pca_discre <- PCA(discrecional[,c(13:27)])

pesos_pca <- function(data){
  pca_sec <- PCA(data)
  Coord <-pca_sec$var$coord[,1]
  vl_p <- pca_sec$eig[1]
  vt_p <- Coord/sqrt(vl_p)
  Pesos <- (vt_p/sum(vt_p))
  print(sum(Pesos))
  return(Pesos)
}

indices_pca <- function(data){
  pesos_x <- pesos_pca(data)
  puntajes_ponderados <- as.matrix(data) %*% pesos_x
  pun_min<-min(puntajes_ponderados); pun_max<-max(puntajes_ponderados)
  puntaje<-round(((puntajes_ponderados-pun_min)/(pun_max-pun_min)),4)
  return(puntaje)
}

pca_discre <- pesos_pca(discrecional[,c(13:27)])
ind_discre_pca <- indices_pca(discrecional[,c(13:27)])

pca_dilemas <- pesos_pca(discrecional[,c(28:40)])
ind_dilemas_pca <- indices_pca(discrecional[,c(28:40)])

pca_heuris <- pesos_pca(discrecional[,c(41:49)])
ind_heuris_pca <- indices_pca(discrecional[,c(41:49)])

pca_afront <- pesos_pca(discrecional[,c(50:58)])
ind_afront_pca <- indices_pca(discrecional[,c(50:58)])

pca_creati <- pesos_pca(discrecional[,c(59:71)])
ind_creati_pca <- indices_pca(discrecional[,c(59:71)])

discrecional$ind_discre_pca <- ind_discre_pca
discrecional$ind_dilemas_pca <- ind_dilemas_pca
discrecional$ind_heuris_pca <- ind_heuris_pca
discrecional$ind_afront_pca <- ind_afront_pca
discrecional$ind_creati_pca<- ind_creati_pca
discrecional$ind_discrecional_pca <- rowMeans(discrecional[, c(78:82)], na.rm = TRUE)
