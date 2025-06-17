#Concultoria LENIN
# LIBRERIAS ----
install.packages("easypackages")

library("easypackages")

lib_req<-c("lubridate","dplyr","visdat","missMDA","mice","DMwR2","corrplot",
           "editrules","readxl","factoextra","FactoMineR","ade4","parameters","apa","haven","ggplot2","ggpubr","gridExtra",
              "apaTables", "reshape", "GPArotation", "mvtnorm", "psych", 
              "psychometric", "lavaan", "nFactors", "semPlot", "lavaan", "MVN",
              "semTools")
easypackages::packages(lib_req) 
# LECTURA Y LIMPIEZA DE DATOS ----
discrecional <- read_excel("data_discrecional.xlsx")
colnames(discrecional)
discrecional <- discrecional %>% select(-Fecha_inicio, -Fecha_final,-Autor,
                                           -País,-No)
#Se eliminan variables irrelevantes

discrecional <- discrecional %>% mutate(across(c(Sexo, Edad, Años_de_graduado,
                  Nivel,Años_en_el_puesto,Años_en_la_AP,Sueldo), as.factor))

# ANALISIS DESCRIPTIVO INICAL----
# Analisis de frecuencias inical
colnames(discrecional)
indices_columnas <- c(2, 3, 6, 8,9,10,12)

for(i in indices_columnas) {
  cat("\nTabla de frecuencias para:", names(discrecional)[i], "\n")
  print(table(discrecional[, i]))
}

# Contar NA por columna
na_por_columna <- colSums(is.na(discrecional))

# Mostrar resultados
print(na_por_columna)

# Analisis de la estructura multivariada
# Prueba KMO y Barlett por seccion
library(psych)

# KMO
KMO(discrecional[,c(13:27)]) # Índice general
KMO(discrecional[,c(28:40)])
KMO(discrecional[,c(41:49)])
KMO(discrecional[,c(50:58)])
KMO(discrecional[,c(59:71)])
# Bartlett
options(scipen = 999, digits = 4)
cortest.bartlett(cor(discrecional[,c(13:27)]
                     , use = "complete.obs"),
                 n = nrow(na.omit(discrecional[,c(13:27)])))

cortest.bartlett(cor(discrecional[,c(28:40)]
                     , use = "complete.obs"),
                 n = nrow(na.omit(discrecional[,c(28:40)])))

cortest.bartlett(cor(discrecional[,c(41:49)]
                     , use = "complete.obs"),
                 n = nrow(na.omit(discrecional[,c(41:49)])))

cortest.bartlett(cor(discrecional[,c(50:58)]
                     , use = "complete.obs"),
                 n = nrow(na.omit(discrecional[,c(50:58)])))

cortest.bartlett(cor(discrecional[,c(59:71)]
                     , use = "complete.obs"),
                 n = nrow(na.omit(discrecional[,c(59:71)])))
# KMO y Barlett general
KMO(discrecional[,c(13:71)])

cortest.bartlett(cor(discrecional[,c(13:71)]
                     , use = "complete.obs"),
                 n = nrow(na.omit(discrecional[,c(13:71)])))
# Analisis de correlaciones
cor_disc<- cor(discrecional[,c(13:27)], 
                            use = "pairwise.complete.obs",method = "spearman")
corrplot(cor_disc, tl.col='black', tl.cex=1)

cor_dile<- cor(discrecional[,c(28:40)], 
              use = "pairwise.complete.obs",method = "spearman")
par(mfrow = c(2, 2))
corrplot(cor_dile, tl.col='black', tl.cex=1)

cor_heuri<- cor(discrecional[,c(41:49)], 
               use = "pairwise.complete.obs",method = "spearman")
corrplot(cor_heuri, tl.col='black', tl.cex=1)

cor_afront<- cor(discrecional[,c(50:58)], 
                use = "pairwise.complete.obs",method = "spearman")
corrplot(cor_afront, tl.col='black', tl.cex=1)

cor_creati<- cor(discrecional[,c(59:71)], 
                 use = "pairwise.complete.obs",method = "spearman")
corrplot(cor_creati, tl.col='black', tl.cex=1)


fviz_pca_var(PCA(discrecional[,c(13:27)]))
PCA(discrecional[,c(13:27)])

fviz_pca_var(PCA(discrecional[,c(28:40)]))
PCA(discrecional[,c(28:40)])

fviz_pca_var(PCA(discrecional[,c(41:49)]))
PCA(discrecional[,c(41:49)])

fviz_pca_var(PCA(discrecional[,c(50:58)]))
PCA(discrecional[,c(50:58)])

fviz_pca_var(PCA(discrecional[,c(59:71)]))
PCA(discrecional[,c(59:71)])


pca1 <- prcomp(discrecional[,c(13:27)], scale. = TRUE)
pca2 <- prcomp(discrecional[,c(28:40)], scale. = TRUE)
pca3 <- prcomp(discrecional[,c(41:49)], scale. = TRUE)
pca4 <- prcomp(discrecional[,c(50:58)], scale. = TRUE)
pca5 <- prcomp(discrecional[,c(59:71)], scale. = TRUE)

g1 <- fviz_pca_var(pca1, title = "Discrepancia")
g2 <- fviz_pca_var(pca2, title = "Dilemas")
g3 <- fviz_pca_var(pca3, title = "Heurística")
g4 <- fviz_pca_var(pca4, title = "Afrontamiento")
g5 <- fviz_pca_var(pca5, title = "Creatividad")

library(ggpubr)

ggarrange(g1, g2, g3, g4, g5, 
          ncol = 3, nrow = 2, 
          labels = c("A", "B", "C", "D", "E"))
#Indice sumativo
#Analisis descriptivo por puntaje y general

#CONSTRUCCIÓN DE INDICES ----

#Son 15 preguntas con maximo valor de 5, el puntaje maximo posible es 75
#puntaje discrepancia
ind_descrepancia <- rowSums(discrecional[,c(13:27)], na.rm = TRUE)/75
discrecional$ind_descrepancia <- ind_descrepancia 
which.max(rowSums(discrecional[,c(13:27)], na.rm = TRUE)/75)

#Son 13 con maximo valor de 5, el puntaje maximo posible es 65
#Puntaje dilemas
ind_dilemas <- rowSums(discrecional[,c(28:40)], na.rm = TRUE)/65
discrecional$ind_dilemas <- ind_dilemas

#Son 9 con maximo valor de 5, el puntaje maximo posible es 45
#Puntaje heuristica
ind_heuris <- rowSums(discrecional[,c(41:49)], na.rm = TRUE)/45
discrecional$ind_heuris <- ind_heuris

#Son 9 con maximo valor de 5, el puntaje maximo posible es 45
#Puntaje afrontamiento
ind_afront <- rowSums(discrecional[,c(50:58)], na.rm = TRUE)/45
discrecional$ind_afront <- ind_afront

#Son 13 con maximo valor de 5, el puntaje maximo posible es 65
#Puntaje creatividad
ind_creati <- rowSums(discrecional[,c(59:71)], na.rm = TRUE)/65
discrecional$ind_creati <- ind_creati

#PUNTAJE GENERAL
ind_discrecional<- rowMeans(discrecional[, c(72:76)], na.rm = TRUE)
discrecional$ind_discrecional <- ind_discrecional
#ANALISIS DE PUNTAJES----






# Graficos por nivel de discrepancia
levels(discrecional$Sueldo) <- c("1601+", "400-","1201-1600",
                                 "401-800","801-1200","No decir")

vars <- c("Sexo", "Edad", "Años_en_el_puesto", "Sueldo")

# Títulos personalizados para cada gráfico
titulos <- c("Nivel de discrepancia por sexo", 
             "Nivel de discrepancia por edad", 
             "Nivel de discrepancia por años puesto", 
             "Nivel de discrepancia por sueldo")

# Etiquetas para el eje X
xlabels <- c("Sexo", "Edad", "Años en el puesto", "Sueldo")
par(mfrow=c(2,2))
# Ciclo para crear los boxplots
for (i in 1:4) {
  boxplot(discrecional$ind_descrepancia ~ discrecional[[vars[i]]],
          main = titulos[i],
          xlab = xlabels[i],
          ylab = "Nivel de discrepancia")
}

# Graficos por nivel de dilemas

titulos2 <- c("Nivel de dilemas por sexo", 
             "Nivel de dilemas por edad", 
             "Nivel de dilemas por años puesto", 
             "Nivel de dilemas por sueldo")

par(mfrow=c(2,2))

for (i in 1:4) {
  boxplot(discrecional$ind_dilemas~ discrecional[[vars[i]]],
          main = titulos2[i],
          xlab = xlabels[i],
          ylab = "Nivel de dilemas")
}

# Graficos dimencion heuristica-experiencia
titulos3 <- c("Nivel heuristico por sexo", 
              "Nivel heuristico por edad", 
              "Nivel heuristico por años puesto", 
              "Nivel heuristico por sueldo")

par(mfrow=c(2,2))

for (i in 1:4) {
  boxplot(discrecional$ind_heuris~ discrecional[[vars[i]]],
          main = titulos3[i],
          xlab = xlabels[i],
          ylab = "Nivel heuristico")
}

# Graficos nivel afrontamiento
titulos4 <- c("Nivel de afrontamiento por sexo", 
              "Nivel de afrontamiento por edad", 
              "Nivel de afrontamiento por años puesto", 
              "Nivel de afrontamiento por sueldo")

par(mfrow=c(2,2))

for (i in 1:4) {
  boxplot(discrecional$ind_afront~ discrecional[[vars[i]]],
          main = titulos4[i],
          xlab = xlabels[i],
          ylab = "Nivel de afrontamiento")
}

# Graficos nivel creatividad
titulos5 <- c("Nivel de creatividad por sexo", 
              "Nivel de creatividad por edad", 
              "Nivel de creatividad por años puesto", 
              "Nivel de creatividad por sueldo")

par(mfrow=c(2,2))

for (i in 1:4) {
  boxplot(discrecional$ind_creati~ discrecional[[vars[i]]],
          main = titulos5[i],
          xlab = xlabels[i],
          ylab = "Nivel de afrontamiento")
}
# Graficos nivel discrecionalidad
titulos6 <- c("Nivel de discrecionalidad por sexo", 
              "Nivel de discrecionalidad por edad", 
              "Nivel de discrecionalidad por años puesto", 
              "Nivel de discrecionalidad por sueldo")

par(mfrow=c(2,2))

for (i in 1:4) {
  boxplot(discrecional$ind_discrecional~ discrecional[[vars[i]]],
          main = titulos6[i],
          xlab = xlabels[i],
          ylab = "Nivel de afrontamiento")
}


variables_factor <- c("Sexo", "Edad", "Años_de_graduado","Nivel",
                      "Años_en_el_puesto","Años_en_la_AP","Sueldo")

for (var in variables_factor) {
  cat("\nResumen por:", var, "\n")

  # Usar la variable como símbolo para tidy evaluation
  resumen <- discrecional %>%
    group_by(.data[[var]]) %>%
    summarise(
      Media   = mean(ind_discrecional, na.rm = TRUE),
      Mediana = median(ind_discrecional, na.rm = TRUE),
      Q1      = quantile(ind_discrecional, 0.25, na.rm = TRUE),
      Q3      = quantile(ind_discrecional, 0.75, na.rm = TRUE),
      IQR     = IQR(ind_discrecional, na.rm = TRUE),
      SD      = sd(ind_discrecional, na.rm = TRUE),
      n       = n(),
      .groups = "drop"
    )

  print(resumen)
}
mean(discrecional$ind_discrecional)

