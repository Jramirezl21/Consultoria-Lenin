#Descriptivas indicadores pca
datos_filtrados <- discrecional %>% 
  filter(Sexo %in% c("Hombre", "Mujer"))

# Lista de variables numéricas
variables_numericas <- c("ind_discre_pca", "ind_dilemas_pca", "ind_heuris_pca", 
                         "ind_afront_pca", "ind_creati_pca", "ind_discrecional_pca")

# Crear los gráficos con lapply
graficos <- lapply(variables_numericas, function(var) {
  ggplot(datos_filtrados, aes(x = .data[[var]])) +
    geom_histogram(aes(y = ..density..),      # Histograma con densidad en el eje y
                   colour = "black",          # Color del borde
                   fill = "white",            # Color de relleno
                   bins = 30) +               # Número de bins
    geom_density(alpha = 0.2, fill = "#FF6666") +  # Curva de densidad
    facet_wrap(~ Sexo) +                      # Divide por la variable sexo
    labs(title = paste("Distribución de", var, "por sexo"),
         x = var,
         y = "Densidad") +
    theme_minimal()
})



# Variables categóricas (eje X)
variables_categoricas <- c("Edad", "Años_de_graduado", "Años_en_el_puesto",
                           "Años_en_la_AP")

# Variables numéricas (eje Y)
variables_numericas <- c("ind_discre_pca", "ind_dilemas_pca", "ind_heuris_pca", 
                         "ind_afront_pca", "ind_creati_pca", 
                         "ind_discrecional_pca")

# discrecional<- factor(discrecional$Edad, 
#                             levels = c("< 18 ", "18 - 25 ", "26 - 35 ",
#                                        "36 - 45", "46 - 55 ", "> 55",
#                                        "Prefiero no decir"),
#                             ordered = TRUE)
# discrecional$Edad <- factor(discrecional$Edad , levels = c("< 18 ","18 - 25 ",
#                                                            "26 - 35 ","36 - 45",
#                                                            "46 - 55 ","> 55",
#                                                            "Prefiero no decir"))
# discrecional$Años_de_graduado <- factor(discrecional$Años_de_graduado ,
#                                         levels = c("< 5 ","6 - 10",
#                                                    "11 - 15","16 - 20",
#                                                           "Prefiero no decir"))
# discrecional$Años_en_el_puesto <- factor(discrecional$Años_en_el_puesto ,
#                                         levels = c("< 5 ","6 - 10",
#                                                    "11 - 15","16 - 20","> 21",
#                                                    "Prefiero no decir"))
# discrecional$Años_en_la_AP <- factor(discrecional$Años_en_la_AP ,
#                                          levels = c("< 5 ","6 - 10",
#                                                     "11 - 15","16 - 20","> 21",
#                                                     "Prefiero no decir"))
##########################
# Función para crear gráficos con líneas de referencia generales
crear_grid_graficos <- function(var_y) {
  graficos <- lapply(variables_categoricas, function(var_x) {
    # Calcular estadísticos generales (sin agrupar por la variable categórica)
    median_val <- median(discrecional[[var_y]], na.rm = TRUE)
    q1_val <- quantile(discrecional[[var_y]], 0.25, na.rm = TRUE)
    q3_val <- quantile(discrecional[[var_y]], 0.75, na.rm = TRUE)
    
    ggplot(discrecional, aes(x = .data[[var_x]], y = .data[[var_y]])) +
      geom_boxplot(fill = "aquamarine3", alpha = 0.7) +
      theme_bw() +
      stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
      # Línea horizontal para la mediana general
      geom_hline(yintercept = median_val, linetype = "dashed", color = "blue", linewidth = 0.7) +
      # Línea horizontal para el primer cuartil general
      geom_hline(yintercept = q1_val, linetype = "dashed", color = "darkgreen", linewidth = 0.7) +
      # Línea horizontal para el tercer cuartil general
      geom_hline(yintercept = q3_val, linetype = "dashed", color = "darkgreen", linewidth = 0.7) +
      labs(x = var_x, y = var_y) +
      theme(legend.position = "none")
  })
  
  # Organizar en grid con título
  grid.arrange(grobs = graficos, ncol = 2, 
               top = paste("Relación entre", var_y, "y variables categóricas"))
}

# Aplicar la función a cada variable numérica
lapply(variables_numericas, crear_grid_graficos)



##########################
# Variables categóricas (eje X)
variables_categoricas <- c("Edad", "Años_de_graduado", "Años_en_el_puesto", "Años_en_la_AP")

# Variables numéricas (eje Y)
variable_num <- c("ind_discrecional_pca")

variables_factor <- c( "Edad", "Años_de_graduado",
                      "Años_en_el_puesto","Años_en_la_AP","Sector")

for (var in variables_factor) {
  cat("\nResumen por:", var, "\n")
  
  # Usar la variable como símbolo para tidy evaluation
  resumen <- discrecional %>%
    group_by(.data[[var]]) %>%
    summarise(
      Media   = mean(ind_discrecional_pca, na.rm = TRUE),
      Mediana = median(ind_discrecional_pca, na.rm = TRUE),
      Q1      = quantile(ind_discrecional_pca, 0.25, na.rm = TRUE),
      Q3      = quantile(ind_discrecional_pca, 0.75, na.rm = TRUE),
      IQR     = IQR(ind_discrecional_pca, na.rm = TRUE),
      SD      = sd(ind_discrecional_pca, na.rm = TRUE),
      n       = n(),
      .groups = "drop"
    )
  
  print(resumen)
}


var_num <- c("ind_discre_pca","ind_dilemas_pca","ind_heuris_pca",
             "ind_afront_pca","ind_creati_pca",
                  "ind_discrecional_pca")

for (var in var_num) {
  cat("\nResumen por:", var, "\n")
  
  # Usar la variable como símbolo para tidy evaluation
  resumen <- discrecional %>%
    summarise(
      Media   = mean(.data[[var]], na.rm = TRUE),
      Mediana = median(.data[[var]], na.rm = TRUE),
      Q1      = quantile(.data[[var]], 0.25, na.rm = TRUE),
      Q3      = quantile(.data[[var]], 0.75, na.rm = TRUE),
      IQR     = IQR(.data[[var]], na.rm = TRUE),
      SD      = sd(.data[[var]], na.rm = TRUE),
      n       = n(),
     # .groups = "drop"
    )
  
  print(resumen)
}

#Transformación de sector
table(discrecional$Sector)
# Supongamos que tu dataframe se llama 'df' y la variable de texto es 'texto'
discrecional$Sector <- ifelse(discrecional$Sector %in% c("Educación (cualquier nivel)",
                          "Gestión de riesgos / administración de emergencias",
              "Policía / Fuerzas Armadas", "TI (tecnología de la información)", 
                        "Administración de justicia", "Salud / trabajo social",
                                   "Órganos legislativos"),discrecional$Sector,"Otros")
discrecional$Sector  <- factor(discrecional$Sector)
levels(discrecional$Sector) <- c("Admin Justi", "Edu", "Ges Riesgos",
                          "Org Legis","Otros","Policia","Salud","TI")
table(discrecional$Sector)

#Transformación de profesion
table(discrecional$Profesión)
discrecional$ind_discrecional_pca

ggplot(discrecional, aes(x=Sector, y=ind_discrecional_pca)) + 
  geom_boxplot(fill = "aquamarine3", alpha = 0.7)+
  theme_bw() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  # Línea horizontal para la mediana general
  geom_hline(yintercept = 0.357, linetype = "dashed", color = "blue", linewidth = 0.7) +
  # Línea horizontal para el primer cuartil general
  geom_hline(yintercept = 0.255, linetype = "dashed", color = "darkgreen", linewidth = 0.7) +
  # Línea horizontal para el tercer cuartil general
  geom_hline(yintercept = 0.464, linetype = "dashed", color = "darkgreen", linewidth = 0.7) +
  theme(legend.position = "none")


medias <- colMeans(discrecional[, 78:83])

# Crear un dataframe con máximos y mínimos (puedes ajustarlos según tus datos)
datos_radar <- rbind(
  Max = rep(max(medias, na.rm = TRUE) * 1.1, length(medias)),  # Límite superior
  Min = rep(0, length(medias)),                                # Límite inferior (puede ser otro valor)
  Media = medias
)

nombres_variables <- colnames(datos_radar)

etiquetas <- paste(nombres_variables, "\n", round(medias, 4))

# 3. Crear el gráfico de radar con etiquetas personalizadas
radarchart(
  data.frame(datos_radar),
  axistype = 1,                # Ejes con etiquetas numéricas (porcentajes)
  pcol = "blue",               # Color de la línea de las medias
  pfcol = rgb(0, 0, 1, 0.3),   # Color de relleno (transparente)
  plwd = 2,                    # Grosor de la línea
  cglcol = "gray",             # Color de las líneas de la red
  cglty = 1,                   # Tipo de línea de la red
  axislabcol = "gray",         # Color de las etiquetas de los ejes
  title = "Medias generales por dimensión",
  vlabels = etiquetas          # Etiquetas personalizadas (nombres + medias)
)

