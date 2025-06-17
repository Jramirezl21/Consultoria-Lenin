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
variables_categoricas <- c("Edad", "Años_de_graduado", "Años_en_el_puesto", "Años_en_la_AP")

# Variables numéricas (eje Y)
variables_numericas <- c("ind_discre_pca", "ind_dilemas_pca", "ind_heuris_pca", 
                         "ind_afront_pca", "ind_creati_pca", 
                         "ind_discrecional_pca")

# Función para crear gráficos para una variable numérica específica
crear_grid_graficso <- function(var_y) {
  graficos <- lapply(variables_categoricas, function(var_x) {
    ggplot(discrecional, aes(x = .data[[var_x]], y = .data[[var_y]], fill = .data[[var_x]])) +
      geom_boxplot(alpha = 0.7) +
      theme_bw() +
      stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
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

variables_factor <- c("Sexo", "Edad", "Años_de_graduado","Nivel",
                      "Años_en_el_puesto","Años_en_la_AP","Sueldo")

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
