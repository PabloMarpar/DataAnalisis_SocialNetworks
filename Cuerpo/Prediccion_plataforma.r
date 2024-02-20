## Predicciones
head(data)
install.packages("nnet")
library(nnet)
set.seed(123)  # Para reproducibilidad
indices <- sample(1:nrow(data), 0.8 * nrow(data))
datos_entrenamiento <- data[indices, ]
datos_prueba <- data[-indices, ]

# Entrenar el modelo de regresión logística multinomial
modelo_plataforma <- multinom(platform ~ ., data = datos_entrenamiento)

# Realizar predicciones en el conjunto de prueba
predicciones_plataforma <- predict(modelo_plataforma, newdata = datos_prueba, type = "class")

# Evaluar la precisión del modelo
precision_plataforma <- mean(predicciones_plataforma == datos_prueba$platform)
cat("Precisión del modelo de plataforma:", precision_plataforma, "\n")
# Imprimir las primeras 10 predicciones
head(predicciones_plataforma, 10)


# Gráfico de barras para las predicciones
predicciones_df <- data.frame(Plataforma_Predicha = predicciones_plataforma, Count = 1)
ggplot(predicciones_df, aes(x = Plataforma_Predicha, fill = Plataforma_Predicha)) +
  geom_bar() +
  scale_fill_manual(values = c("Instagram" = "#8a3ab9", "Facebook" = "#3b5998", "YouTube" = "#ff0000")) +  # Especificar colores
  labs(title = "Predicción de Plataforma",
       x = "Plataforma",
       y = "Número de Usuarios")

# Gráfico de barras para los datos originales
datos_prueba_df <- data.frame(Plataforma_Real = datos_prueba$platform, Count = 1)
ggplot(datos_prueba_df, aes(x = Plataforma_Real, fill = Plataforma_Real)) +
  geom_bar() +
  scale_fill_manual(values = c("Instagram" = "#8a3ab9", "Facebook" = "#3b5998", "YouTube" = "#ff0000")) +  # Especificar colores
  labs(title = "Datos Originales de Plataforma",
       x = "Plataforma",
       y = "Número de Usuarios")

