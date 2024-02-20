## Predicciones
head(data)
library(nnet)

# Dividir los datos en conjunto de entrenamiento y conjunto de prueba
set.seed(123)  # Para reproducibilidad
indices <- sample(1:nrow(data), 0.8 * nrow(data))
datos_entrenamiento <- data[indices, ]
datos_prueba <- data[-indices, ]

# Entrenar el modelo de regresión logística multinomial para intereses
library(nnet)
modelo_intereses <- multinom(interests ~ ., data = datos_entrenamiento)

# Realizar predicciones en el conjunto de prueba
predicciones_intereses <- predict(modelo_intereses, newdata = datos_prueba, type = "class")

# Evaluar la precisión del modelo
precision_intereses <- mean(predicciones_intereses == datos_prueba$interests)
cat("Precisión del modelo de intereses:", precision_intereses, "\n")

# Imprimir las primeras 10 predicciones
head(predicciones_intereses, 10)


# Graficos
# Gráfico de barras para las predicciones de intereses
predicciones_df <- data.frame(Intereses_Predichos = predicciones_intereses, Count = 1)
ggplot(predicciones_df, aes(x = Intereses_Predichos, fill = Intereses_Predichos)) +
  geom_bar() +
  scale_fill_manual(values = c("Sports" = "green", "Travel" = "#3b5998", "Lifestlye" = "yellow")) +  # Especificar colores
  labs(title = "Predicción de Intereses",
       x = "Intereses",
       y = "Número de Usuarios")

# Gráfico de barras para los datos originales de intereses
datos_prueba_df <- data.frame(Intereses_Reales = datos_prueba$interests, Count = 1)
ggplot(datos_prueba_df, aes(x = Intereses_Reales, fill = Intereses_Reales)) +
  geom_bar() +
  scale_fill_manual(values = c("Sports" = "green", "Travel" = "#3b5998", "Lifestlye" = "yellow")) +  # Especificar colores
  labs(title = "Datos Originales de Intereses",
       x = "Intereses",
       y = "Número de Usuarios")
