# Somos una empresa de viajes y queremos ver en que mercado será más fructifero ivertir y publicitarse, 
# queremos hacer una campaña que vaya directamente a nuestros posibles consumidores.

library(ggplot2)
library(dplyr)
library(gridExtra)  
data_raw=read.csv('dummy_data.csv')
head(data_raw)

# Primero limpiamos los datos
data <- na.omit(data_raw)

# Primera visualizacion de los datos
# Definir los grupos de edad
data$age_group <- cut(data$age, breaks = c(14, 21, 31, 50, 70), labels = c("Adolescentes", "Jóvenes", "Adultos", "Adultos mayores"))
# Calcular la media del tiempo invertido por grupo de edad y plataforma
resumen <- data %>%
  group_by(gender, age_group, platform) %>%
  summarise(mean_time_spent = mean(time_spent))
head(data)
# Crear un gráfico para cada sexo
# Filtrar los datos por género y crear un gráfico para cada grupo
datos_male <- resumen[resumen$gender == "male", ]
grafico_male <- ggplot(datos_male, aes(x = age_group, y = mean_time_spent, size = mean_time_spent, color = platform)) +
  geom_point(position = position_dodge(width = 0.5), alpha = 1) +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_manual(values = c("Facebook" = "blue", "Instagram" = "purple", "YouTube" = "red")) +
  labs(title = "Género: Male", x = "Grupo de edad", y = "Tiempo promedio invertido", size = "Tiempo promedio invertido", color = "Plataforma") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

datos_female <- resumen[resumen$gender == "female", ]
grafico_female <- ggplot(datos_female, aes(x = age_group, y = mean_time_spent, size = mean_time_spent, color = platform)) +
  geom_point(position = position_dodge(width = 0.5), alpha = 1) +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_manual(values = c("Facebook" = "blue", "Instagram" = "purple", "YouTube" = "red")) +
  labs(title = "Género: Female", x = "Grupo de edad", y = "Tiempo promedio invertido", size = "Tiempo promedio invertido", color = "Plataforma") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

datos_non_binary <- resumen[resumen$gender == "non-binary", ]
grafico_non_binary <- ggplot(datos_non_binary, aes(x = age_group, y = mean_time_spent, size = mean_time_spent, color = platform)) +
  geom_point(position = position_dodge(width = 0.5), alpha = 1) +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_manual(values = c("Facebook" = "blue", "Instagram" = "purple", "YouTube" = "red")) +
  labs(title = "Género: Non-binary", x = "Grupo de edad", y = "Tiempo promedio invertido", size = "Tiempo promedio invertido", color = "Plataforma") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostrar los gráficos
print(grafico_male)
print(grafico_female)
print(grafico_non_binary)

# Gráfico Relación plataforma e intereses

colores_vistosos <- c("#FF5733", "#FFC300", "#DAF7A6", "#FF5733", "#C70039", "#900C3F", "#581845")

# Gráfico de intereses por plataforma
grafico <- ggplot(data, aes(x = platform, fill = interests)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Intereses por Plataforma", x = "Plataforma", y = "Número de Usuarios") +
  scale_fill_manual(values = colores_vistosos) +  # Personalizar colores
  theme_minimal()

# Mostrar el gráfico
print(grafico)

# Gráfico de intereses por género
grafico_intereses_genero <- ggplot(data, aes(x = gender, fill = interests)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Intereses por Género", x = "Género", y = "Número de Usuarios") +
  scale_fill_manual(values = colores_vistosos) +  # Personalizar colores
  theme_minimal()

# Mostrar el gráfico
print(grafico_intereses_genero)


# Gráfico de intereses por país
grafico_intereses_pais <- ggplot(data, aes(x = location, fill = interests)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Intereses por País", x = "País", y = "Número de Usuarios") +
  scale_fill_manual(values = colores_vistosos) +  # Utiliza la paleta de colores vistosos definida anteriormente
  theme_minimal()

# Mostrar el gráfico
print(grafico_intereses_pais)

data <- data[!is.na(data$income), ]

# Crear los grupos de ingresos
data$income_group <- cut(data$income, 
                         breaks = c(11000, 13000, 15000, 17000, Inf),  # Usar Inf para indicar infinito
                         labels = c("0-11000", "11000-13000", "13000-15000", "15000-17000", "17000+"))

# Gráfico de barras de intereses e ingresos
grafico_intereses_income <- ggplot(data, aes(x = income_group, fill = interests)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Intereses por Grupo de Ingresos", x = "Grupo de Ingresos", y = "Número de Usuarios") +
  scale_fill_manual(values = colores_vistosos) +  # Utiliza la paleta de colores vistosos definida anteriormente
  theme_minimal()

# Mostrar el gráfico
print(grafico_intereses_income)




