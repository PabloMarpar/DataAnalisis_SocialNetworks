# Procedemos a la clusterización
head(data)

# Eliminar columnas no relevantes para el clustering aglomerativo
datos_clustering <- data[, c("age", "time_spent", "platform", "income")]

# Codificar la variable categórica "platform" como numérica
datos_clustering$platform <- as.numeric(factor(datos_clustering$platform))

# Eliminar filas con valores NA
datos_clustering <- na.omit(datos_clustering)

# Normalizar los datos
datos_norm <- scale(datos_clustering[, c("age", "time_spent", "platform")])

# Realizar clustering aglomerativo
cluster_aglomerativo <- hclust(dist(datos_norm), method = "ward.D2")

# Cortar el dendrograma para obtener los clústeres
cluster_labels <- cutree(cluster_aglomerativo, k = 3)  # Especifica el número de clústeres

# Añadir las etiquetas de clúster a los datos originales
datos_clustering$cluster <- cluster_labels

# Ver los datos clusterizados
print(datos_clustering)
#### VISUALIZACIÓN
library(ggplot2)
library(dplyr)

# Gráfico de caja para cada variable por clúster
boxplot_data <- reshape2::melt(datos_clustering, id.vars = c("cluster"))

ggplot(boxplot_data, aes(x = cluster, y = value, fill = factor(cluster))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribución de Variables por Clúster", x = "Clúster", y = "Valor") +
  theme_minimal()

