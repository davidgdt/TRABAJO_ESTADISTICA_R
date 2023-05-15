#1 pivotar el data frame de formato ancho a largo o viceversa
library(tidyverse)

# Leer el DataFrame
df <- read_csv("oscars2")

# Convertir a formato largo
df_long <- df %>%
  pivot_longer(cols = c("IMDB.Rating", "Movie.Time"),
               names_to = "variable",
               values_to = "value")

# Convertir de nuevo a formato ancho
df_wide <- df_long %>%
  pivot_wider(names_from = variable,
              values_from = value)
#2 y 3 Realizar diagramas (barras histogramas , diagramas de cajas o sectores) agrupado con facetas # nolint: line_length_linter.
#Diagrama de barras con facetas:
library(ggplot2)

ggplot(df, aes(x = Movie.Genre, fill = Movie.Genre)) +
  geom_bar() +
  facet_wrap(~categorical_variable) +
  xlab("Género") +
  ylab("Número de películas") +
  ggtitle("Número de películas por género") +
  scale_fill_brewer(palette = "Set2") # usa una paleta de colores diferente


#Histograma con facetas:
ggplot(df, aes(x = IMDB.Rating)) +
  geom_histogram(bins = 30, fill = "purple", color = "black") + # Cambia el color de relleno a púrpura
  facet_wrap(~categorical_variable) +
  xlab("Calificación IMDB") +
  ylab("Número de películas") +
  ggtitle("Distribución de las calificaciones IMDB")


#Diagrama de cajas con facetas:
ggplot(df, aes(x = Award, y = IMDB.Rating, fill = Award)) +
  geom_boxplot(outlier.shape = 8, outlier.size = 2) + # Usa marcadores diferentes para los outliers
  facet_wrap(~categorical_variable) +
  xlab("Estado del premio Oscar") +
  ylab("Calificación IMDB") +
  ggtitle("Comparación de las calificaciones de IMDB entre las películas ganadoras y no ganadoras del Oscar") +
  scale_fill_brewer(palette = "Set3") # usa una paleta de colores diferente
#4comparar la distribucion observada de una variable cuantitativa discreta con su distribucion teorica a partir de sus percentiles # nolint: line_length_linter. # Supongamos que 'IMDB.Rating' es la variable cuantitativa discreta
# Primero, necesitamos instalar y cargar la librería 'ggplot2'

library(ggplot2)

# Luego, creamos el gráfico Q-Q
qqnorm(df$IMDB.Rating, main="Gráfico Q-Q para la calificación de IMDB")
qqline(df$IMDB.Rating, col="red")  # añade la línea de la distribución teórica normal
# explicacíon :
#Si la distribución de los datos es cercana a la normal, los puntos en el gráfico Q-Q seguirán aproximadamente la línea roja. Si se alejan significativamente de esta línea en alguna parte, indica que los datos no se distribuyen normalmente en esa región. # nolint: line_length_linter.

#Por favor, ten en cuenta que este es un ejemplo genérico. Tendrás que adaptarlo a tus necesidades y a tus datos. # nolint: line_length_linter.

#Además, este código asume que estás interesado en comparar tu variable con la distribución normal. Si quieres compararla con una distribución diferente, tendrás que usar una función diferente para el gráfico Q-Q. # nolint: line_length_linter.


#5 comparar la distribucion observada de una variable cuantitativa continua con su distribucion teorica a partir de sus percentiles # nolint: line_length_linter.
# Supongamos que 'Movie.Time' es la variable cuantitativa continua
# Primero, necesitamos instalar y cargar la librería 'ggplot2'

library(ggplot2)

# Luego, creamos el gráfico Q-Q
qqnorm(df$Movie.Time, main="Gráfico Q-Q para la duración de las películas")
qqline(df$Movie.Time, col="red")  # añade la línea de la distribución teórica normal

#Si la distribución de los datos es cercana a la normal, los puntos en el gráfico Q-Q seguirán aproximadamente la línea roja. Si se alejan significativamente de esta línea en alguna parte, indica que los datos no se distribuyen normalmente en esa región. # nolint: line_length_linter.

#Por favor, ten en cuenta que este es un ejemplo genérico. Tendrás que adaptarlo a tus necesidades y a tus datos. # nolint: line_length_linter.

#Además, este código asume que estás interesado en comparar tu variable con la distribución normal. Si quieres compararla con una distribución diferente, tendrás que usar una función diferente para el gráfico Q-Q. # nolint: line_length_linter.



#6 programar funciones para automatizar el analisis de datos # nolint: line_length_linter.
# Función para calcular estadísticas descriptivas
desc_stats <- function(df, column) {
  mean <- mean(df[[column]], na.rm = TRUE)
  median <- median(df[[column]], na.rm = TRUE)
  sd <- sd(df[[column]], na.rm = TRUE)
  min <- min(df[[column]], na.rm = TRUE)
  max <- max(df[[column]], na.rm = TRUE)
  
  list(mean = mean, median = median, sd = sd, min = min, max = max)
}

# Para usar la función en la columna "Movie.Time" de tu dataframe df:
desc_stats(df, "Movie.Time")
#Esta función devuelve una lista con las estadísticas descriptivas calculadas. Puedes acceder a cada estadística individualmente utilizando el nombre de la estadística como índice. Por ejemplo, desc_stats(df, "Movie.Time")$mean te dará la media de la columna "Movie.Time". # nolint: line_length_linter.

#Por favor, ten en cuenta que este es un ejemplo básico. Hay muchas otras cosas que podrías querer automatizar, dependiendo de tu análisis específico. # nolint: line_length_linter.

#7 simular un experimento aleatorio mediante la generacion de numeros aleatorios a partir de una distribucion discreta y describir su variable resultante # nolint: line_length_linter.
# Estadísticas descriptivas
mean(simulated_ratings)
sd(simulated_ratings)

# Histograma
hist(simulated_ratings, main = "Distribución de calificaciones IMDB simuladas", xlab = "Calificación IMDB")
