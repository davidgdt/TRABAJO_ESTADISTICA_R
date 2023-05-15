#crear un data frame a partir del fcihero csv limpiado llamado "oscars" con separador de campos "," y separador de decimales adecuado # nolint: line_length_linter.
library(tidyverse)
library(readr)
library(dplyr)
#1
#crear un data frame a paritr del fichero oscars2.csv con separador de campos "," y separador de decimales adecuado # nolint: line_length_linter.
df <- read_csv("oscars2.csv", col_types = cols())
df


#3 generar columnas nuevas (numericas, cadena, booleanas) a partir de las columnas existentes # nolint: line_length_linter.
#3.1 Crear una nueva columna que es la suma de IMDB.Rating y Movie.Time
df <- df %>%
  mutate(Sum_Rating_Time = IMDB.Rating + Movie.Time)
df
#3.2 Crear una nueva columna que es la concatenación de Film y Award
df <- df %>%
  mutate(Film_and_Award = paste(Film, Award, sep = " - "))
df
#3.3 Crear una nueva columna que es TRUE si IMDB.Rating es mayor que 7, y FALSE en caso contrario # nolint: line_length_linter.
df <- df %>%
  mutate(High_Rating = IMDB.Rating > 7)
df
#4 Generar nuevas columnas a partir de recodificacion de otras
#4.1Crear una nueva columna que recodifica IMDB.Rating en una variable categórica # nolint: line_length_linter.
df <- df %>%
  mutate(Rating_Category = case_when(
    IMDB.Rating >= 8 ~ "High",
    IMDB.Rating >= 6 ~ "Medium",
    TRUE ~ "Low"
  ))
  df
#4.2  Crear una nueva columna que recodifica Award en una variable binaria
df <- df %>%
  mutate(Award_Binary = case_when(
    Award == "Winner" ~ "Won",
    TRUE ~ "Not Won"
  ))
df



#5 Aplicar filtros
#5.1 Filtrar las filas donde Award es "Winner"
df_winner <- df %>%
  filter(Award == "Winner")
df_winner
#5.2 Filtrar y ver las filas donde Award es "Winner" y IMDB.Rating es mayor que 7 # nolint: line_length_linter.
df_winner_high_rating <- df %>%
  filter(Award == "Winner", IMDB.Rating > 7)
df_winner_high_rating
#5.3Filtrar y ver las filas donde Year_of_Release es menor que 1950 o mayor que 2000 # nolint: line_length_linter.
df_old_or_new <- df %>%
  filter(Year_of_Release < 1950 | Year_of_Release > 2000)
df_old_or_new
#6 tablas de frecuencias cualitativas
#6.1 una sola variable cualitativa, 'Award'
table(df$Award)
#6.2varias variables cualitativas, 'Award' y 'Movie.Genre':
table(df$Award, df$Movie.Genre)
#6.3 ACUMULADAS
# Calcular la tabla de frecuencias de 'Award'
freq <- table(df$Award)

# Calcular las frecuencias acumuladas
cum_freq <- cumsum(freq)

# Imprimir las frecuencias acumuladas
print(cum_freq)
#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
#A.1calcular los principales estadisticos de las variables cualitativas # nolint: line_length_linter.
#cuantitativas simples y  agrupando segun una o varias variables cualitativas e interpretalos # nolint: line_length_linter.
#7.1.1 IMDB.Rating
mean(df$IMDB.Rating, na.rm = TRUE)
# media=7.570403                               # nolint: commented_code_linter.
median(df$IMDB.Rating, na.rm = TRUE)
#mediana= 7.6                                  # nolint: commented_code_linter.
sd(df$IMDB.Rating, na.rm = TRUE)
#desviacion estandar=0.5596518                # nolint: commented_code_linter.
min(df$IMDB.Rating, na.rm = TRUE)
#minimo=5.6                                  # nolint: commented_code_linter.
max(df$IMDB.Rating, na.rm = TRUE)
#maximo=9.3  # nolint: commented_code_linter.
quantile(df$IMDB.Rating, na.rm = TRUE)
#cuartiles=  0%  25%  50%  75% 100%
            #5.6  7.3  7.6  7.9  9.3
#7.1.2 Movie.Time
mean(df$Movie.Time, na.rm = TRUE)
#media= 124.8949          # nolint: commented_code_linter.
median(df$Movie.Time, na.rm = TRUE)
#mediana=121               # nolint: commented_code_linter.
sd(df$Movie.Time, na.rm = TRUE)
#desviacion estandar=26.32282        # nolint: commented_code_linter.
min(df$Movie.Time, na.rm = TRUE)
#minimo=66                       # nolint: commented_code_linter.
max(df$Movie.Time, na.rm = TRUE)
#maximo=238                      # nolint: commented_code_linter.
quantile(df$Movie.Time, na.rm = TRUE)
#cuartiles=  0%   25%   50%   75%  100%
           #66.0 107.0 121.0 136.5 238.0

#AHORA PARA LAS VARIABLES CUALITATIVAS
#Para las variables cualitativas, puedes usar table() para obtener las frecuencias de cada categoría. Por ejemplo, para la variable Award: # nolint: line_length_linter.
table(df$Award)
aggregate(df$IMDB.Rating ~ df$Award, FUN = mean, na.rm = TRUE)
#Nominee       7.528870
#  Winner       7.783871
#BBBBBBBBBBBBBBBBBBBBBBBb
#realizar diagramas de barras o lineas simples agrupando segun una variable cualitativa  e interpretarlos # nolint: line_length_linter.
library(ggplot2)

# Supongamos que 'df' es tu DataFrame y 'Movie.Genre' es la columna de género de la película # nolint: line_length_linter.
df$Movie.Genre <- as.factor(df$Movie.Genre)

# Crear el diagrama de barras
# Asumiendo que 'Award' es tu columna que indica si la película es un "Ganador" o "Nominado" # nolint: line_length_linter.
ggplot(df, aes(x=Movie.Genre, fill=Award)) + 
  geom_bar() +
  xlab("Género") +
  ylab("Número de películas") +
  ggtitle("Número de películas por género") +
  labs(fill = "Estado del premio")
# interpretacion En este diagrama, cada barra representa un género de película diferente. La altura de la barra representa el número de películas en ese género. # nolint: line_length_linter.


#En este diagrama, cada barra representa un género de película diferente. La altura de la barra representa el número de películas en ese género. # nolint: line_length_linter.
#Para un diagrama de líneas que muestre la calificación promedio de IMDB por año, puedes usar el siguiente código: # nolint: line_length_linter.
# Crear el diagrama de barras
# Supongamos que 'Year_of_Release' es la columna del año de lanzamiento de la película # nolint: line_length_linter.
df$Year_of_Release <- as.numeric(df$Year_of_Release)

# Calcular la calificación promedio de IMDB por año
avg_rating <- aggregate(IMDB.Rating ~ Year_of_Release, data = df, FUN = mean)

# Crear el diagrama de líneas
ggplot(avg_rating, aes(x=Year_of_Release, y=IMDB.Rating, color="Calificación promedio de IMDB")) +
  geom_line() +
  xlab("Año de lanzamiento") +
  ylab("Calificación promedio de IMDB") +
  ggtitle("Calificación promedio de IMDB por año de lanzamiento") +
  scale_color_manual(values="black", labels="Calificación promedio de IMDB", name="")

#En este diagrama, el eje x representa el año de lanzamiento y el eje y representa la calificación promedio de IMDB.  # nolint: line_length_linter.
# La línea muestra cómo ha cambiado la calificación promedio de IMDB a lo largo del tiempo. Si la línea tiene una tendencia ascendente,        # nolint: line_length_linter.
# eso podría indicar que las películas han tendido a tener calificaciones más altas en años más recientes. Si la línea tiene una tendencia descendente, eso podría indicar que las películas han tendido a tener calificaciones más bajas en años más recientes. # nolint: line_length_linter.

#CCCCCCCCCCCCCCCCCCCCCCCCCCC
#realizar histogramas e interpretarlos # nolint: line_length_linter.
#Por ejemplo, si quisieras ver la distribución de la calificación de IMDB de las películas en tu DataFrame, podrías usar el siguiente código: # nolint: line_length_linter.
library(ggplot2)

# Crear el histograma
ggplot(df, aes(x=IMDB.Rating)) +
  geom_histogram(bins=30, fill="blue", color="black") +
  xlab("Calificación IMDB") +
  ylab("Número de películas") +
  ggtitle("Distribución de las calificaciones IMDB") +
  annotate("text", x=8, y=10, label="Las barras azules representan el número de películas", hjust=1)
#En este histograma, el eje x representa la calificación de IMDB y el eje y representa el número de películas. Cada barra representa el número de películas que tienen una calificación de IMDB dentro del rango especificado por esa barra. # nolint: line_length_linter.

#La interpretación de un histograma se basa en su forma y los valores que presenta. Si el histograma tiene una forma simétrica, como una distribución normal o gaussiana, esto indica que los valores están igualmente distribuidos alrededor de la media. Si el histograma está sesgado hacia la derecha o hacia la izquierda, esto indica que los valores tienden a ser más altos o más bajos, respectivamente. Además, cualquier pico o hueco en el histograma puede indicar la presencia de subgrupos dentro de los datos. # nolint: line_length_linter.

#Por ejemplo, si tu histograma de las calificaciones de IMDB tiene una forma simétrica, esto podría indicar que la mayoría de las películas tienen una calificación media, con pocas películas con calificaciones muy altas o muy bajas. Si el histograma está sesgado hacia la derecha, esto podría indicar que la mayoría de las películas tienen calificaciones bajas, pero hay algunas películas con calificaciones muy altas. # nolint: line_length_linter.

#DDDDDDDDDDDDDDDDDDDD
#realizar diagramas de cajas simples y agrupando segun una variable cualitativa e interpretarlos # nolint: line_length_linter.
# son una manera excelente de visualizar la distribución de una variable cuantitativa y compararla entre diferentes categorías de una variable cualitativa. En R, puedes usar la función geom_boxplot() del paquete ggplot2 para crear boxplots. # nolint: line_length_linter.

#Por ejemplo, si quisieras comparar las calificaciones de IMDB entre las películas ganadoras y no ganadoras del Oscar en tu DataFrame, podrías usar el siguiente código: # nolint: line_length_linter.
library(ggplot2)

# Crear el boxplot
ggplot(df, aes(x=Award, y=IMDB.Rating)) +
  geom_boxplot(fill="blue", color="black") +
  xlab("Estado del premio Oscar") +
  ylab("Calificación IMDB") +
  ggtitle("Comparación de las calificaciones de IMDB entre las películas ganadoras y no ganadoras del Oscar")

 #En este boxplot, el eje x representa el estado del premio Oscar (ganador o no ganador) y el eje y representa la calificación de IMDB. Cada boxplot muestra la mediana (la línea en el medio de la caja), los cuartiles Q1 y Q3 (los bordes inferior y superior de la caja, respectivamente), y los valores atípicos (los puntos por encima o por debajo de los bigotes). # nolint: line_length_linter.

#La interpretación de un boxplot se basa en la posición y la propagación de la caja y los bigotes, y la presencia de valores atípicos. Si una caja es más alta en el eje y, esto indica que las calificaciones de IMDB son más altas para ese grupo. Si una caja es más larga, esto indica que hay una mayor variabilidad en las calificaciones de IMDB para ese grupo. Los valores atípicos pueden indicar calificaciones de IMDB que son inusualmente altas o bajas. # nolint

#Por ejemplo, si el boxplot de las películas ganadoras del Oscar es más alto y tiene menos valores atípicos que el de las películas no ganadoras, esto podría sugerir que las películas ganadoras tienden a tener calificaciones de IMDB más altas y más consistentes. # nolint: line_length_linter.

#EEEEEEEEEEEEEEEEEEEE
#poner titulo a los grafuicos a los ejes y añadir una leyenda cuando sea necesaria  # nolint: line_length_linter.

#FFFFFFFFFFFFFFFFFFF
#construir modelos de regresion lineales simples y agrupando segun una o varias variables cualitativas he interpretarlos # nolint: line_length_linter.
#La regresión lineal es una técnica estadística que nos permite explorar y estudiar la relación entre dos o más características de un conjunto de datos. En R, puedes usar la función lm() para ajustar un modelo de regresión lineal a tus datos. # nolint: line_length_linter.
#n R, el paquete principal para la construcción de modelos de regresión lineal es lm() del paquete básico de estadísticas. Como ejemplo, vamos a construir un modelo de regresión lineal simple que prediga la calificación de IMDB basándose en el tiempo de película. # nolint: line_length_linter.
# Ajustar el modelo de regresión lineal
model <- lm(IMDB.Rating ~ Movie.Time, data = df)

# Resumen del modelo
summary(model)
#GGGGGGGGGGGGGGGGGGGGGGg
#construir modelos de regresion no lineales simples y agrupando segun una o varias variables cualitativas e interpretarlos # nolint: line_length_linter.
#La regresión no lineal es una forma de análisis de regresión en la que los datos observados se modelan mediante una función que es una combinación no lineal de los parámetros del modelo. Los datos se ajustan por un procedimiento de mínimos cuadrados. # nolint: line_length_linter.

#En R, puedes usar la función nls() para ajustar un modelo de regresión no lineal. Supongamos que tienes una hipótesis de que la relación entre IMDB.Rating y Movie.Time podría ser mejor modelada por una función cuadrática en lugar de una línea recta. Podrías ajustar un modelo de regresión no lineal de la siguiente manera: # nolint: line_length_linter.
# Ajustar el modelo de regresión no lineal
model_nln <- nls(IMDB.Rating ~ a * Movie.Time^2 + b * Movie.Time, data = df, start = list(a = 1, b = 1))

# Resumen del modelo
summary(model_nln)
#En este modelo, a y b son los parámetros que la función nls() intentará estimar a partir de los datos. La opción start se utiliza para proporcionar estimaciones iniciales para estos parámetros, lo cual es necesario para muchos algoritmos de optimización no lineal. # nolint: line_length_linter.

#La interpretación de un modelo de regresión no lineal puede ser un poco más complicada que la de un modelo lineal. En lugar de simplemente mirar los coeficientes y su significación estadística, a menudo es útil visualizar la línea de regresión ajustada y los datos originales en un gráfico. Esto te permitirá ver cómo se ajusta la línea de regresión a los datos. # nolint: line_length_linter.

#Si quieres ajustar modelos de regresión no lineal por grupos de una variable cualitativa, puedes hacerlo de forma similar a como lo harías con la regresión lineal, es decir, ajustando un modelo para cada subconjunto de datos. # nolint: line_length_linter.
# Dividir el dataframe por Award
df_split <- split(df, df$Award)

# Ajustar un modelo de regresión no lineal para cada subconjunto
models <- lapply(df_split, function(subset) nls(IMDB.Rating ~ a * Movie.Time^2 + b * Movie.Time, data = subset, start = list(a = 1, b = 1)))

# Ver los resúmenes de los modelos
lapply(models, summary)
#En este caso, se ajusta un modelo de regresión no lineal para cada grupo de la variable Award y luego se obtiene el resumen de cada modelo. # nolint # nolint: line_length_linter.
#HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
#comparar distintos modelos de regresion 
#Comparar diferentes modelos de regresión puede ayudarte a decidir cuál de ellos es el mejor para explicar tu variable dependiente en función de las variables independientes. Existen varias técnicas para hacer esto. # nolint: line_length_linter.
#Coeficiente de determinación (R cuadrado): Este es una medida de qué tan bien los valores predichos por el modelo de regresión se ajustan a los valores reales. Un R cuadrado de 1 indica que el modelo predice perfectamente la variable dependiente, mientras que un R cuadrado de 0 indica que el modelo no explica nada de la variabilidad en la variable dependiente. # nolint: line_length_linter.
#Prueba de la razón de verosimilitud: Esta prueba compara la verosimilitud de dos modelos, uno de los cuales (el modelo nulo) es un caso especial del otro (el modelo alternativo). La prueba de la razón de verosimilitud puede ayudarte a decidir si la adición de más variables a tu modelo mejora significativamente el ajuste del modelo. # nolint: line_length_linter.
#Criterio de información de Akaike (AIC) y Criterio de información bayesiano (BIC): Estos son criterios de selección de modelos que toman en cuenta tanto el ajuste del modelo como la complejidad del modelo (en términos del número de parámetros en el modelo). Los modelos con AIC o BIC más bajos son preferibles. # nolint: line_length_linter.
# Ajustar dos modelos de regresión
model1 <- lm(IMDB.Rating ~ Movie.Time, data = df)
model2 <- lm(IMDB.Rating ~ Movie.Time + Year_of_Release, data = df)

# Comparar los modelos
anova(model1, model2)

# Obtener AIC y BIC
AIC(model1, model2)
BIC(model1, model2)
#En este código, ajustamos dos modelos de regresión lineal: model1, que incluye Movie.Time como variable independiente, y model2, que incluye tanto Movie.Time como Year_of_Release como variables independientes. Luego, usamos la función anova() para realizar una prueba de la razón de verosimilitud y las funciones AIC() y BIC() para obtener los valores de AIC y BIC para los modelos. # nolint: line_length_linter.

#IIIIIIIIIIIIIIIIIIII
#hacer predicciones con un modelo de regresion 
#Hacer predicciones con un modelo de regresión es bastante simple en R. Primero, necesitas ajustar el modelo a tus datos. Luego, puedes usar la función predict() para hacer predicciones con el modelo. # nolint: line_length_linter.
# Ajustar un modelo de regresión lineal a los datos
model <- lm(IMDB.Rating ~ Movie.Time, data = df)

# Hacer predicciones con el modelo
predictions <- predict(model, newdata = df)

# Mostrar las predicciones
print(predictions)
#En este código, ajustamos un modelo de regresión lineal a los datos en df, utilizando IMDB.Rating como nuestra variable dependiente y Movie.Time como nuestra variable independiente. Luego, usamos la función predict() para hacer predicciones con el modelo, usando los mismos datos que usamos para ajustar el modelo. Finalmente, imprimimos las predicciones. # nolint: line_length_linter.

#Ten en cuenta que si quieres hacer predicciones para nuevos datos (es decir, datos que no utilizaste para ajustar el modelo), necesitas proporcionar estos nuevos datos a la función predict() a través del argumento newdata. Estos nuevos datos deben tener la misma estructura que los datos que utilizaste para ajustar el modelo (es decir, deben tener las mismas variables). # nolint: line_length_linter.
