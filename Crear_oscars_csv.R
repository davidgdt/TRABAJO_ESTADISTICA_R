#preprocesamos el csv y seleccionamos las columnas que nos interesan
library(dplyr)

# Leemos el archivo CSV
movies <- read.csv("movies.csv")

# Seleccionamos las columnas deseadas
movies_select <- select(movies, Film, Movie.Genre, IMDB.Rating, Audience.Count, Movie.Time)

# Escribimos el nuevo dataframe en un nuevo archivo CSV
write.csv(movies_select, "oscars.csv", row.names = FALSE)
#pasamos de movies.csv a oscars.csv