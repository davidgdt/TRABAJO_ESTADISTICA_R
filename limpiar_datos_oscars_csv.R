#nos enfrentamos a un problema , que en la columna e IMDB_votes, los valores numericos estan entre comillas, y a la hora de crear el dataframe no procesa los datos # nolint: line_length_linter.
#entonces este codigo es para que en esa columna los valores numericos no esten entre comillas y asi poder crear el dataframe # nolint: line_length_linter.
library(readr)

# Leemos el archivo CSV
oscars <- read_csv("oscars.csv")
# Cargamos las librerías necesarias

library(dplyr)

# Leemos el archivo CSV
oscars <- read_csv("oscars.csv", col_types = cols())

# Convertimos "IMDB.Rating" a numérico
oscars <- oscars %>%
  mutate(IMDB.Rating = as.numeric(IMDB.Rating))

# Verificamos si la transformación fue exitosa
summary(oscars$IMDB.Rating)

# Guardamos el DataFrame limpio en un nuevo archivo CSV
write_csv(oscars, "oscars2.csv")
#pasamos de oscars.csv a oscars2.csv