## Calcular rendimiento relativo de híbridos en unreplicated trials with checks
## 
## - Cada entrada debe estar identificada con un número (Id)
## - Cada entrada debe tener una identificación de que checks usar (Id_checks)

# Packages
library(dplyr)

# Funciones para calcular media de los checks
med_ctrl <- function(X, rend, Id) {
  ids <- as.numeric(unlist(X))
  mean(rend[Id %in% ids])
}

# Funciones para calcular sd de los checks
sd_ctrl <- function(X, rend, Id) {
  ids <- as.numeric(unlist(X))
  sd(rend[Id %in% ids])
}

# Aplica funciones
datos <- datos %>%
  group_by(Localidad) %>%                                           # si es es un MET
  mutate(ids = strsplit(Id_checks, split = ","),                    # Crea una lista con ids para usar en sapply
         rend_ctrl = sapply(ids, med_ctrl, rend = rend, Id = Id),
         sd_ctrl = sapply(ids, sd_ctrl, rend = rend, Id = Id)) %>%
  ungroup() %>%
  mutate(rend_dif = rend - rend_ctrl, rend_std = rend_dif/sd_ctrl)