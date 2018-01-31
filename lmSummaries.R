# Función para extraer el estadísticas relevantes de modelos lineales lm
lmSummaries <- function(mods = list()) {
  
  # Agregar chequeo clase lm
  
  # Genera vector con nombre de los modelos
  models <- as.character(lapply(mods, formula))
  
  # Extrae resumen de cada modelo
  res_mods <- lapply(mods, summary)
  
  # Extrae estadísicos a partir de los resumenes
  R2 <- sapply(res_mods, function(X) X$r.squared)

    # Extrae estadísiticos del modelo
  AIC <- sapply(mods, AIC)
  BIC <- sapply(mods, BIC)
  RSE <- sapply(mods, sigma)
  
  # RMSE
  RMSE <- sapply(mods, function(X) mean(residuals(X)^2))
  
  # Output
  data.frame(
    model = models,
    RSE = RSE,
    R2 = R2,
    AIC = AIC,
    BIC = BIC,
    RMSE = RMSE
    )
}
