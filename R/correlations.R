###########################################################################
###########################################################################
##                                                                       ##
## > Script: correlations.R                                              ##
## > Objetivo: establecer correlaciones entre variables numéricas        ##
## > Fecha de creación: 2024-01-11                                       ##
## > Autor: Matías Castillo-Aguilar                                      ##
##                                                                       ##
###########################################################################
###########################################################################


# Preparación del entorno de trabajo --------------------------------------

library(correlation) # Para realizar correlaciones
library(data.table) # Para manipular los objetos intermedios

data("mindfulness") # Cargamos los datos

## Función auxiliar
all_tag_pairs <- function() {
  ## Esta función entrega entrega los índices con todos los pares
  ## de variables que no son del mismo tipo. Por ejemplo: no te va
  ## a entregar filas con correlaciones que ambas sean de variables
  ## que comiencen con hrv_* y hrv_*, mostrando solo las correlaciones
  ## en donde las variables sean diferentes.
  
  ## Accede a las variables del data.table
  env <- parent.frame()
  ## Asigna los nombres de los parámetros a objetos
  par1 <- env$Parameter1
  par2 <- env$Parameter2
  ## Recupera todos los nombres de variables únicos
  all_ps <- c(unique(par1), unique(par2))
  ## Obtén los tags (e.g., "hrv_", "cc_", etc) de inicio únicos
  tags <- unique(
    x = regmatches(
      x = all_ps,
      m = regexpr(pattern = "[a-z]+_",
                  text = all_ps)
    )
  )
  ## Combina todos esos tags en parejas de a dos que no se repitan
  cmb_tags <- combn(tags, 2, simplify = FALSE)
  ## Genera una búqueda lógica de esos pares de tags en las filas
  ## de los datos de correlaciones originales
  out <- lapply(cmb_tags, function(i) {
    text1 <- paste0("^", i[1])
    text2 <- paste0("^", i[2])
    (grepl(text1, par1) & grepl(text2, par2)) | (grepl(text1, par1) & grepl(text2, par2))
  })
  out <- as.data.table(out)
  apply(out, 1, any)
}


# Pearson r ---------------------------------------------------------------

## Primero realizaremos correlaciones entre los sujetos evaluados basalmente
## para explorar las relaciones latentes entre variables
cor_1 <- correlation(
  data = mindfulness[
    id_event_name == "Basal", 
    j = .SD, 
    .SDcols = grepl(
      pattern = "^cc|^cv|^hrv|total$|tm_pasos", 
      x = names(mindfulness)
    )
  ],
  p_adjust = "none", 
  method = "pearson"
)

cor_1 <- as.data.table(cor_1)

results_cor_1 <- cor_1[all_tag_pairs() & p < 0.05
                       ][, list(Parameter1, Parameter2, r, p, n_Obs)]

results_cor_1[order(Parameter1), list(
  paste0(Parameter1, ", ", Parameter2, ", r = ", round(r, 3), ", p = ", round(p, 3))
)][, paste0(V1, collapse = "\n")] |> cat()

