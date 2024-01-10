
# Prepare workspace -------------------------------------------------------

library(data.table)

# Import dataset ----------------------------------------------------------

raw <- fread(input = "data-raw/data-raw.csv",
             col.names = readLines("data-raw/helpers/col-names.txt"))

# Data wrangling ----------------------------------------------------------

## Removing all variables starting with "rm_" as a flag to eliminate those
## variables
raw <- raw[, .SD, .SDcols = grep("rm_", names(raw), value = TRUE, invert = TRUE)]

## Add intervention groups using external lookup table and transform into a
## factor variable
groups <- fread("data-raw/helpers/intervention_groups.csv")
raw <- raw[groups, on = "id_record"]
raw[, trt_group := factor(trt_group, levels = c("Control", "Ejercicio", "Combinado"))]
rm(groups)

## Checking unique values for string variables - searching for entry errors
lapply(raw[, .SD, .SDcols = sapply(raw, is.character)], unique)

raw[, `:=`(
#> id_event_name <- transform to factor
  id_event_name = factor(id_event_name, 
                         levels = c("Fase 0", "Fase 2", "Fase 6"),
                         labels = c("Basal", "2-meses", "6-meses")),
#> id_record <- transform to anonymous ids
  id_record = factor(rleid(id_record))
)]

#> sd_sexo <- change to factor of two levels
raw[, sd_sexo := factor(sd_sexo, levels = c("Femenino", "Masculino"))]

#> sd_presenta_enfermedad_fisica_metabolica <- try to transform to a factor
raw[, `:=`(
  cm_hipertension = grepl("HTA|hipertens", sd_presenta_enfermedad_fisica_metabolica, ignore.case = TRUE),
  cm_dislipidemia = grepl("dislipid|colesterol", sd_presenta_enfermedad_fisica_metabolica, ignore.case = TRUE),
  cm_diabetes = grepl("DM|diabet", sd_presenta_enfermedad_fisica_metabolica, ignore.case = TRUE),
  cm_alguna = grepl(".", sd_presenta_enfermedad_fisica_metabolica, ignore.case = TRUE),
  sd_presenta_enfermedad_fisica_metabolica = NULL
)]

raw[, `:=`(
  cm_hipertension = factor(cm_hipertension, levels = c(T, F), labels = c("Sí", "No")),
  cm_dislipidemia = factor(cm_dislipidemia, levels = c(T, F), labels = c("Sí", "No")),
  cm_diabetes = factor(cm_diabetes, levels = c(T, F), labels = c("Sí", "No")),
  cm_alguna = factor(cm_alguna, levels = c(T, F), labels = c("Sí", "No"))
)]
 
#> sd_presenta_enfermedad_psicologica <- same as with previous
raw[, `:=`(
  cm_depresion = grepl("depres", sd_presenta_enfermedad_psicologica, ignore.case = TRUE),
  cm_ansiedad = grepl("ansied", sd_presenta_enfermedad_psicologica, ignore.case = TRUE),
  sd_presenta_enfermedad_psicologica = NULL
)]

raw[, `:=`(
  cm_depresion = factor(cm_depresion, levels = c(T, F), labels = c("Sí", "No")),
  cm_ansiedad = factor(cm_ansiedad, levels = c(T, F), labels = c("Sí", "No"))
)]

#> gds_q1 to gds_q30 <- standardize as factors of two levels
ind <- grep("gds_q", names(raw), value = TRUE)
raw[, (ind) := lapply(.SD, function(i) {
  i <- gsub("si|sí", "Sí", i, ignore.case = TRUE)
  i <- gsub("no", "No", i, ignore.case = TRUE)
  factor(i, levels = c("Sí", "No"))
}), .SDcols = ind]
rm(ind)

#> sppb_categoria <- standardize as factor
raw[, sppb_categoria := factor(sppb_categoria, 
                               levels = c("Limitación mínima o sin limitación (AUTÓNOMO)", "Limitación leve (Frágil-Prefrágil)", "Limitación moderada (Frágil)"),
                               labels = c("Autónomo", "Pre-Frágil", "Frágil"))]

#> spaq_patron_verano/invierno <- standardize as factors of two levels
raw[, spaq_patron_verano := factor(spaq_patron_verano, levels = c("Sí", "No"))]
raw[, spaq_patron_invierno := factor(spaq_patron_invierno, levels = c("Sí", "No"))]

#> spaq_patron_estacional <- standardize as factors of four levels
raw[, spaq_patron_estacional := factor(spaq_patron_estacional, 
                                       levels = c("Ausencia", "Ambos", 
                                                  "Patrón verano",
                                                  "Patrón invierno"))]

#> spaq_ssi <- standardize as factor of three levels
raw[, spaq_ssi := factor(spaq_ssi, levels = c("Puntaje promedio", "Winter blues", "SAD"))]

#> spaq_severidad <- standardize as ordered factor
raw[, spaq_severidad := factor(spaq_severidad, 
                               levels = c("No es problema", "Leve", "Moderado", 
                                          "Importante", "Severo"))]

#> bai_categoria <- standardize as ordered factor
raw[, bai_categoria := factor(bai_categoria, levels = c("Muy baja", "Moderada", "Severa"))]

#> katz_q1 to katz_q6 <- standardize as factor of two levels
ind <- grep("katz_q", names(raw), ignore.case = TRUE, value = TRUE)
raw[, (ind) := lapply(.SD, function(i) {
  factor(i, levels = c("Independiente", "Dependiente"))
}), .SDcols = ind]
rm(ind)

#> katz_categoria <- standardize as ordered factor
raw[, katz_categoria := factor(katz_categoria, 
                               levels = c("A (Independiente en todas las funciones)", "B (Independiente en todas las funciones menos en una cualquiera)", 
                                          "C (Independiente en todas las funciones menos en el baño y en otra una cualquiera)"),
                               labels = c("Independiente", "Excepto en una función", "Excepto en dos funciones"))]

#> tm_categoria <- standardize as factor
raw[, tm_categoria := factor(tm_categoria, 
                             levels = c("BAJO RANGO", "NORMAL", "ALTO RANGO"),
                             labels = c("Bajo", "Normal", "Alto"))]

#> cc_imc_categoria <- standardize as ordered factor
raw[, cc_imc_categoria := factor(cc_imc_categoria, 
                                 levels = c("Peso normal", "Sobrepeso", "Obeso"),
                                 ordered = TRUE)]

## Function to change missformated numbers into numeric format
text_to_number <- function(x, remove_dash = TRUE) {
  x[grepl("[A-Za-z]", x) | nchar(x) == 0] <- NA
  x <- gsub("\\,", "\\.", x)
  if (remove_dash) {
    x <- gsub("\\-|\\*", "", x)
  }
  x <- as.numeric(x)
}

#> cc_grasa_total_porcentaje <- change into number, remove text and commas
raw[, cc_grasa_total_porcentaje := text_to_number(cc_grasa_total_porcentaje)]

#> cc_grasa_pierna_izquierda_porcentaje <- change into number and remove commas
raw[, cc_grasa_pierna_izquierda_porcentaje := text_to_number(cc_grasa_pierna_izquierda_porcentaje)]

#> cc_grasa_torso_porcentaje <- change to number
raw[, cc_grasa_torso_porcentaje := text_to_number(cc_grasa_torso_porcentaje)]

#> cc_masa_muscular_brazo_derecho <- change into number and remove commas
raw[, cc_masa_muscular_brazo_derecho := text_to_number(cc_masa_muscular_brazo_derecho)]

#> cc_peso_corporal <- change to number
raw[, cc_peso_corporal := text_to_number(cc_peso_corporal)]

#> cc_masa_osea_porcentaje <- change to number
raw[, cc_masa_osea_porcentaje := text_to_number(cc_masa_osea_porcentaje)]

#> cc_edad_calculada <- change to number
raw[, cc_edad_calculada := text_to_number(cc_edad_calculada)]

#> hrv_pns_pre <- change to number
raw[, hrv_pns_pre := text_to_number(hrv_pns_pre, remove_dash = FALSE)]

#> hrv_sns_pre <- change to number
raw[, hrv_sns_pre := text_to_number(hrv_sns_pre, remove_dash = FALSE)]

#> hrv_rmssd_peri <- change to number
raw[, hrv_rmssd_peri := gsub("\\-", "\\.", hrv_rmssd_peri)]
raw[, hrv_rmssd_peri := text_to_number(hrv_rmssd_peri, remove_dash = FALSE)]

#> hrv_pns_peri <- change to number
raw[, hrv_pns_peri := text_to_number(hrv_pns_peri, remove_dash = FALSE)]

#> hrv_pns_post <- change to number
raw[, hrv_pns_post := text_to_number(hrv_pns_post, remove_dash = FALSE)]

#> hrv_sns_post <- change to number
raw[, hrv_sns_post := text_to_number(hrv_sns_post, remove_dash = FALSE)]

#> bdnf_concentracion <- change to number
raw[, bdnf_concentracion := text_to_number(bdnf_concentracion, remove_dash = FALSE)]

mindfulness <- raw

# Data export -------------------------------------------------------------

save(mindfulness, file = "data/mindfulness.RData")
fwrite(mindfulness, file = "data-raw/minduflness.csv")
