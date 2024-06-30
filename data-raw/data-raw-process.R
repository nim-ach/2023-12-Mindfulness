
# Prepare workspace -------------------------------------------------------

library(data.table)

# Import dataset ----------------------------------------------------------

raw <- fread(input = "data-raw/data-raw.csv",
             col.names = readLines("data-raw/helpers/col-names.txt"))


# Variable removing or cleaning -------------------------------------------


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

#> cc_talla <- Remove outlier
raw[cc_talla == "", cc_talla := NA]
raw[cc_talla %like% "\\,", cc_talla := gsub("\\,", "\\.", cc_talla)]
raw[, cc_talla := as.numeric(cc_talla)]
raw[cc_talla < 2, cc_talla := cc_talla * 100]

#> cc_imc <- recalculate
raw[c(68,187), cc_peso_corporal := round(cc_imc * (cc_talla / 100), 1)]
raw[, cc_imc := round(cc_peso_corporal / ((cc_talla / 100) ^ 2), 2)]

#> cc_imc_categoria <- Recompute labels
raw[, cc_imc_categoria := cut(cc_imc, 
                              breaks = c(0, 18.5, 24.9, 29.9, Inf), 
                              labels = c("Infrapeso", "Normopeso", "Sobrepeso", "Obeso"))]

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

rm(text_to_number)

## Re-checking the dataset
lapply(raw[, .SD, .SDcols = sapply(raw, is.character)], unique)
#> No further missclassified variables


# Cleaning missing or conflicting observations ----------------------------

## Use the minimum registered age for each participant id
age_per_id <- raw[, list(
  sd_edad = fifelse(
    test = all(is.na(sd_edad)), # Test if all age is missing
    yes = NA_real_, # If it is, then use NA
    no = trunc(mean(sd_edad, na.rm = TRUE)) # Otherwise use the minimum of recorded age
  )), keyby = .(id_record)] # For each person
raw[, sd_edad := NULL]
mindfulness <- raw[age_per_id, on = "id_record"]
rm(age_per_id, raw)

## Correcting mislabeled sex
mindfulness[id_record == 72, sd_sexo := "Masculino"]

## Repeat assigned sex to each participant for missing rows
sex_per_id <- mindfulness[, list(sd_sexo = unique(sd_sexo)), id_record][!is.na(sd_sexo)]
mindfulness <- mindfulness[,-c("sd_sexo")][sex_per_id, on = "id_record"]
rm(sex_per_id)

## Remove completely empty observations
mindfulness <- mindfulness[id_record != 37]

## Remove empty baseline observations
mindfulness <- mindfulness[id_event_name != "Basal" | !is.na(gds_total)]


# Checking consistency ----------------------------------------------------

## Age across event name
mindfulness[, plot(sd_edad ~ as.numeric(id_record), 
                   col = id_event_name, 
                   type = "o",
                   main = "Mean age across id records",
                   ylab = "Age", xlab = "ID")]
abline(h = mindfulness$sd_edad |> mean(na.rm = TRUE), lty = 2) # mean age
mindfulness[, hist(sd_edad, breaks = 20)]

## Function to standardize variables (z-scoring)
z_score <- function(i) {
  if (!is.numeric(i)) return(i)
  (i - mean(i, na.rm = TRUE)) / sd(i, na.rm = TRUE)
}

## Check standardized numeric variables (z-scores)
std_vars <- mindfulness[, lapply(.SD, z_score), .SDcols = grepl("^id|^cc|^hrv|^cv", names(mindfulness))]

## Select those observations with 3 SD above or below the mean
ind <- suppressWarnings(which(std_vars[,-c(1:2)] > 6, arr.ind = T))

rows <- unique(ind[, 1]) 
cols <- unique(ind[, 2]) + 2

(outliers <- std_vars[rows, .SD, .SDcols = c(1,2, cols)])
#> Extreme values are observed for muscle mass and visceral fat
#> specifically id_records 43 and 68, possibly entry errors
#> Other outliers were spotted on hrv parameters for id 11

## Inspecting the values in the original scale (not standardized)
mindfulness[rows][id_record %in% outliers$id_record, .SD, .SDcols = names(outliers)]

## Changing the entry errors
mindfulness[id_record == 43 & id_event_name == "6-meses", cc_masa_muscular_pierna_derecha := cc_masa_muscular_pierna_derecha / 10]
mindfulness[id_record == 43 & id_event_name == "6-meses", cc_masa_muscular_pierna_izquierda := cc_masa_muscular_pierna_izquierda / 10]
mindfulness[id_record == 68 & id_event_name == "2-meses", cc_grasa_visceral_porcentaje := 11]

## Removing extreme outliers
### First round
mindfulness[id_record == 11 & id_event_name == "Basal", hrv_vlf_peri := NA]
mindfulness[id_record == 11 & id_event_name == "Basal", hrv_sdnn_post := NA]
mindfulness[id_record == 11 & id_event_name == "Basal", hrv_vlf_post := NA]
mindfulness[id_record == 11 & id_event_name == "Basal", hrv_lf_post := NA]
mindfulness[id_record == 29 & id_event_name == "Basal", hrv_lf_peri := NA]
mindfulness[id_record == 18 & id_event_name == "Basal", hrv_sdnn_post := NA]
mindfulness[id_record == 18 & id_event_name == "Basal", hrv_lf_post := NA]
mindfulness[id_record == 18 & id_event_name == "Basal", hrv_hf_post := NA]

### Second round of removal extreme outliers
mindfulness[id_record == 19 & id_event_name == "Basal", hrv_vlf_peri := NA]
mindfulness[id_record == 14 & id_event_name == "Basal", hrv_lf_peri := NA]
mindfulness[id_record == 48 & id_event_name == "2-meses", hrv_vlf_post := NA]

### Third and final round of removal
mindfulness[id_record == 36 & id_event_name == "Basal", hrv_vlf_peri := NA]
mindfulness[id_record == 36 & id_event_name == "2-meses", hrv_vlf_peri := NA]



### No extreme outliers with z-scores greater than 6 beyond this point
rm(ind, cols, rows, z_score, std_vars, outliers)


### Fixing age typo-error
mindfulness[sd_edad < 60, sd_edad := sd_edad + 10]

# Data export -------------------------------------------------------------

save(mindfulness, file = "data/mindfulness.RData")
fwrite(mindfulness, file = "data-raw/minduflness.csv")
