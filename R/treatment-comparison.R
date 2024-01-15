###########################################################################
###########################################################################
##                                                                       ##
## > Script: treatment-comparison.R                                      ##
## > Objetivo: Valorar el efecto de la intervención sobre las            ##
##   concentraciones de BDNF basal vs 6 meses después.                   ##
## > Fecha de creación: 2024-01-14                                       ##
## > Autor: Matías Castillo-Aguilar                                      ##
##                                                                       ##
###########################################################################
###########################################################################


# Preparación del entorno de trabajo --------------------------------------

library(data.table) # Para manipular los objetos intermedios
library(parameters) # Para estimar/extraer parametros de modelos
library(lme4) # Para estimar modelos con presencia de efectos aleatorios

data("mindfulness") # Cargamos los datos


# Valoración en BDNF ------------------------------------------------------



bdnf_data <- mindfulness[
  !is.na(bdnf_concentracion), 
  bdnf_concentracion, 
  keyby = list(id_record, id_event_name, trt_group)
] 

## Modelo lineal simple, se valora el efecto del tiempo (pre a post) entre todos
## los individuos sobre las concentraciones de BDNF independiente del grupo de
## intervención e independiente de la estructura de medidas repetidas, es decir,
## un modelo simple, básico, pero de igual manera no recomendado para este tipo
## de datos.
mod_1 <- bdnf_data[, lm(bdnf_concentracion ~ id_event_name)]

set.seed(1234)
mod_1 <- bootstrap_parameters(mod_1)
#> Parameter            | Coefficient |         95% CI |      p
#> ------------------------------------------------------------
#> (Intercept)          |        0.54 | [ 0.37,  0.75] | < .001
#> id_event_name6-meses |       -0.26 | [-0.49, -0.07] | 0.008

## Modelo lineal simple para cada grupo de intervención. En ambos grupos (ejercicio y combinado)
## se observaron disminuciones del BDNF, pero al no considerar cada observación como un dato
## repetido, el error observado en cada grupo es mayor, y en consecuencia la incertidumbre e
## intervalos de confianza son más amplios también.
set.seed(1234)
mod_2 <- bdnf_data[, lm(bdnf_concentracion ~ id_event_name) |> bootstrap_parameters(), trt_group]
#> trt_group |            Parameter |  Coefficient |     CI_low |    CI_high |       p
#> ----------|----------------------|--------------|------------|------------|--------
#> Combinado |          (Intercept) |    0.4929964 |  0.2683433 | 0.75277507 | < 0.001
#>           | id_event_name6-meses |   -0.2340861 | -0.5180736 | 0.04142965 |   0.104
#> Ejercicio |          (Intercept) |    0.5990536 |  0.3608456 | 0.89460614 | < 0.001
#>           | id_event_name6-meses |   -0.2932563 | -0.6605689 | 0.03496917 |   0.074

## En el último modelo, ya considerando la estructura inherente de los datos mediante
## un modelo de efectos aleatorios de tipo jerárquico, permitimos que el nivel basal
## de BDNF fuera modelado de manera independiente para cada sujeto, estimando la misma
## pendiente para todos (a.k.a. random intercept, fixed slope model), permitiendo 
## reducir la variabilidad de las observaciones y de los grupos.
## 
## En este modelo se observó un efecto significativo del tiempo, es decir, la presencia
## del ejercicio en ambos se asoció a una disminución del BDNF en -0.31 unidades respecto
## de la medición basal, sin embargo, la presencia de mindfulness no se asoció a una
## diferencia adicinal en las concentraciones de BDNF. Al evaluar la interacción entre 
## el tiempo x grupo, no se observaron trayectorias divergentes en cuanto al efecto sobre
## el BDNF, sugiriendo que ambos grupos cambiaron en cantidades similares de pre a post.
set.seed(1234)
mod_3 <- lme4::lmer(formula = bdnf_concentracion ~ trt_group * id_event_name + (1 | id_record), 
           data = bdnf_data) |> 
  bootstrap_parameters()
#> Parameter                               | Coefficient |         95% CI |      p
#> -------------------------------------------------------------------------------
#> (Intercept)                             |        0.61 | [ 0.36,  0.86] | < .001
#> trt_groupCombinado                      |       -0.12 | [-0.43,  0.22] | 0.484 
#> id_event_name6-meses                    |       -0.31 | [-0.55, -0.03] | 0.020 
#> trt_groupCombinado:id_event_name6-meses |        0.08 | [-0.26,  0.39] | 0.642 


# Valoración en SPPB ------------------------------------------------------

sppb_data <- mindfulness[
  !is.na(sppb_total), 
  sppb_total, 
  keyby = list(id_record, id_event_name, trt_group)
] 

lme4::lmer(formula = sppb_total ~   id_event_name + (1 | id_record), 
           data = mindfulness) |> 
  parameters()
  # modelbased::estimate_contrasts(contrast = "trt_group", p_adjust = "none")



# Comparativa pre a post --------------------------------------------------

comp_data <- mindfulness[id_event_name != "2-meses" & trt_group != "Control", .SD, .SDcols = c(1,2,grep("_total$|^hrv_|bdnf_concentracion|trt", names(mindfulness)))]

comp_data <- comp_data |> 
  melt.data.table(id.vars = c("id_record", "id_event_name","trt_group"))

comp_data <- comp_data[!variable %in% c("hrv_fc_max_estimada", "hrv_borg_pre", 
                                        "hrv_borg_post", "hsps_total")]
comp_data[, list(sum(is.na(.SD)), .N), list(variable, id_event_name)]

comp_data[, lm(value ~ id_event_name) |> parameters(), variable
          ][p < 0.05 & Parameter == "id_event_name6-meses", list(variable, coef = round(Coefficient, 3), p = round(p, 3))]
#>              variable   coef     p
#> 1:         moca_total  2.985 0.001
#> 2:    hrv_stress_peri  4.361 0.048
#> 3:       hrv_sns_peri  1.067 0.040
#> 4:      hrv_sdnn_peri -2.207 0.048
#> 5:         sppb_total  1.196 0.001
#> 6: bdnf_concentracion -0.263 0.026
#> 
## Es posible que el ejercicio y el mindfulness, disminuyan el incremento en la actividad simpática, inducida por el tiempo en los controles
## Ya que al evaluar el efecto del tiempo sobre indicadores de SNS y Stress observamos un incremento, pero al excluir los controles
## observamos que esta variacion inducida por el tiempo se pierde, al quedar solo los grupos expuestos a ejercicio.
