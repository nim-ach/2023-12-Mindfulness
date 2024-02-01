# Prepare workspace -------------------------------------------------------

## Import packages
library(data.table)
library(lme4)
library(parameters)

## Import data
data("mindfulness")

# Prepare data for analyses -----------------------------------------------

comp_data <- mindfulness[
   i = id_event_name != "2-meses" & trt_group != "Control", 
   j = .SD, 
   .SDcols = c(1,2,grep("_total$|^hrv_|bdnf_concentracion|trt", names(mindfulness)))
]

comp_data <- comp_data |> 
  melt.data.table(
    id.vars = c("id_record", "id_event_name","trt_group")
  ) |> 
  suppressWarnings()

comp_data <- comp_data[
  i = !variable %in% c("hrv_fc_max_estimada", "hrv_borg_pre", 
                       "hrv_borg_post", "hsps_total")
]


# Analyse the data --------------------------------------------------------

set.seed(1234)
models <- comp_data[
  j = lm(value ~ id_event_name) |> bootstrap_parameters(), 
  by = variable
]

models[
  i = p <= 0.05 & Parameter == "id_event_name6-meses", 
  j = list(variable, coef = round(Coefficient, 3), p = round(p, 3))
]
#>             variable   coef     p
#>1:          gds_total -3.515 0.042
#>2:         moca_total  2.845 0.008
#>3:         sppb_total  1.302 0.010
#>4: bdnf_concentracion -0.259 0.010
#> 
## Es posible que el ejercicio y el mindfulness, disminuyan el incremento en la 
## actividad simpática, inducida por el tiempo en los controles ya que al evaluar
## el efecto del tiempo sobre indicadores de SNS y Stress observamos un incremento, 
## pero al excluir los controles observamos que esta variacion inducida por el 
## tiempo se pierde, al quedar solo los grupos expuestos a ejercicio.

