library(data.table)

data("mindfulness")
dat <- mindfulness[id_event_name == "Basal"]

names(mindfulness)

custom_mediate <- function(outcome, treat, mediator, seed = 1234) {
  dat <- mindfulness[id_event_name == "Basal", .SD, .SDcols = c(outcome, treat, mediator)]
  dat <- na.omit(dat)
  y <- dat[[outcome]]
  x <- dat[[treat]]
  m <- dat[[mediator]]
  main_model <- lm(y ~ x + m, dat)
  mod_model <- lm(m ~ x, dat)
  set.seed(seed)
  medi <- mediation::mediate(
    model.m = mod_model,
    model.y = main_model,
    boot = TRUE, boot.ci.type = "bca",
    mediator = "m", treat = "x",
    sims = 1e4
  ) 
  summary(medi)
}

# Comp corporal y HRV -----------------------------------------------------

# Acaso ejercen un efecto moderador, las variables psicológicas y de aptitud fisica, sobre la 
# relación entre la composición corporal y HRV? Observamos lo siguiente:
# -> Observamos un efecto mediador del puntaje del SPPB sobre el efecto de la grasa en la RMSSD peri ejercicio

## cc_grasa_total_porcentaje, hrv_rmssd_peri ----

custom_mediate("hrv_rmssd_peri", "cc_grasa_total_porcentaje", mediator = "gds_total") # p = 0.944
custom_mediate("hrv_rmssd_peri", "cc_masa_muscular_total", mediator = "gds_total") # p = 0.98

custom_mediate("hrv_rmssd_peri", "cc_grasa_total_porcentaje", mediator = "bai_total") # p = 0.550
custom_mediate("hrv_rmssd_peri", "cc_masa_muscular_total", mediator = "bai_total") # p = 0.36

custom_mediate("hrv_rmssd_peri", "cc_grasa_total_porcentaje", mediator = "sppb_total") # p = 0.057? <- 
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME            0.02889      0.00313         0.11   0.057 .
#> ADE             0.08762     -0.00497         0.20   0.077 .
#> Total Effect    0.11651      0.02775         0.24   0.012 *
#> Prop. Mediated  0.24800      0.07425         3.63   0.068 .
custom_mediate("hrv_rmssd_peri", "cc_masa_muscular_total", mediator = "sppb_total") # p = 0.56

custom_mediate("hrv_rmssd_peri", "cc_grasa_total_porcentaje", mediator = "tm_pasos") # p = 0.079? <- 
#>                 Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME            2.83e-02    -4.61e-05         0.07   0.079 .
#> ADE             8.82e-02    -7.70e-05         0.22   0.066 .
#> Total Effect    1.17e-01     2.78e-02         0.24   0.012 *
#> Prop. Mediated  2.43e-01     7.34e-02         4.41   0.089 .
custom_mediate("hrv_rmssd_peri", "cc_masa_muscular_total", mediator = "tm_pasos") # p = 0.60

## cc_grasa_total_porcentaje, hrv_stress_peri ----

custom_mediate("hrv_stress_peri", "cc_grasa_total_porcentaje", mediator = "gds_total") # p = 0.938
custom_mediate("hrv_stress_peri", "cc_masa_muscular_total", mediator = "gds_total") # p = 0.95

custom_mediate("hrv_stress_peri", "cc_grasa_total_porcentaje", mediator = "bai_total") # p = 0.510
custom_mediate("hrv_stress_peri", "cc_masa_muscular_total", mediator = "bai_total") # p = 0.40

custom_mediate("hrv_stress_peri", "cc_grasa_total_porcentaje", mediator = "sppb_total") # p = 0.27
custom_mediate("hrv_stress_peri", "cc_masa_muscular_total", mediator = "sppb_total") # p = 0.61

custom_mediate("hrv_stress_peri", "cc_grasa_total_porcentaje", mediator = "tm_pasos") # p = 0.554
custom_mediate("hrv_stress_peri", "cc_masa_muscular_total", mediator = "tm_pasos") # p = 0.72

## cc_grasa_total_porcentaje, hrv_sns_peri ----

custom_mediate("hrv_sns_peri", "cc_grasa_total_porcentaje", mediator = "gds_total") # p = 0.714
custom_mediate("hrv_sns_peri", "cc_masa_muscular_total", mediator = "gds_total") # p = 0.80

custom_mediate("hrv_sns_peri", "cc_grasa_total_porcentaje", mediator = "bai_total") # p = 0.308
custom_mediate("hrv_sns_peri", "cc_masa_muscular_total", mediator = "bai_total") # p = 0.18

custom_mediate("hrv_sns_peri", "cc_grasa_total_porcentaje", mediator = "sppb_total") # p = 0.041*
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME            -0.0166      -0.0494         0.00   0.041 *
#> ADE             -0.0498      -0.1073         0.01   0.077 .
#> Total Effect    -0.0664      -0.1251        -0.01   0.016 *
#> Prop. Mediated   0.2500       0.0414         3.07   0.056 .
custom_mediate("hrv_sns_peri", "cc_masa_muscular_total", mediator = "sppb_total") # p = 0.56

custom_mediate("hrv_sns_peri", "cc_grasa_total_porcentaje", mediator = "tm_pasos") # p = 0.050*
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME            -0.0148      -0.0417         0.00   0.050 *
#> ADE             -0.0516      -0.1055         0.00   0.041 *
#> Total Effect    -0.0664      -0.1251        -0.01   0.016 *
#> Prop. Mediated   0.2227       0.0272         1.39   0.061 .
custom_mediate("hrv_sns_peri", "cc_masa_muscular_total", mediator = "tm_pasos") # p = 0.59

## cc_grasa_total_porcentaje, hrv_sdnn_peri ----

custom_mediate("hrv_sdnn_peri", "cc_grasa_total_porcentaje", mediator = "gds_total") # p = 0.874
custom_mediate("hrv_sdnn_peri", "cc_masa_muscular_total", mediator = "gds_total") # p = 0.970

custom_mediate("hrv_sdnn_peri", "cc_grasa_total_porcentaje", mediator = "bai_total") # p = 0.508
custom_mediate("hrv_sdnn_peri", "cc_masa_muscular_total", mediator = "bai_total") # p = 0.402

custom_mediate("hrv_sdnn_peri", "cc_grasa_total_porcentaje", mediator = "sppb_total") # p = 0.732
custom_mediate("hrv_sdnn_peri", "cc_masa_muscular_total", mediator = "sppb_total") # p = 0.648

custom_mediate("hrv_sdnn_peri", "cc_grasa_total_porcentaje", mediator = "tm_pasos") # p = 0.302
custom_mediate("hrv_sdnn_peri", "cc_masa_muscular_total", mediator = "tm_pasos") # p = 0.618


# Rendimiento y comp corporal ---------------------------------------------


## cc_grasa_total_porcentaje, sppb_total ----

custom_mediate("sppb_total", "cc_grasa_total_porcentaje", mediator = "gds_total") # p = 0.582
custom_mediate("sppb_total", "cc_masa_muscular_total", mediator = "gds_total") # p = 0.95

custom_mediate("sppb_total", "cc_grasa_total_porcentaje", mediator = "bai_total") # p = 0.514
custom_mediate("sppb_total", "cc_masa_muscular_total", mediator = "bai_total") # p = 0.18

custom_mediate("sppb_total", "cc_grasa_total_porcentaje", mediator = "cc_masa_muscular_total") # p = 0.107
#>                 Estimate 95% CI Lower 95% CI Upper p-value   
#> ACME            0.011111    -0.000165         0.04  0.1072   
#> ADE            -0.075317    -0.123254        -0.02  0.0072 **
#> Total Effect   -0.064206    -0.110665        -0.01  0.0144 * 
#> Prop. Mediated -0.173059    -2.042215        -0.02  0.1176   
custom_mediate("sppb_total", "cc_masa_muscular_total", mediator = "cc_grasa_total_porcentaje") # p = 0.025* <- 
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME            0.02550      0.00463         0.06   0.025 *
#> ADE            -0.04215     -0.09784         0.00   0.092 .
#> Total Effect   -0.01665     -0.06979         0.03   0.433  
#> Prop. Mediated -1.53123     -0.18325      1279.72   0.453  

## cc_grasa_total_porcentaje, tm_pasos ----

custom_mediate("tm_pasos", "cc_grasa_total_porcentaje", mediator = "gds_total") # p = 0.532
custom_mediate("tm_pasos", "cc_masa_muscular_total", mediator = "gds_total") # p = 0.98

custom_mediate("tm_pasos", "cc_grasa_total_porcentaje", mediator = "bai_total") # p = 0.580
custom_mediate("tm_pasos", "cc_masa_muscular_total", mediator = "bai_total") # p = 0.77

custom_mediate("tm_pasos", "cc_grasa_total_porcentaje", mediator = "cc_masa_muscular_total") # p = 0.075?
#>                Estimate 95% CI Lower 95% CI Upper p-value   
#> ACME            0.11804      0.00469         0.35  0.0750 . 
#> ADE            -0.77301     -1.19282        -0.30  0.0014 **
#> Total Effect   -0.65497     -1.08372        -0.21  0.0042 **
#> Prop. Mediated -0.18022     -3.72334        -0.04  0.0792 . 
custom_mediate("tm_pasos", "cc_masa_muscular_total", mediator = "cc_grasa_total_porcentaje") # p = 0.019* <- 
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME             0.2617       0.0631         0.52   0.019 *
#> ADE             -0.4478      -0.9549         0.01   0.059 .
#> Total Effect    -0.1861      -0.6874         0.24   0.406  
#> Prop. Mediated  -1.4065      -1.1369       688.03   0.423  

