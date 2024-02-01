
# Prepare workspace -------------------------------------------------------

## Import packages
library(data.table)
library(lme4)
library(parameters)

## Import data
data("mindfulness")


# Prepare data for analyses -----------------------------------------------

hrv_data <- mindfulness[
  ## Transformamos variables de HRV a formato largo
  j = melt.data.table(
    data = .SD, 
    id.vars = c("id_record", "id_event_name", "trt_group"),
    na.rm = TRUE) |> suppressWarnings(),
  .SDcols = grep("^id|^hrv_|^trt", names(mindfulness), value = TRUE)
]

hrv_data[ ## Creamos una variable de Pre y Post para poder calcular los Deltas
  j = `:=`(
    Time = fcase(
      grepl("pre$", variable), "Pre",
      grepl("post$", variable),  "Post"
    ),
    variable = gsub("_pre$|_post$", "", variable)
  )
]

hrv_data <- hrv_data[ ## Separamos en columnas los Pre y Post
  i = !is.na(Time),
  j = dcast(
    data = .SD,
    formula = id_record + id_event_name + trt_group + variable ~ Time,
    value.var = "value"
  )
]

hrv_data[ ## Calculamos los Deltas
  j = `:=`(
    Delta = Post - Pre
  )
][]


# Analyse trt_group influence on delta ------------------------------------

set.seed(1234)
models <- hrv_data[
  j = lmer(Delta ~ trt_group + (1 | id_event_name) + (1 | id_record)) |> 
    bootstrap_parameters(), 
  by = variable
]

models[, `:=`(
  p = round(p, 3),
  Coefficient = round(Coefficient, 3),
  CI_low = round(CI_low, 3),
  CI_high = round(CI_high, 3)
)][]
#>       variable          Parameter Coefficient  CI_low CI_high     p
#> 1:      hrv_hf        (Intercept)      -4.092 -33.594  25.510 0.784
#> 2:      hrv_hf trt_groupEjercicio       7.212 -36.771  48.160 0.778
#> 3:      hrv_hf trt_groupCombinado      -4.016 -34.770  29.161 0.828
#> 4:      hrv_lf        (Intercept)      25.470 -18.130  72.544 0.278
#> 5:      hrv_lf trt_groupEjercicio       4.346 -79.353  85.351 0.918
#> 6:      hrv_lf trt_groupCombinado      14.053 -55.467  79.340 0.694
#> 7: hrv_mean_hr        (Intercept)       4.854   3.179   6.664 0.000 ***
#> 8: hrv_mean_hr trt_groupEjercicio       1.138  -2.313   4.327 0.542
#> 9: hrv_mean_hr trt_groupCombinado      -0.672  -3.663   2.050 0.652
#> 10: hrv_mean_rr        (Intercept)     -54.962 -72.673 -37.944 0.000 ***
#> 11: hrv_mean_rr trt_groupEjercicio      -1.947 -37.334  33.516 0.910
#> 12: hrv_mean_rr trt_groupCombinado       5.029 -21.259  33.328 0.710
#> 13:     hrv_pns        (Intercept)      -0.305  -0.419  -0.182 0.000 ***
#> 14:     hrv_pns trt_groupEjercicio       0.072  -0.136   0.287 0.538
#> 15:     hrv_pns trt_groupCombinado       0.097  -0.070   0.264 0.244
#> 16:   hrv_rmssd        (Intercept)      -0.248  -2.236   1.544 0.800
#> 17:   hrv_rmssd trt_groupEjercicio       1.450  -1.540   4.511 0.348
#> 18:   hrv_rmssd trt_groupCombinado       0.519  -1.990   2.779 0.732
#> 19:      hrv_sd        (Intercept)      -2.985  -4.974  -1.165 0.002 **
#> 20:      hrv_sd trt_groupEjercicio       3.577   0.253   6.346 0.034 *
#> 21:      hrv_sd trt_groupCombinado       2.643   0.343   5.062 0.026 *
#> 22:    hrv_sdnn        (Intercept)       1.209  -0.302   2.567 0.098
#> 23:    hrv_sdnn trt_groupEjercicio       0.197  -2.107   2.639 0.864
#> 24:    hrv_sdnn trt_groupCombinado      -0.563  -2.452   1.286 0.538
#> 25:     hrv_sns        (Intercept)       0.387  -0.088   0.827 0.134
#> 26:     hrv_sns trt_groupEjercicio      -0.061  -0.622   0.527 0.824
#> 27:     hrv_sns trt_groupCombinado      -0.165  -0.588   0.323 0.458
#> 28:  hrv_stress        (Intercept)      -0.217  -2.412   1.884 0.856
#> 29:  hrv_stress trt_groupEjercicio      -0.301  -2.807   2.212 0.830
#> 30:  hrv_stress trt_groupCombinado      -0.221  -2.467   1.969 0.856
#> 31:     hrv_vlf        (Intercept)       2.716 -10.298  16.011 0.680
#> 32:     hrv_vlf trt_groupEjercicio       4.389 -20.359  29.171 0.704
#> 33:     hrv_vlf trt_groupCombinado     -13.535 -34.274   6.624 0.184
#>        variable          Parameter Coefficient  CI_low CI_high     p

models[Parameter != "(Intercept)" & p < 0.05]
#>    variable          Parameter Coefficient CI_low CI_high     p
#> 1:   hrv_sd trt_groupEjercicio       3.577  0.253   6.346 0.034
#> 2:   hrv_sd trt_groupCombinado       2.643  0.343   5.062 0.026

