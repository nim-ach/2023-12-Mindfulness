library(data.table)

data("mindfulness")
dat <- mindfulness[id_event_name == "Basal"]

names(mindfulness)

custom_mediate <- function(outcome, treat, mediator) {
  dat <- mindfulness[id_event_name == "Basal", .SD, .SDcols = c(outcome, treat, mediator)]
  dat <- na.omit(dat)
  y <- dat[[outcome]]
  x <- dat[[treat]]
  m <- dat[[mediator]]
  main_model <- lm(y ~ x + m, dat)
  mod_model <- lm(m ~ x, dat)
  medi <- mediation::mediate(
    model.m = mod_model,
    model.y = main_model,
    boot = TRUE, boot.ci.type = "bca",
    mediator = "m", treat = "x"
  ) 
  summary(medi)
}

# Comp corporal y HRV -----------------------------------------------------


## cc_grasa_total_porcentaje, hrv_rmssd_peri ----

custom_mediate("hrv_rmssd_peri", "cc_grasa_total_porcentaje", mediator = "gds_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value   
#> ACME           -0.00280     -0.04114         0.03   0.950   
#> ADE            -0.29943     -0.53900        -0.11   0.004 **
#> Total Effect   -0.30224     -0.53198        -0.10   0.004 **
#> Prop. Mediated  0.00927     -0.12467         0.12   0.950   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74

custom_mediate("hrv_rmssd_peri", "cc_grasa_total_porcentaje", mediator = "bai_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME            0.00954     -0.02657         0.05   0.626  
#> ADE             0.10697      0.00410         0.23   0.046 *
#> Total Effect    0.11651      0.02822         0.23   0.014 *
#> Prop. Mediated  0.08187     -0.20165         1.39   0.640  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74 

custom_mediate("hrv_rmssd_peri", "cc_grasa_total_porcentaje", mediator = "moca_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME            0.00465     -0.00225         0.05   0.508  
#> ADE             0.11186      0.01881         0.23   0.026 *
#> Total Effect    0.11651      0.02714         0.25   0.024 *
#> Prop. Mediated  0.03994     -0.02348         1.15   0.524  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74 

custom_mediate("hrv_rmssd_peri", "cc_grasa_total_porcentaje", mediator = "sppb_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME            0.02889      0.00223         0.10   0.060 .
#> ADE             0.08762     -0.01681         0.20   0.098 .
#> Total Effect    0.11651      0.01992         0.24   0.022 *
#> Prop. Mediated  0.24800      0.06327         4.63   0.078 .
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74 

## cc_grasa_total_porcentaje, hrv_stress_peri ----

custom_mediate("hrv_stress_peri", "cc_grasa_total_porcentaje", mediator = "gds_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value   
#> ACME           -0.00280     -0.03200         0.04   0.956   
#> ADE            -0.29943     -0.54897        -0.09   0.008 **
#> Total Effect   -0.30224     -0.53260        -0.08   0.008 **
#> Prop. Mediated  0.00927     -0.16700         0.10   0.948   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74

custom_mediate("hrv_stress_peri", "cc_grasa_total_porcentaje", mediator = "bai_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value   
#> ACME            -0.0260      -0.1267         0.07   0.508   
#> ADE             -0.2763      -0.5381        -0.09   0.008 **
#> Total Effect    -0.3022      -0.5544        -0.12   0.006 **
#> Prop. Mediated   0.0859      -0.3285         0.46   0.510   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74 

custom_mediate("hrv_stress_peri", "cc_grasa_total_porcentaje", mediator = "moca_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value   
#> ACME            -0.0203      -0.0937         0.01   0.376   
#> ADE             -0.2819      -0.5251        -0.09   0.010 **
#> Total Effect    -0.3022      -0.5562        -0.12   0.008 **
#> Prop. Mediated   0.0672      -0.0301         0.46   0.372   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74 

custom_mediate("hrv_stress_peri", "cc_grasa_total_porcentaje", mediator = "sppb_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value   
#> ACME            -0.0357      -0.1406         0.02   0.282   
#> ADE             -0.2665      -0.4993        -0.04   0.014 * 
#> Total Effect    -0.3022      -0.5404        -0.11   0.002 **
#> Prop. Mediated   0.1182      -0.0432         0.97   0.284   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74 


## cc_grasa_total_porcentaje, hrv_sns_peri ----

custom_mediate("hrv_sns_peri", "cc_grasa_total_porcentaje", mediator = "gds_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME           -0.00114     -0.01672         0.00   0.742  
#> ADE            -0.06521     -0.12351        -0.01   0.014 *
#> Total Effect   -0.06635     -0.12731        -0.02   0.014 *
#> Prop. Mediated  0.01714     -0.07410         0.40   0.744  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74 

custom_mediate("hrv_sns_peri", "cc_grasa_total_porcentaje", mediator = "bai_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME           -0.00893     -0.03024         0.01   0.306  
#> ADE            -0.05742     -0.11663         0.00   0.054 .
#> Total Effect   -0.06635     -0.12751        -0.01   0.026 *
#> Prop. Mediated  0.13456     -0.08259         1.55   0.328  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74 

custom_mediate("hrv_sns_peri", "cc_grasa_total_porcentaje", mediator = "moca_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value   
#> ACME           -0.00463     -0.02264         0.00   0.360   
#> ADE            -0.06172     -0.12208        -0.01   0.004 **
#> Total Effect   -0.06635     -0.12754        -0.02   0.006 **
#> Prop. Mediated  0.06977     -0.07570         0.36   0.362   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74

custom_mediate("hrv_sns_peri", "cc_grasa_total_porcentaje", mediator = "sppb_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME            -0.0166      -0.0476         0.00   0.038 *
#> ADE             -0.0498      -0.1152         0.01   0.104  
#> Total Effect    -0.0664      -0.1325        -0.01   0.022 *
#> Prop. Mediated   0.2500       0.1029         7.37   0.060 .
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74 

## cc_grasa_total_porcentaje, hrv_sdnn_peri ----

custom_mediate("hrv_sdnn_peri", "cc_grasa_total_porcentaje", mediator = "gds_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                 Estimate 95% CI Lower 95% CI Upper p-value   
#> ACME           -0.000212    -0.031764         0.00   0.828   
#> ADE             0.181302     0.078668         0.35   0.002 **
#> Total Effect    0.181090     0.076771         0.34   0.002 **
#> Prop. Mediated -0.001170    -0.148191         0.05   0.830   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74

custom_mediate("hrv_sdnn_peri", "cc_grasa_total_porcentaje", mediator = "bai_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value    
#> ACME             0.0115      -0.0291         0.05    0.55    
#> ADE              0.1696       0.0607         0.31  <2e-16 ***
#> Total Effect     0.1811       0.0805         0.32  <2e-16 ***
#> Prop. Mediated   0.0636      -0.1686         0.37    0.55    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74 

custom_mediate("hrv_sdnn_peri", "cc_grasa_total_porcentaje", mediator = "moca_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value    
#> ACME            0.01011     -0.00343         0.07    0.43    
#> ADE             0.17098      0.06874         0.33  <2e-16 ***
#> Total Effect    0.18109      0.07932         0.34  <2e-16 ***
#> Prop. Mediated  0.05585     -0.02268         0.47    0.43    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74 

custom_mediate("hrv_sdnn_peri", "cc_grasa_total_porcentaje", mediator = "sppb_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value    
#> ACME            0.00489     -0.03321         0.05   0.772    
#> ADE             0.17620      0.06384         0.36   0.002 ** 
#> Total Effect    0.18109      0.08487         0.35  <2e-16 ***
#> Prop. Mediated  0.02700     -0.12959         0.44   0.772    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 74

## cc_grasa_total_porcentaje, sppb_total ----

custom_mediate("sppb_total", "cc_grasa_total_porcentaje", mediator = "gds_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME           -0.00325     -0.01965         0.01   0.582  
#> ADE            -0.06095     -0.10323         0.00   0.030 *
#> Total Effect   -0.06421     -0.10545        -0.01   0.018 *
#> Prop. Mediated  0.05064     -0.07855         1.74   0.588  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 79 

custom_mediate("sppb_total", "cc_grasa_total_porcentaje", mediator = "bai_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME           -0.00628     -0.02412         0.01   0.524  
#> ADE            -0.05793     -0.10214         0.00   0.044 *
#> Total Effect   -0.06421     -0.10790        -0.01   0.012 *
#> Prop. Mediated  0.09782      1.18482       669.69   0.528  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 79 

custom_mediate("sppb_total", "cc_grasa_total_porcentaje", mediator = "moca_total")
#> Causal Mediation Analysis 
#> 
#> Nonparametric Bootstrap Confidence Intervals with the BCa Method
#> 
#>                 Estimate 95% CI Lower 95% CI Upper p-value  
#> ACME            0.000127    -0.011949         0.01   0.940  
#> ADE            -0.064333    -0.105754        -0.01   0.024 *
#> Total Effect   -0.064206    -0.104609        -0.01   0.022 *
#> Prop. Mediated -0.001981    -0.119361         0.22   0.938  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Sample Size Used: 79 
