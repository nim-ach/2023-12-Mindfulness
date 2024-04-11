library(data.table)

data("mindfulness")

vars <- c("sd_sexo", "sd_edad", "cm_hipertension", "cm_diabetes", 
  "cc_peso_corporal", "cc_talla", "cc_imc", "cc_imc_categoria", "cc_masa_muscular_total",
  "cc_grasa_total_porcentaje", "cc_masa_osea_porcentaje",
  "cc_agua_corporal_porcentaje", "hrv_mean_rr_peri", "hrv_rmssd_peri", "hrv_pns_peri", 
  "hrv_mean_hr_peri", "hrv_stress_peri", "hrv_sns_peri", 
  "hrv_sdnn_peri", "hrv_mean_rr_post", "hrv_rmssd_post", 
  "hrv_pns_post", "hrv_mean_hr_post", "hrv_stress_post",
  "hrv_sns_post", "hrv_sdnn_post", "sppb_total", "tm_pasos")

dat <- mindfulness[id_event_name == "Basal", .SD, .SDcols = vars]

dat[, sd_sexo := `levels<-`(sd_sexo, c("Female", "Male"))][]

dat[, cm_hipertension := `levels<-`(cm_hipertension, c("Yes", "No"))][]

dat[, cm_diabetes := `levels<-`(cm_diabetes, c("Yes", "No"))][]

dat[, cc_imc_categoria := `levels<-`(cc_imc_categoria, c("Underweight", "Normal", "Overweight", "Obese"))][]

names(dat) <- c("sd_sex", "sd_age", "cm_hypertension", "cm_diabetes", "cc_body_weight", 
                "cc_height", "cc_bmi", "cc_bmi_category", "cc_muscle_mass_total", 
                "cc_fat_mass_total_percentage", "cc_bone_mass_percentage", "cc_body_water_percentage", 
                "hrv_mean_rr_peri", "hrv_rmssd_peri", "hrv_pns_peri", "hrv_mean_hr_peri", 
                "hrv_stress_peri", "hrv_sns_peri", "hrv_sdnn_peri", "hrv_mean_rr_post", 
                "hrv_rmssd_post", "hrv_pns_post", "hrv_mean_hr_post", "hrv_stress_post", 
                "hrv_sns_post", "hrv_sdnn_post", "sppb_total", "tm_steps")

View(dat)

fwrite(dat, file = "data-raw/paper-data.csv")
