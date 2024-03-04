
# Prepare workspace -------------------------------------------------------

## Import libraries
library(data.table)
library(corrplot)

## Import data
data("mindfulness")

## Select variables
ind <- c(`GDS-30 score` = "gds_total", `BAI score` = "bai_total", `SPPB score` = "sppb_total", `TMST steps` = "tm_pasos",
         Height = "cc_talla", Weight = "cc_peso_corporal", BMI = "cc_imc", `Body fat` = "cc_grasa_total_porcentaje", `Muscle mass` = "cc_masa_muscular_total", 
         Water = "cc_agua_corporal_porcentaje", `Bone mass` = "cc_masa_osea_porcentaje", 
         `Systolic BP` = "cv_pas_inicio_mmhg", `Diastolic BP` = "cv_pad_inicio_mmhg", `Pulse pressure` = "cv_pp_media_mmhg", 
         `Mean HR` ="hrv_mean_hr_peri", `Mean R-R` = "hrv_mean_rr_peri", RMSSD = "hrv_rmssd_peri", SDNN = "hrv_sdnn_peri",
         LF = "hrv_lf_peri", HF = "hrv_hf_peri",  `PNS Index` = "hrv_pns_peri", `SNS Index` = "hrv_sns_peri", `Stress Index` = "hrv_stress_peri")

cor_data <- mindfulness[id_event_name == "Basal", .SD, .SDcols = ind]
names(cor_data) <- names(ind)


corr <- cor(cor_data, use = "pairwise.complete.obs", method = "pearson")
corr_p <- cor.mtest(cor_data, conf.level = .95, method = "pearson")

fig <- corrplot(corr, type = "upper", diag = TRUE, p.mat = corr_p$p,
                sig.level = 0.05, insig = "pch", pch = "X", pch.cex = .75,
                tl.col = "gray20") |> substitute()
eval(fig)

jpeg("docs/manuscript/figures/fig-1.jpeg", width = 1080*2.5, height = 1080*2.5, res = 400)
eval(fig)
dev.off()

