library(flextable)
library(data.table)
library(ggplot2)

data("mindfulness")

dat <- mindfulness[id_event_name == "Basal", -2L]

dat_long <- melt.data.table(
  data = dat,
  id.vars = c("id_record", "cc_grasa_total_porcentaje"), 
  measure.vars = c("hrv_sns_peri", "hrv_stress_peri", "hrv_rmssd_peri", "hrv_sdnn_peri", "hrv_pns_peri")
)

dat_long[, variable := fcase(
  grepl("sns", variable), "SNS index",
  grepl("pns", variable), "PNS index",
  grepl("stress", variable), "Stress index",
  grepl("sdnn", variable), "SDNN",
  grepl("rmssd", variable), "RMSSD"
)]

dat_long[, variable := factor(variable, levels = c("RMSSD", "SDNN", "PNS index", "SNS index", "Stress index"))]

fig <- ggplot(dat_long, aes(value, cc_grasa_total_porcentaje)) +
  facet_grid(~ variable, scales = "free_x") +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = text), data = data.frame(
    variable = as.factor(c("RMSSD", "SDNN", "PNS index", "SNS index", "Stress index")),
    cc_grasa_total_porcentaje = c(61, 61, 61, 61, 61),
    value = c(13, 20, -2, 8, 45),
    text = c("p = 0.029", "p = 0.020", "p = 0.262", "p = 0.030", "p = 0.026")
  ), size = 3.5, fontface = "italic") +
  labs(y = "Body fat (%)", x = "Values") +
  tidybayes::theme_ggdist()

ggsave(filename = "docs/manuscript/figures/fig-1.jpeg", plot = fig, 
       width = 8, height = 4, dpi = 300)
