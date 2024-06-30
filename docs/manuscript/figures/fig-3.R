library(flextable)
library(data.table)
library(ggplot2)

data("mindfulness")

dat <- mindfulness[id_event_name == "Basal", -2L]

dat_long <- melt.data.table(
  data = dat,
  id.vars = c("id_record", "tm_pasos", "sppb_total"),
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

fig3a <- ggplot(dat_long, aes(value, tm_pasos)) +
  facet_grid(~ variable, scales = "free_x") +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = text), data = data.frame(
    variable = as.factor(c("RMSSD", "SDNN", "PNS index", "SNS index", "Stress index")),
    tm_pasos = c(125, 125, 125, 125, 125),
    value = c(13, 20, -2, 8, 45),
    text = c("p = 0.014\nr = -0.285", "p = 0.141\nr = -0.173", "p = 0.002\nr = -0.344", "p = 0.022\nr = 0.265", "p = 0.261\nr = 0.132")
  ), size = 3.5, fontface = "italic") +
  labs(y = "TMST steps", x = NULL) +
  scale_y_continuous(expand = c(.1,.1)) +
  tidybayes::theme_ggdist() +
  theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

fig3b <- ggplot(dat_long, aes(value, sppb_total)) +
  facet_grid(~ variable, scales = "free_x") +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = text), data = data.frame(
    variable = as.factor(c("RMSSD", "SDNN", "PNS index", "SNS index", "Stress index")),
    sppb_total = c(14, 14, 14, 14, 14),
    value = c(13, 20, -2, 8, 45),
    text = c("p = 0.010\nr = -0.294", "p = 0.419\nr = -0.095", "p < 0.001\nr = -0.375", "p = 0.006\nr = 0.313", "p = 0.095\nr = 0.194")
  ), size = 3.5, fontface = "italic") +
  labs(y = "SPPB score", x = "Values") +
  scale_y_continuous(expand = c(.1,.1)) +
  tidybayes::theme_ggdist()

fig <- ggpubr::ggarrange(
  fig3a, fig3b, ncol = 1
)

ggsave(filename = "docs/manuscript/figures/fig-3.jpeg", plot = fig, 
       width = 8, height = 8, dpi = 300)
