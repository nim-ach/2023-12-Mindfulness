library(flextable)
library(data.table)
library(ggplot2)

data("mindfulness")

dat <- mindfulness[id_event_name == "Basal", -2L]

dat_long <- melt.data.table(
  data = dat,
  id.vars = c("id_record", "cc_grasa_total_porcentaje", "cc_masa_muscular_total"), 
  measure.vars = c("tm_pasos", "sppb_total")
)

dat_long[, variable := fcase(
  grepl("tm_pasos", variable), "TMST steps",
  grepl("sppb_total", variable), "SPPB total score"
)]

dat_long[, variable := factor(variable, levels = c("TMST steps", "SPPB total score"))]

fig2a <- ggplot(dat_long, aes(value, cc_grasa_total_porcentaje)) +
  facet_grid(~ variable, scales = "free_x") +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = text), data = data.frame(
    variable = as.factor(c("TMST steps", "SPPB total score")),
    cc_grasa_total_porcentaje = c(60, 60),
    value = c(42, 4),
    text = c("p = 0.013", "p = 0.015")
  ), size = 3.5, fontface = "italic") +
  labs(y = "Body fat (%)", x = NULL) +
  scale_y_continuous(expand = c(.1,.1)) +
  tidybayes::theme_ggdist()

fig2b <- ggplot(dat_long, aes(value, cc_masa_muscular_total)) +
  facet_grid(~ variable, scales = "free_x") +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = text), data = data.frame(
    variable = as.factor(c("TMST steps", "SPPB total score")),
    cc_masa_muscular_total = c(65.5, 65.5),
    value = c(42, 4),
    text = c("p = 0.541", "p = 0.584")
  ), size = 3.5, fontface = "italic") +
  labs(y = "Muscle mass (kg)", x = NULL) +
  scale_y_continuous(expand = c(.1,.1)) +
  tidybayes::theme_ggdist()

fig <- ggpubr::ggarrange(
  fig2a, fig2b, nrow = 1
)


ggsave(filename = "docs/manuscript/figures/fig-2.jpeg", plot = fig, 
       width = 8, height = 4, dpi = 300)
