
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(gtsummary)
library(flextable)

## Load dataset
data("mindfulness")


# Prepare the data --------------------------------------------------------

## Variable names
mindfulness |> names()

## Select variables to work with
vars <- c("sd_sexo", "sd_edad", "cm_hipertension", "cm_diabetes", 
          "cc_peso_corporal", "cc_talla", "cc_imc", "cc_imc_categoria", "cc_masa_muscular_total",
          "cc_grasa_total_porcentaje", "cc_masa_osea_porcentaje",
          "cc_agua_corporal_porcentaje")

tbl_data <- mindfulness[id_event_name == "Basal", .SD, .SDcols = vars]

tbl_data[, cm_hipertension := `levels<-`(cm_hipertension, c("Yes", "No"))]
tbl_data[, cm_diabetes := `levels<-`(cm_diabetes, c("Yes", "No"))]
tbl_data[, cc_imc_categoria := factor(cc_imc_categoria, levels = c("Normopeso", "Sobrepeso", "Obeso"), labels = c("Normal", "Overweight", "Obese"))]
tbl_data[, sd_sexo := `levels<-`(sd_sexo, c("Female", "Male"))]

## weight and height ----

tbl_1 <- tbl_summary(
  data = tbl_data, 
  by = sd_sexo,
  missing = "no",
  label = list(
    sd_edad ~ "Age (years)",
    cm_hipertension ~ "Hypertension",
    cm_diabetes ~ "Diabetes",
    cc_peso_corporal ~ "Body weight (kg)",
    cc_talla ~ "Height (cm)",
    cc_imc ~ "BMI (kg/m2)",
    cc_imc_categoria ~ "BMI category",
    cc_masa_muscular_total ~ "Muscle mass (kg)",
    cc_grasa_total_porcentaje ~ "Fat mass (%)",
    cc_masa_osea_porcentaje ~ "Bone mass (%)",
    cc_agua_corporal_porcentaje ~ "Water (%)"
  ),
  statistic = list(
    all_continuous() ~ "{mean} ± {sd}"
  ),
  digits = list(
    all_continuous() ~ 1,
    all_categorical() ~ 0
  )
) |> 
  add_overall() |> 
  add_difference(test = list(
    all_continuous() ~ "smd"
  ),
  include = all_continuous()
)

tbl_1 <- as_flex_table(tbl_1) |> 
  flextable::autofit(add_w = 0, add_h = 0) |> 
  flextable::padding(padding.top = 1, 
                     padding.bottom = 1,
                     part = "body") |> 
  flextable::border_inner_h(border = officer::fp_border(color = "gray90", width = 1)) |> 
  flextable::delete_part("footer") |> 
  flextable::add_header_row(values = c("","Sex",""),
                            colwidths = c(2, 2, 2)) |> 
  flextable::padding(padding.top = 2, 
                     padding.bottom = 2,
                     part = "header") |> 
  flextable::border_inner_h(border = officer::fp_border(color = "gray90", width = 1),
                            part = "header") |> 
  flextable::border(i = 1, j = c(1,2,5,6), part = "header",
                    border.bottom = officer::fp_border(color = "white", width = 1)) |> 
  style(part = "body", pr_t = officer::fp_text(font.size = 9)) |> 
  style(part = "header", pr_t = officer::fp_text(font.size = 9, bold = TRUE))

saveRDS(object = tbl_1,
        file = "docs/manuscript/tables/tbl-descriptives.RDS")
