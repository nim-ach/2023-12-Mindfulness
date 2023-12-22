
# Prepare workspace -------------------------------------------------------

library(data.table)

# Import dataset ----------------------------------------------------------

raw <- fread(input = "data-raw/data-raw.csv")

# Data wrangling ----------------------------------------------------------

## Observar los valores unicos de cada variable
lapply(raw, unique)



# Data export -------------------------------------------------------------

