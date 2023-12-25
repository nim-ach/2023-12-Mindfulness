
# Prepare workspace -------------------------------------------------------

library(data.table)

# Import dataset ----------------------------------------------------------

raw <- fread(input = "data-raw/data-raw.csv")

# Data wrangling ----------------------------------------------------------
colnames(raw)
View(raw)
 ##eliminar columnas repetidas sin valores
raw_rename <- raw [, -"Complete?"]
raw_rename <- raw_rename [, -"Survey Timestamp"]

##renombrar columnas repetidas con valores
names(raw_rename)[86] <- "bai_total"
colnames(raw_rename)

##renombrar columnas
sapply(raw_rename, class)
View(raw)



## Observar los valores unicos de cada variable
lapply(raw, unique)




# Data export -------------------------------------------------------------

