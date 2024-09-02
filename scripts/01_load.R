# Script carga de Dataset toyota.csv

# Carga de Paquetes
library(readr)      # Import data

# Load Data
path <- "https://s3.amazonaws.com/talent-assets.datacamp.com/toyota.csv"
raw <- read_csv(path)

saveRDS(raw, "./data/raw.rds")