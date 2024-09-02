# 2. Data validation ----
## Identifica valores NA
## Text Variables Validation
### Check unique text by variable
### Factorize Text Variables
## Numeric Variables Validation
### Identifica valores negativos
### Identifica valores cero
#### tax anomaly
#### engineSize anomaly
#### Corrections to engineSize anomaly


# Realiza una validación de los datos para identificar valores atípicos, 
# errores o inconsistencias.

# Setup
library(tidyverse)

## Load raw data
raw <- readRDS("./data/01_raw.rds")

## Identifica valores NA ----
missing_values <- raw %>%           # There are no variables with missing values
    summarise_all(~ sum(is.na(.))) 

## Text Variables Validation ----
### Check unique text by variable
unique_strings <- raw %>%
    select(where(is.character)) %>%
    summarise(across(everything(), ~ paste(unique(.), collapse = ", "))) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Levels")

### Factorize Text Variables
factorized <- raw %>%
    mutate(model = if_else(
        !grepl("\\s", model),       # Trasnsform format into variable model
        model,
        str_to_title(model)
    )) %>% 
    mutate(model = fct_relabel(model, 
                               ~ gsub(" ", "_", .))) %>% # Fix a problem in recipe processing
    mutate(across(where(is.character), as.factor)) %>% 
    relocate(price, .after = last_col())   # Dependent variable at last column

## Numeric Variables Validation ----
### Revisa valores negativos ----
negative_values <-raw %>%
    summarise(across(where(is.numeric), ~ sum(. < 0)))

### Revisa valores 0 ----
zero_values <-raw %>%
    summarise(across(where(is.numeric), ~ sum(. == 0)))

#### tax anomaly ----
raw %>% 
    filter(tax == 0) %>% 
    View

#### engineSize anomaly ----
raw %>%                     
    filter(engineSize == 0) %>% 
    count(fuelType)

raw %>%                     # Models with engineSize anomaly
    filter(engineSize == 0) %>%
    group_by(model) %>% 
    distinct(model)

raw %>%                     # Filter by model, year and fuelType
    semi_join(
        raw %>%
            filter(engineSize == 0) %>%
            select(model, year, fuelType) %>%
            distinct(),
        by = c("model", "year", "fuelType")
    ) %>%
    select(model, year, fuelType, engineSize) %>%
    distinct()

raw %>%                     # Case C-HR. Filter by model, year, transmission and fuelType
    filter(model == "C-HR" & 
           year == 2017 & 
           transmission == "Manual" & 
           fuelType == "Petrol"
           ) %>% 
    select(model, year, transmission, fuelType, engineSize) %>%
    distinct()

#### Corrección valores enginSize = 0 ----
replacement_values_non_c_hr <- factorized %>%
    filter(engineSize != 0) %>%
    group_by(model, year, fuelType) %>%
    summarise(replacement_engineSize = first(engineSize), .groups = "drop")

replacement_values_c_hr <- factorized %>%
    filter(engineSize != 0) %>%
    group_by(model, year, transmission, fuelType) %>%
    summarise(replacement_engineSize = first(engineSize), .groups = "drop")

corrected_data_non_c_hr <- factorized %>%
    filter(engineSize == 0, model != "C-HR") %>%
    left_join(replacement_values_non_c_hr, by = c("model", "year", "fuelType")) %>%
    mutate(engineSize = replacement_engineSize) %>%
    select(-replacement_engineSize)

corrected_data_c_hr <- factorized %>%
    filter(engineSize == 0, model == "C-HR") %>%
    left_join(replacement_values_c_hr, by = c("model", "year", "transmission", "fuelType")) %>%
    mutate(engineSize = replacement_engineSize) %>%
    select(-replacement_engineSize)

engineSize0 <- bind_rows(corrected_data_non_c_hr, corrected_data_c_hr)
corrected <- factorized %>%
    filter(engineSize != 0) %>%
    bind_rows(corrected_data_non_c_hr, corrected_data_c_hr)

# Guarda datos de procesamiento de modelos
save(missing_values,
     unique_strings, 
     negative_values,
     zero_values,
     engineSize0,
     corrected, 
     file = "./data/02_validated.RData")
