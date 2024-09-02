# 3. Exploratory Analysis ----
## Describe Dataset
## Factorize categorical variables
## Decribe levels
## Describe Target variable
## Describe independent variables
## Save outcomes

# Carga de Paquetes
library(tidyverse)

# Load validated data
load("./data/02_validated.RData")

## Describe Dataset ----
### Listas de variables ----
target_var <- "price"                           # Name of target variable
numeric_vars <- corrected %>%                   # Numeric variables, excluded year
    select(where(is.numeric), -all_of(target_var)) %>%
    names()
categorical_vars <- corrected %>%               # Categorical variables
    select(where(is.character) | where(is.factor),
           -all_of(target_var)) %>%
    names()

### Crear la tabla resumen para cada variable ----
overview <- corrected %>%
    summarise_all(list(Type = ~ class(.),
                       `NA` = ~ sum(is.na(.)),
                       Unique = ~ length(unique(.)),
                       Min = ~ if(is.numeric(.)) min(.) 
                        else NA_real_,
                       Mean = ~ if(is.numeric(.)) mean(.) 
                        else NA_real_,
                       Max = ~ if(is.numeric(.)) max(.) 
                        else NA_real_
                       )
                  ) %>%
    pivot_longer(cols = everything(), 
                 names_to = c("variable", ".value"), 
                 names_sep = "_")

## Describe Categorical Variable Levels ----
levels_categorical_vars <- corrected %>% 
    select(all_of(categorical_vars)) %>%
    mutate(across(all_of(categorical_vars), ~ as.factor(.))) %>% 
    pivot_longer(cols = everything(), 
                 names_to = "variable", 
                 values_to = "level") %>%
    group_by(variable, level) %>%
    summarize(relative_freq = (n() / nrow(corrected)) * 100) %>%
    arrange(variable, desc(relative_freq)) %>%
    # Agrupar los niveles y frecuencias en una sola cadena por variable
    group_by(variable) %>%
    summarize(
        levels_freq = paste0(level, 
                                   "(", 
                                   round(relative_freq, 1), 
                                   "%)", 
                                   collapse = ", ")) %>%
    ungroup()

## Target statistics ----
target_statistics <- corrected %>% 
    select(all_of(target_var)) %>% 
    summarize(
        n_missing = sum(is.na(!!sym(target_var))),
        Min = min(.),
        P25 = quantile(!!sym(target_var), probs = 0.25),
        Median = median(!!sym(target_var)),
        Mean = round(mean(!!sym(target_var)), 0),
        P75 = quantile(!!sym(target_var), probs = 0.75),
        Max = max(.),
        StdDesv = round(sd(!!sym(target_var)), 0),
        IQR = IQR(!!sym(target_var))
    )

## Numeric Variables ----
numeric_vars_distr <- corrected %>%
    select(all_of(numeric_vars)) %>% 
    pivot_longer(cols = everything(), 
                 names_to = "variable", 
                 values_to = "value") %>% 
    ggplot(aes(x = variable, 
               y = value)) +
    geom_boxplot() +
    facet_wrap(~ variable, scales = "free", nrow = 1) +
    labs(title = "Numeric Variables Distribution",
         x = "Variable",
         y = "Value") +
    theme_bw()

numeric_vars_target <- corrected %>%
    select(!all_of(categorical_vars)) %>% 
    pivot_longer(cols = - price,             # Pivotear todas las columnas excepto 'price'
                 names_to = "variable", 
                 values_to = "value") %>% 
    ggplot(aes(x = value, y = price)) + 
    geom_smooth(, color = "red", size = 0.5) +
    facet_wrap(~ variable, scales = "free", ncol = 1) +
    theme_bw()

## Guardar los dataframes y variables en un archivo .RData ----
save(target_var,
     numeric_vars,
     categorical_vars,
     overview,
     levels_categorical_vars,
     target_statistics,
     file = "./data/03_explore.RData"
     )

# Distribuciones por variable categórica ----
## Pre-Processing Data ----
corrected_long <- corrected %>%
    select(!all_of(numeric_vars)) %>% 
    pivot_longer(cols = all_of(categorical_vars), 
                 names_to = "variable", 
                 values_to = "value")

## Genera densidad de frecuencias por nivel y variable ----
graficos <- imap(categorical_vars, function(var, index) {
    scale_value <-  c(4, 1, 1)[index]
    corrected_long %>%
        filter(variable == var) %>%
        ggplot(aes(x = !!sym(target_var), y = value,
                   color = value, 
                   fill = value)) +  
        stat_dist_halfeye(fill_type = "segments", 
                          alpha = 0.3, 
                          scale = scale_value) +
        labs(x = target_var, y = "Densidad") +
        theme_bw() +
        guides(fill = "none", color = "none")
})

## Combinar los gráficos usando patchwork
### Primer gráfico en dos filas, y los otros dos gráficos en la segunda columna
categorical_distr_plot <- graficos[[1]] + graficos[[2]] + graficos[[3]] +
    plot_layout(design = "
                AAB
                AAC
                ") +
    plot_annotation(title = "Categorical Variables Distributions",
                    subtitle = "level vs price")

## Guarda Diagrama de selección de modelo
ggsave("./figures/03_categorical_distr_plot.png", 
       plot = categorical_distr_plot, width = 8, 
       height = 6)
