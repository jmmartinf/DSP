# Setup
library(tidyverse)
library(broom)
library(purrr)
library(ggdist)
library(patchwork)

load("./data/02_validated.RDATA")
load("./data/03_explore.RData")

 # Suposiciones del ANOVA de una vía:
#* Las observaciones son independientes.
#* Los datos dentro de cada grupo siguen una distribución normal.
#* La varianza es igual entre los grupos (homocedasticidad).

#### Distribuciones por variable categórica
# Datos preprocesados
corrected_long <- corrected %>%
    select(!all_of(numeric_vars)) %>% 
    pivot_longer(cols = all_of(categorical_vars), 
                 names_to = "variable", 
                 values_to = "value")

# Genera densidad de frecuencias por nivel y variable
graficos <- imap(categorical_vars, function(var, index) {
    scale_value <-  c(4, 1, 1)[index]
    corrected_long %>%
        filter(variable == var) %>%
        ggplot(aes(x = !!sym(target_var), y = value,
                   color = value, 
                   fill = value)) +  
        stat_dist_halfeye(fill_type = "segments", alpha = 0.3, scale = scale_value) +
        labs(x = target_var, y = "Densidad") +
        theme_bw() +
        guides(fill = "none", color = "none")
})

# Combinar los gráficos usando patchwork
# Primer gráfico en dos filas, y los otros dos gráficos en la segunda columna
categorical_distr_plot <- graficos[[1]] + graficos[[2]] + graficos[[3]] +
    plot_layout(design = "
                AAB
                AAC
                ") +
    plot_annotation(title = "Categorical Variables Distributions",
                    subtitle = "level vs price")

# Guarda Diagrama de selección de modelo
ggsave("./figures/05_categorical_distr_plot.png", plot = categorical_distr_plot, width = 8, height = 6)

# ANOVA de una vía
anova_models <- corrected %>%
    select(!all_of(numeric_vars)) %>% 
    pivot_longer(cols = all_of(categorical_vars), 
                 names_to = "factor", 
                 values_to = "level") %>%
    group_by(factor) %>%
    nest() %>%
    mutate(
        anova_model = map(data, ~aov(as.formula(paste(target_var, "~ level")), data = .x)),
        tidy_anova = map(anova_model, tidy),
        residuals = map(anova_model, resid),     # Extraemos los residuos
        fitted_values = map(anova_model, fitted) # Extraemos los valores ajustados
    ) 

anova_results <- anova_models %>%
    unnest(tidy_anova) %>% 
    filter(term == "level") %>% 
    select(factor, df, sumsq, meansq, statistic, p.value)

anova_res_fit <- anova_models %>%
    unnest(cols = c(residuals, fitted_values))

anova_res_fit %>% 
    ggplot(aes(x = fitted_values, y = residuals)) +
    geom_point() +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    facet_wrap(~ factor, scales = "free", ncol = 1) +
    theme_bw() +
    labs(title = "Residuos vs Valores Ajustados", 
         x = "Valores Ajustados", 
         y = "Residuos")

anova_res_fit %>% 
    ggplot(aes(x = residuals)) +
    geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
    facet_wrap(~ factor, ncol = 1) +
    theme_bw() +
    labs(title = "Histograma de los Residuos", 
         x = "Residuos", 
         y = "Frecuencia")

anova_res_fit %>% 
    ggplot(aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ factor, ncol = 1) +
    theme_bw() +
    labs(title = "Q-Q Plot de los Residuos")

anova_modelo <- aov(price ~ model, data = corrected)
anova_transmision <- aov(price ~ transmission, data = corrected)
anova_combustible <- aov(price ~ fuelType, data = corrected)


summarize_anova <- function(anova_obj) {
    summary_anova <- summary(anova_obj)
    data.frame(
        term = rownames(summary_anova[[1]]),
        df = summary_anova[[1]]$Df,
        sum_sq = summary_anova[[1]]$`Sum Sq`,
        mean_sq = summary_anova[[1]]$`Mean Sq`,
        f_value = summary_anova[[1]]$`F value`,
        p_value = summary_anova[[1]]$`Pr(>F)`
    )
}

corrected %>%
    select(all_of(numeric_vars)) %>% 
    pivot_longer(cols = everything(), 
                 names_to = "variable",
                 values_to = "value") %>% 
    left_join(density_range, by = "variable") %>% 
    ggplot(aes(x = value)) +
    geom_density(fill = "grey") +     # Gráfico de densidad
    geom_boxplot(aes(), # Boxplot
                
                 #width = ~ as.numeric(density_range[ .x,2]),
                 outlier.shape = NA, 
                 alpha = 0.5
                 ) +
    facet_wrap(~ variable, ncol = 2, scales = "free", labeller = label_both) +
    labs(title = "Frequency density of numerical variables",
         x = "Value",
         y = "Density") +
    theme_bw()

width_fun <- function(x){return(0.1)}
