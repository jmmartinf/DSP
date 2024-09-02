# Relación entre variables
# Correlación entre columnas numéricas

## Carga librerías
library(tidyverse)
library(corrr)  #Used for working with tidyverse style

## Lee archivo de procesamiento
load("./data/02_validated.RDATA")
load("./data/03_explore.RData")

correlations <- corrected %>%
    select(all_of(numeric_vars)) %>% 
    correlate() %>% 
    stretch(na.rm = TRUE, remove.dups = TRUE) %>% 
    arrange(desc(abs(r)))

# Correlation with target
correlation_vs_target <- corrected %>% select(target_var,
                                              unlist(correlations[1, 1:2], 
                                                     use.names = FALSE)) %>% 
    correlate() %>% 
    stretch(na.rm = TRUE, remove.dups = TRUE) 


# Save Data
save(correlations,
     correlation_vs_target,
     file = "./data/04_correlations.RData"
     )
