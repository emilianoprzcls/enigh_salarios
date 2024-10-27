library(readr)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(MetBrewer)
# Cargar datos
base_1 <- read_csv("base_1.csv")

# Crear bases sin ceros en cada variable
base_rentas <- base_1 %>% filter(rentas_sum != 0)
base_remesas <- base_1 %>% filter(remesas_sum != 0)
base_sueldos <- base_1 %>% filter(sueldos_sum != 0)
base_transfer <- base_1 %>% filter(transfer_sum != 0)

# Tabla descriptiva para rentas_sum
rentas_summary <- base_rentas %>%
  summarise(
    Media = weighted.mean(rentas_sum, w = factor, na.rm = TRUE),
    Desviación_Estandar = sqrt(sum((rentas_sum - weighted.mean(rentas_sum, w = factor, na.rm = TRUE))^2 * factor) / sum(factor)),
    Mínimo = min(rentas_sum, na.rm = TRUE),
    Percentil_25 = quantile(rep(rentas_sum, times = factor), 0.25, na.rm = TRUE),
    Mediana = quantile(rep(rentas_sum, times = factor), 0.5, na.rm = TRUE),
    Percentil_75 = quantile(rep(rentas_sum, times = factor), 0.75, na.rm = TRUE),
    Percentil_95 = quantile(rep(rentas_sum, times = factor), 0.95, na.rm = TRUE),
    Máximo = max(rentas_sum, na.rm = TRUE)
  )
rentas_summary

# Tabla descriptiva para remesas_sum
remesas_summary <- base_remesas %>%
  summarise(
    Media = weighted.mean(remesas_sum, w = factor, na.rm = TRUE),
    Desviación_Estandar = sqrt(sum((remesas_sum - weighted.mean(remesas_sum, w = factor, na.rm = TRUE))^2 * factor) / sum(factor)),
    Mínimo = min(remesas_sum, na.rm = TRUE),
    Percentil_25 = quantile(rep(remesas_sum, times = factor), 0.25, na.rm = TRUE),
    Mediana = quantile(rep(remesas_sum, times = factor), 0.5, na.rm = TRUE),
    Percentil_75 = quantile(rep(remesas_sum, times = factor), 0.75, na.rm = TRUE),
    Percentil_95 = quantile(rep(remesas_sum, times = factor), 0.95, na.rm = TRUE),
    Máximo = max(remesas_sum, na.rm = TRUE)
  )
remesas_summary

# Tabla descriptiva para sueldos_sum
sueldos_summary <- base_sueldos %>%
  summarise(
    Media = weighted.mean(sueldos_sum, w = factor, na.rm = TRUE),
    Desviación_Estandar = sqrt(sum((sueldos_sum - weighted.mean(sueldos_sum, w = factor, na.rm = TRUE))^2 * factor) / sum(factor)),
    Mínimo = min(sueldos_sum, na.rm = TRUE),
    Percentil_25 = quantile(rep(sueldos_sum, times = factor), 0.25, na.rm = TRUE),
    Mediana = quantile(rep(sueldos_sum, times = factor), 0.5, na.rm = TRUE),
    Percentil_75 = quantile(rep(sueldos_sum, times = factor), 0.75, na.rm = TRUE),
    Percentil_95 = quantile(rep(sueldos_sum, times = factor), 0.95, na.rm = TRUE),
    Máximo = max(sueldos_sum, na.rm = TRUE)
  )
sueldos_summary

# Tabla descriptiva para transfer_sum
transfer_summary <- base_transfer %>%
  summarise(
    Media = weighted.mean(transfer_sum, w = factor, na.rm = TRUE),
    Desviación_Estandar = sqrt(sum((transfer_sum - weighted.mean(transfer_sum, w = factor, na.rm = TRUE))^2 * factor) / sum(factor)),
    Mínimo = min(transfer_sum, na.rm = TRUE),
    Percentil_25 = quantile(rep(transfer_sum, times = factor), 0.25, na.rm = TRUE),
    Mediana = quantile(rep(transfer_sum, times = factor), 0.5, na.rm = TRUE),
    Percentil_75 = quantile(rep(transfer_sum, times = factor), 0.75, na.rm = TRUE),
    Percentil_95 = quantile(rep(transfer_sum, times = factor), 0.95, na.rm = TRUE),
    Máximo = max(transfer_sum, na.rm = TRUE)
  )
transfer_summary

# Prepare weighted income data
data_combined_weighted <- bind_rows(
  base_rentas %>% select(rentas_sum, factor) %>% rename(Ingreso = rentas_sum, factor_expansion = factor) %>% mutate(Tipo = "Rentas", Ingreso_ponderado = Ingreso * factor_expansion),
  base_remesas %>% select(remesas_sum, factor) %>% rename(Ingreso = remesas_sum, factor_expansion = factor) %>% mutate(Tipo = "Remesas", Ingreso_ponderado = Ingreso * factor_expansion),
  base_sueldos %>% select(sueldos_sum, factor) %>% rename(Ingreso = sueldos_sum, factor_expansion = factor) %>% mutate(Tipo = "Sueldos", Ingreso_ponderado = Ingreso * factor_expansion),
  base_transfer %>% select(transfer_sum, factor) %>% rename(Ingreso = transfer_sum, factor_expansion = factor) %>% mutate(Tipo = "Transferencias", Ingreso_ponderado = Ingreso * factor_expansion)
)

# Create the density plot with custom colors and no legend title
ggplot(data_combined_weighted, aes(x = log(Ingreso_ponderado), fill = Tipo, weight = factor_expansion)) +
  geom_density(alpha = 0.3) + # Adjust alpha for transparency
  scale_x_continuous(labels = scales::comma) + # Format x-axis labels
  labs(title = " ", 
       x = "Ingreso ponderado (log)", 
       y = "Densidad") +
  scale_fill_manual(values=met.brewer("Austria", 4), name = NULL) +
  theme_light() +
  theme(legend.position = "bottom")



