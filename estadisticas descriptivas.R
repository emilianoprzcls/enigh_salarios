library(readr)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(MetBrewer)
library(tidyr)
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

colnames(base_1)

# Listas de variables
variables <- c("mayor_rentas", "mayor_sueldos", "mayor_transfer", "mayor_remesas")
variables_cont <- c("edad_jefe", "tot_integ", "jefe_horas", "jefe_hijos")
variables_desc <- c("sexo_jefe", "educa_jefe", "jefe_etnia")
variables_consumo <- c(
  "ali_dentro_sum", "ali_fuera_sum", "vesti_calz_sum", "pred_cons_sum", 
  "agua_sum", "energia_sum", "salud_sum", "transporte_sum", 
  "publico_sum", "comunica_sum", "educacion_sum", "erogac_tot_sum", "cuota_viv_sum"
)

# Inicializar una lista para almacenar los resultados
resultados <- list()

# Iterar sobre cada variable en "variables"
for (var in variables) {
  # Crear resumen ponderado para la variable actual
  resumen <- base_1 %>%
    group_by(!!sym(var)) %>%
    summarise(across(all_of(variables_consumo),
                     list(
                       promedio = ~ weighted.mean(.x, w = factor, na.rm = TRUE),
                       desviacion = ~ sqrt(weighted.mean((.x - weighted.mean(.x, w = factor, na.rm = TRUE))^2, w = factor, na.rm = TRUE))
                     ),
                     .names = "{col}_{fn}"
    )) %>%
    ungroup() %>%
    # Añadir una columna para identificar la variable agrupada
    mutate(variable = var)
  
  # Añadir el resumen a la lista de resultados
  resultados[[var]] <- resumen
}

# Combinar todos los resultados en un solo data frame
cont_summary_all <- bind_rows(resultados)

#VARIABLES DESCRIPTIVAS
# Listas de variables descriptivas y variables de interés
variables_desc <- c("sexo_jefe", "educa_jefe", "jefe_etnia")
variables <- c("mayor_rentas", "mayor_sueldos", "mayor_transfer", "mayor_remesas")

# Crear una tabla vacía para almacenar los resultados
tabla_final <- data.frame()

# Iterar sobre cada variable de interés
for (var_interes in variables) {
  # Iterar sobre cada variable descriptiva
  for (var_desc in variables_desc) {
    # Calcular la tabla de frecuencias ponderada para cada combinación
    table_proporcion <- with(base_1, tapply(factor, list(base_1[[var_desc]], base_1[[var_interes]]), sum, na.rm = TRUE))
    
    # Convertir a proporciones
    table_proporcion <- table_proporcion / sum(base_1$factor) * 100
    
    # Convertir en data frame y agregar el nombre de la variable descriptiva y de interés para identificar
    table_df <- as.data.frame.table(table_proporcion)
    table_df$variable_desc <- var_desc
    table_df$variable_interes <- var_interes
    
    # Hacer append de la tabla de la variable actual a la tabla final
    tabla_final <- rbind(tabla_final, table_df)
  }
}

# Visualizar la tabla final
tabla_final









