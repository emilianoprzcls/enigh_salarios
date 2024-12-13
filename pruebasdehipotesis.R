library(readr)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(MetBrewer)
library(tidyr)
# Cargar datos
base_1 <- read_csv("base_1.csv")

# Crear bases sin ceros en cada variable y con dependencia en ingreso
base_rentas_d <- base_1 %>% filter(rentas_sum != 0 & mayor_rentas == 1)  %>% mutate(ingreso_id = "Dependencia rentas", ingreso = rentas_sum)
base_sueldos_d <- base_1 %>% filter(sueldos_sum != 0 & mayor_sueldos == 1)  %>% mutate(ingreso_id = "Dependencia sueldos", ingreso = sueldos_sum)
base_transfer_d <- base_1 %>% filter(transfer_sum != 0 & mayor_transfer == 1) %>% mutate(ingreso_id = "Dependencia transferencias", ingreso = transfer_sum)

# Combinar las bases con dependencia en ingreso
base_combined_d <- bind_rows(base_rentas_d, base_sueldos_d, base_transfer_d) %>% select(folio_g, factor, ingreso, ingreso_id)

quantiles <- base_combined_d %>%
  group_by(ingreso_id) %>%
  mutate(
    q_25 = quantile(ingreso, 0.25, na.rm = TRUE),
    q_50 = quantile(ingreso, 0.50, na.rm = TRUE),
    q_75 = quantile(ingreso, 0.75, na.rm = TRUE),
    q_95 = quantile(ingreso, 0.95, na.rm = TRUE),
    ingreso_rango = case_when(
      ingreso <= q_25 ~ "Abajo del 25",
      ingreso > q_25 & ingreso <= q_50 ~ "Entre 25 y 50",
      ingreso > q_50 & ingreso <= q_75 ~ "Entre 50 y 75",
      ingreso > q_75 & ingreso <= q_95 ~ "Entre 75 y 95",
      ingreso > q_95 ~ "Arriba del 95"
    )
  ) %>%
  ungroup()

str(quantiles)

resultados <- data.frame()

# Iterar sobre cada grupo de ingreso_rango
for (rango in unique(quantiles$ingreso_rango)) {
  # Filtrar los datos por el grupo de ingreso_rango
  subset_data <- quantiles %>% filter(ingreso_rango == rango)
  
  # Obtener los niveles de ingreso_id
  ids <- unique(subset_data$ingreso_id)
  
  # Realizar comparaciones 1 a 1 entre los tipos de ingreso_id
  for (i in 1:(length(ids) - 1)) {
    for (j in (i + 1):length(ids)) {
      # Subset de los dos grupos a comparar
      group1 <- subset_data %>% filter(ingreso_id == ids[i]) %>% pull(ingreso)
      group2 <- subset_data %>% filter(ingreso_id == ids[j]) %>% pull(ingreso)
      
      # Prueba t de diferencia de medias
      test <- t.test(group1, group2)
      
      # Guardar los resultados
      resultados <- rbind(resultados, data.frame(
        ingreso_rango = rango,
        ingreso_id_1 = ids[i],
        ingreso_id_2 = ids[j],
        media_1 = mean(group1, na.rm = TRUE),
        media_2 = mean(group2, na.rm = TRUE),
        diferencia = mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE),
        p_value = test$p.value
      ))
    }
  }
}
resultados
# Crear un nuevo dataframe para guardar los resultados
varianza_resultados <- data.frame()

# Iterar sobre cada grupo de ingreso_cuantil
for (grupo_cuantil in unique(quantiles$ingreso_rango)) {
  # Filtrar los datos por el grupo de ingreso_cuantil
  datos_filtrados <- quantiles %>% filter(ingreso_rango == grupo_cuantil)
  
  # Obtener los niveles únicos de ingreso_tipo
  tipos_ingreso <- unique(datos_filtrados$ingreso_id)
  
  # Realizar comparaciones 1 a 1 entre los tipos de ingreso
  for (idx1 in 1:(length(tipos_ingreso) - 1)) {
    for (idx2 in (idx1 + 1):length(tipos_ingreso)) {
      # Subset de los dos grupos a comparar
      grupo_1 <- datos_filtrados %>% filter(ingreso_id == tipos_ingreso[idx1]) %>% pull(ingreso)
      grupo_2 <- datos_filtrados %>% filter(ingreso_id == tipos_ingreso[idx2]) %>% pull(ingreso)
      
      # Prueba F de diferencia de varianzas
      f_test <- var.test(grupo_1, grupo_2)
      
      # Guardar los resultados en el nuevo dataframe
      varianza_resultados <- rbind(varianza_resultados, data.frame(
        rango_ingreso = grupo_cuantil,
        tipo_ingreso_1 = tipos_ingreso[idx1],
        tipo_ingreso_2 = tipos_ingreso[idx2],
        varianza_1 = var(grupo_1, na.rm = TRUE),
        varianza_2 = var(grupo_2, na.rm = TRUE),
        f_estadistico = f_test$statistic,
        p_valor = f_test$p.value
      ))
    }
  }
}

grupo_1 <- quantiles %>% filter(ingreso_id == "Dependencia sueldos" & ingreso_rango == "Entre 75 y 95") %>% pull(ingreso)
grupo_2 <- quantiles %>% filter(ingreso_id == "Dependencia transferencias" & ingreso_rango == "Entre 75 y 95" ) %>% pull(ingreso)
# Calcular las varianzas muestrales de ambos grupos
var_grupo_1 <- var(grupo_1)
var_grupo_2 <- var(grupo_2)
var.test(grupo_1, grupo_2)
# Calcular el valor F manualmente
F_manual <- var_grupo_2 / var_grupo_1

# Mostrar el resultado
F_manual
# Tamaño de las muestras
n1 <- length(grupo_1)
n2 <- length(grupo_2)

# Grados de libertad
df1 <- n1 - 1
df2 <- n2 - 1

# Nivel de significancia (5%)
alpha <- 0.05

# Valor crítico F (one-tailed test)
F_critico <- qf(1 - alpha, df1, df2)

# Mostrar el resultado
F_critico


# Mostrar los resultados
library(ace_tools)
ace_tools.display_dataframe_to_user(name="Diferencias de medias por grupo", dataframe=resultados)


# ANOVA para evaluar la diferencia de medias
anova_model <- aov(ingreso ~ ingreso_rango + ingreso_id, data = quantiles)
summary(anova_model)




