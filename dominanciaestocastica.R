library(readr)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(MetBrewer)

# Define los colores personalizados en formato hexadecimal
colores_personalizados <- c("Rentas" = "#16317d", "Remesas" = "#a40000", "Sueldos" = "#007e2f", "Transferencias" = "#ffcd12",
                            "Dependencia rentas" = "#16317d", "Gana remesas" = "#a40000", "Dependencia sueldos" = "#007e2f", "Dependencia transferencias" = "#ffcd12")

# Cargar datos
base_1 <- read_csv("base_1.csv")

# Crear bases sin ceros en cada variable
base_rentas <- base_1 %>% filter(rentas_sum != 0) %>% select(folio_g, factor, rentas_sum) %>% mutate(ingreso_id = "Rentas", ingreso = rentas_sum)
base_remesas <- base_1 %>% filter(remesas_sum != 0) %>% select(folio_g, factor, remesas_sum) %>% mutate(ingreso_id = "Remesas", ingreso = remesas_sum)
base_sueldos <- base_1 %>% filter(sueldos_sum != 0) %>% select(folio_g, factor, sueldos_sum) %>% mutate(ingreso_id = "Sueldos", ingreso = sueldos_sum)
base_transfer <- base_1 %>% filter(transfer_sum != 0) %>% select(folio_g, factor, transfer_sum) %>% mutate(ingreso_id = "Transferencias", ingreso = transfer_sum)

# Combinar las bases
base_combined <- bind_rows(base_rentas, base_remesas, base_sueldos, base_transfer) %>% select(folio_g, factor, ingreso, ingreso_id)
str(base_combined)

# Calcular la CDF acumulada ponderada por el factor de expansión, agrupando por tipo de ingreso
base_cdf <- base_combined %>%
  group_by(ingreso_id) %>%
  arrange(ingreso) %>%
  mutate(cdf = cumsum(factor) / sum(factor))

# Graficar la CDF de los ingresos generales
ggplot(base_cdf, aes(x = ingreso, y = cdf, color = ingreso_id, fill = ingreso_id)) +
  geom_line() +  # Dibuja la línea de la CDF
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  # Línea horizontal en el 1
  labs(title = " ",
       x = "Ingreso",
       y = "Función de Distribución Acumulada",
       color = " ",
       fill = "Tipo de Ingreso") +
  theme_classic() +
  xlim(0,1000000)+
  theme(legend.position = "bottom") +
  scale_color_manual(values = colores_personalizados)

# Graficar la CDF de los ingresos generales
ggplot(base_cdf, aes(x = log(ingreso), y = cdf, color = ingreso_id, fill = ingreso_id)) +
  geom_line() +  # Dibuja la línea de la CDF
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  # Línea horizontal en el 1
  labs(title = " ",
       x = "Ingreso (log)",
       y = "Función de Distribución Acumulada",
       color = " ",
       fill = "Tipo de Ingreso") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colores_personalizados)

# Crear bases sin ceros en cada variable y con dependencia en ingreso
base_rentas_d <- base_1 %>% filter(rentas_sum != 0 & mayor_rentas == 1) %>% select(folio_g, factor, rentas_sum) %>% mutate(ingreso_id = "Dependencia rentas", ingreso = rentas_sum)
base_remesas_d <- base_1 %>% filter(remesas_sum != 0 & mayor_remesas == 1) %>% select(folio_g, factor, remesas_sum) %>% mutate(ingreso_id = "Gana remesas", ingreso = remesas_sum)
base_sueldos_d <- base_1 %>% filter(sueldos_sum != 0 & mayor_sueldos == 1) %>% select(folio_g, factor, sueldos_sum) %>% mutate(ingreso_id = "Dependencia sueldos", ingreso = sueldos_sum)
base_transfer_d <- base_1 %>% filter(transfer_sum != 0 & mayor_transfer == 1) %>% select(folio_g, factor, transfer_sum) %>% mutate(ingreso_id = "Dependencia transferencias", ingreso = transfer_sum)

# Combinar las bases con dependencia en ingreso
base_combined_d <- bind_rows(base_rentas_d, base_remesas_d, base_sueldos_d, base_transfer_d) %>% select(folio_g, factor, ingreso, ingreso_id)
str(base_combined_d)

# Calcular la CDF acumulada ponderada por el factor de expansión, agrupando por tipo de ingreso con dependencia
base_cdf_d <- base_combined_d %>%
  group_by(ingreso_id) %>%
  arrange(ingreso) %>%
  mutate(cdf = cumsum(factor) / sum(factor))


# Graficar la CDF de los ingresos con dependencia
ggplot(base_cdf_d, aes(x = ingreso, y = cdf, color = ingreso_id, fill = ingreso_id)) +
  geom_line() +  # Dibuja la línea de la CDF
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  # Línea horizontal en el 1
  labs(title = " ",
       x = "Ingreso",
       y = "Función de Distribución Acumulada",
       color = " ",
       fill = "Tipo de Ingreso") +
  theme_classic() +
  xlim(0,1000000)+
  theme(legend.position = "bottom") +
  scale_color_manual(values = colores_personalizados)

# Graficar la CDF de los ingresos con dependencia
ggplot(base_cdf_d, aes(x = log(ingreso), y = cdf, color = ingreso_id, fill = ingreso_id)) +
  geom_line() +  # Dibuja la línea de la CDF
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  # Línea horizontal en el 1
  labs(title = " ",
       x = "Ingreso (log)",
       y = "Función de Distribución Acumulada",
       color = " ",
       fill = "Tipo de Ingreso") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = colores_personalizados)




