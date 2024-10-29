# Cargar librerías
library(dplyr)      # Para manipulación de datos
library(foreign)    # Para leer archivos .dbf

# Definir los años a procesar
years <- c(2016, 2018, 2020, 2022)

# Función para cargar y procesar los datos para cada año
process_data <- function(year) {
  
  # Cargar y procesar datos de 'concen' para el año específico
  conce <- read.dbf(paste0("db/", year, "/concen.dbf")) %>%
    mutate(
      folio_g = as.factor(paste0(folioviv, foliohog))  # Crear identificador único
    ) %>%
    select(
      folio_g, factor, ubica_geo, edad_jefe, sexo_jefe, educa_jefe, 
      tot_integ, rentas, remesas, sueldos, transfer,
      ali_dentro, ali_fuera, vesti_calz, pred_cons, agua, energia, 
      salud, transporte, publico, comunica, educacion, erogac_tot, cuota_viv
    )
  
  # Cargar y procesar datos de 'pobla' para el año específico
  pobla <- read.dbf(paste0("db/", year, "/pobla.dbf")) %>%
    filter(parentesco == 101) %>%  # Filtrar registros donde la persona es el jefe de hogar
    mutate(
      folio_g = as.factor(paste0(folioviv, foliohog)),  # Crear identificador único
      jefe_etnia = ifelse(etnia == 1, 1, 0),            # Marcar etnia del jefe (1 si es de etnia)
      jefe_horas = hor_1,                               # Horas de trabajo del jefe
      jefe_hijos = hijos_viv                            # Número de hijos vivos del jefe
    ) %>%
    select(folio_g, jefe_etnia, jefe_horas, jefe_hijos)  # Seleccionar variables relevantes
  
  # Realizar la unión de los datasets 'conce' y 'pobla' para el año específico
  data <- left_join(conce, pobla, by = "folio_g")
  
  # Añadir una columna con el año para referencia
  data <- mutate(data, year = year)
  
  return(data)
}

# Aplicar la función a cada año y combinar los resultados en una lista
data_list <- lapply(years, process_data)

# Unir todas las filas de la lista en un solo dataframe
final_data <- bind_rows(data_list)

# Agrupar datos y sumar los ingresos para cada combinación de variables relevantes
final_data_summarized <- final_data %>%
  group_by(
    year, ubica_geo, folio_g, factor, edad_jefe, sexo_jefe, educa_jefe, 
    tot_integ, jefe_etnia, jefe_horas, jefe_hijos
  ) %>%
  summarize(
    rentas_sum = sum(rentas, na.rm = TRUE),           # Sumar ingresos por rentas
    remesas_sum = sum(remesas, na.rm = TRUE),         # Sumar ingresos por remesas
    sueldos_sum = sum(sueldos, na.rm = TRUE),         # Sumar ingresos por sueldos
    transfer_sum = sum(transfer, na.rm = TRUE),       # Sumar ingresos por transferencias
    ali_dentro_sum = sum(ali_dentro, na.rm = TRUE),   # Sumar alimentación dentro del hogar
    ali_fuera_sum = sum(ali_fuera, na.rm = TRUE),     # Sumar alimentación fuera del hogar
    vesti_calz_sum = sum(vesti_calz, na.rm = TRUE),   # Sumar vestimenta y calzado
    pred_cons_sum = sum(pred_cons, na.rm = TRUE),     # Sumar predial y construcción
    agua_sum = sum(agua, na.rm = TRUE),               # Sumar agua
    energia_sum = sum(energia, na.rm = TRUE),         # Sumar energía
    salud_sum = sum(salud, na.rm = TRUE),             # Sumar salud
    transporte_sum = sum(transporte, na.rm = TRUE),   # Sumar transporte
    publico_sum = sum(publico, na.rm = TRUE),         # Sumar gastos públicos
    comunica_sum = sum(comunica, na.rm = TRUE),       # Sumar comunicación
    educacion_sum = sum(educacion, na.rm = TRUE),     # Sumar educación
    erogac_tot_sum = sum(erogac_tot, na.rm = TRUE),   # Sumar erogaciones totales
    cuota_viv_sum = sum(cuota_viv, na.rm = TRUE),     # Sumar cuota de vivienda
    .groups = 'drop'                                  # Desagrupar después del resumen
  )

# Crear una copia de la base de datos original para evitar modificarla directamente
final_data_summarized <- final_data_summarized %>%
  mutate(
    # Crear variable binaria para indicar cuando el hogar gana mayormente por sueldos
    mayor_sueldos = ifelse(sueldos_sum > rentas_sum & sueldos_sum > transfer_sum & sueldos_sum > remesas_sum, 1, 0),
    
    # Crear variable binaria para indicar cuando el hogar gana mayormente por rentas
    mayor_rentas = ifelse(rentas_sum > sueldos_sum & rentas_sum > transfer_sum & rentas_sum > remesas_sum, 1, 0),
    
    # Crear variable binaria para indicar cuando el hogar gana mayormente por transferencias
    mayor_transfer = ifelse(transfer_sum > sueldos_sum & transfer_sum > rentas_sum & transfer_sum > remesas_sum, 1, 0),
    
    # Crear variable binaria para indicar cuando el hogar gana mayormente por remesas
    mayor_remesas = ifelse(remesas_sum > 0, 1, 0)
  )

variables_def <- c(
  "rentas_sum", "remesas_sum", "sueldos_sum", "transfer_sum", 
  "ali_dentro_sum", "ali_fuera_sum", "vesti_calz_sum", "pred_cons_sum", 
  "agua_sum", "energia_sum", "salud_sum", "transporte_sum", 
  "publico_sum", "comunica_sum", "educacion_sum", "erogac_tot_sum", "cuota_viv_sum"
)

inpc_data <- data.frame(
  year = c(2022, 2020, 2018, 2016),
  def_factor = c(100/129.38, 100/113.818, 100/103.642, 100/95.849360000000)
)

final_data_summarized <- left_join(final_data_summarized, inpc_data)
final_data_summarized[variables_def] <- lapply(final_data_summarized[variables_def], function(x) x * final_data_summarized$def_factor)

str(final_data_summarized)
# Exportar datos resumidos a CSV
write.csv(final_data_summarized, "base_1.csv", row.names = FALSE)












