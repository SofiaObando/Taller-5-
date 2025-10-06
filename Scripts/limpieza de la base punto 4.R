# -----------------------------------------------------------
# TALLER 5 - Limpieza y transformación de datos de acciones
# -----------------------------------------------------------

# 0) Cargar librerías necesarias
library(readxl)   # para leer archivos Excel (.xlsx)
library(dplyr)    # para manipulación de datos
library(tidyr)    # para transformar datos (pivot_longer / pivot_wider)
library(stringr)  # para manejo de cadenas de texto

# -----------------------------------------------------------
# 1) Leer base original desde GitHub
# -----------------------------------------------------------

url <- "https://github.com/SofiaObando/Taller-5-/raw/889eb1009f79be010ab689316aa4e50ef828e804/data/raw/acciones_precio_volumen.xlsx"
temp <- tempfile(fileext = ".xlsx")     # crear archivo temporal
download.file(url, temp, mode = "wb")   # descargar en formato binario
df <- read_excel(temp, sheet = "Valores")  # leer hoja "Valores"

# -----------------------------------------------------------
# 2) Renombrar y limpiar estructura inicial
# -----------------------------------------------------------

df <- df[-1, ] %>%                      # eliminar primera fila (cabecera duplicada)
  rename(Fecha = "...1")                # renombrar la primera columna

# -----------------------------------------------------------
# 3) Transformar a formato largo (long)
# -----------------------------------------------------------

df_long <- df %>%
  pivot_longer(
    cols = 2:last_col(),                # columnas desde la 2 en adelante
    names_to = "Stock_Variable",        # nombre de variable
    values_to = "Value"                 # valores
  )

# -----------------------------------------------------------
# 4) Clasificar tipo de dato (precio o volumen)
# -----------------------------------------------------------

df_long <- df_long %>%
  mutate(
    num = str_extract(Stock_Variable, "\\d+$") %>% as.numeric(),  # extraer número final
    tipo = case_when(
      num %% 2 == 0 & num >= 2 & num <= 20 ~ "precio",           # pares = precio
      num %% 2 == 1 & num >= 3 & num <= 21 ~ "volumen",          # impares = volumen
      TRUE ~ NA_character_
    )
  ) %>%
  select(-num) %>%                        # eliminar columna auxiliar
  relocate(tipo, .before = Value)         # mover 'tipo' antes de 'Value'

# -----------------------------------------------------------
# 5) Clasificar región según sufijo del nombre
# -----------------------------------------------------------

df_long <- df_long %>%
  mutate(
    region = case_when(
      grepl("US Equity", Stock_Variable) ~ "america",
      grepl("GY Equity|LN Equity", Stock_Variable) ~ "europa",
      grepl("JT Equity", Stock_Variable) ~ "asia",
      TRUE ~ "otro"
    )
  ) %>%
  relocate(region, .before = Stock_Variable)

# -----------------------------------------------------------
# 6) Pasar a formato ancho (precio y volumen por acción)
# -----------------------------------------------------------

df_clean <- df_long %>%
  mutate(
    # quitar número final para unificar acciones
    accion = str_trim(str_remove(Stock_Variable, "\\.\\.\\.\\d+$"))
  ) %>%
  select(Fecha, region, accion, tipo, Value) %>%
  pivot_wider(
    names_from = tipo,
    values_from = Value
  )

# -----------------------------------------------------------
# 7) Convertir columnas a tipo numérico
# -----------------------------------------------------------

df_clean$precio  <- as.numeric(df_clean$precio)
df_clean$volumen <- as.numeric(df_clean$volumen)
df_clean$Fecha   <- as.numeric(df_clean$Fecha)

# -----------------------------------------------------------
# 8) Exportar base limpia a Excel
# -----------------------------------------------------------

library(writexl)
write_xlsx(df_clean, "acciones_limpias.xlsx")

# -----------------------------------------------------------
# Fin del script
# -----------------------------------------------------------



