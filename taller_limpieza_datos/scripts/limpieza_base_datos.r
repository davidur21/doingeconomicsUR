# -----------------------------------------------------------------------------#
# Taller de GitHub y limpieza básica de datos con dplyr
# Archivo: limpieza_base_datos.R
# -----------------------------------------------------------------------------
#
# Objetivo:
# Observar una base con errores y corregir cada problema de manera explícita
# usando mutate() y recode().
# -----------------------------------------------------------------------------#

# Ejecute esta línea una sola vez si no tiene instalado tidyverse:
# install.packages("tidyverse")
library(tidyverse)

ruta_entrada <- "base_sucia_encuesta.txt"   # ajuste la ruta a donde tenga el .txt
ruta_salida  <- "resultados/base_limpia.csv"


# 0. Inspección inicial --------------------------------------------------------

lineas_iniciales <- readLines(
  ruta_entrada,
  n = 5,
  warn = FALSE
)

print(lineas_iniciales)

# Respuesta TODO 1:
# a) Delimitador: ";"
# b) Codificación: "WINDOWS-1252"
# c) Cadenas de valores perdidos: "N/D", "-"


# 1. Importar la base ----------------------------------------------------------

base <- read_delim(
  file = ruta_entrada,
  delim = ";",
  locale = locale(encoding = "WINDOWS-1252"),
  na = c("N/D", "-", ""),
  col_types = cols(.default = col_character()),
  trim_ws = FALSE,
  show_col_types = FALSE
)

glimpse(base)
print(base)


# 2. Corregir los nombres ------------------------------------------------------

unique(base$nombre)

base <- base %>%
  mutate(
    nombre = recode(
      nombre,
      " Ana María López " = "Ana María López",
      "JOSE MUÑOZ"         = "Jose Muñoz"
    )
  )


# 3. Corregir las ciudades -----------------------------------------------------

unique(base$ciudad)

base <- base %>%
  mutate(
    ciudad = recode(
      ciudad,
      "Bogotá "   = "Bogotá",
      "medellín"  = "Medellín",
      "CALI"      = "Cali",
      " bogotá"   = "Bogotá"
    )
  )


# 4. Corregir las fechas -------------------------------------------------------

unique(base$fecha_encuesta)

# Primero, todas las fechas deben quedar escritas como AAAA-MM-DD.

base <- base %>%
  mutate(
    fecha_encuesta = recode(
      fecha_encuesta,
      "03/08/2026"    = "2026-08-03",  # DD/MM/AAAA
      "5 agosto 2026"  = "2026-08-05",  # texto en español
      "06-08-26"       = "2026-08-06",  # DD-MM-AA
      "2026/08/07"     = "2026-08-07",  # separador "/" en vez de "-"
      "08.08.2026"     = "2026-08-08",  # DD.MM.AAAA
      "08/13/2026"     = "2026-08-13"   # MM/DD/AAAA (mes 13 no existe -> formato US)
      # "2026-08-04" ya viene en formato correcto, no necesita recode.
    )
  )

# Después, convierta la columna de texto al tipo fecha.

base <- base %>%
  mutate(
    fecha_encuesta = as.Date(
      fecha_encuesta,
      format = "%Y-%m-%d"
    )
  )


# 5. Corregir el ingreso mensual ----------------------------------------------

unique(base$ingreso_mensual)

# Quite manualmente los separadores de miles.
# Use punto únicamente para separar los decimales.

base <- base %>%
  mutate(
    ingreso_mensual = recode(
      ingreso_mensual,
      "1.250.000,50"   = "1250000.50",  # miles con punto, decimal con coma
      "1,100,000.00"   = "1100000.00",  # miles con coma, decimal con punto (US)
      "875.500,00"     = "875500.00",
      "1 050 000,25"   = "1050000.25"   # miles con espacio, decimal con coma
      # "950000.75" (fila 2) y "725000" (fila 7) ya vienen limpios.
      # "N/D" (fila 4) ya quedó como NA desde la importación.
    )
  )

# Convertir la columna de texto a número.

base <- base %>%
  mutate(
    ingreso_mensual = as.numeric(ingreso_mensual)
  )


# 6. Corregir la nota promedio -------------------------------------------------

unique(base$nota_promedio)

# Todas las notas deben usar punto como separador decimal.

base <- base %>%
  mutate(
    nota_promedio = recode(
      nota_promedio,
      "4,2" = "4.2",
      "4,0" = "4.0",
      "3,5" = "3.5",
      "4,1" = "4.1"
      # "3.8" y "4.5" ya vienen con punto.
      # "-" (fila 6) ya quedó como NA desde la importación.
    )
  )

# Convertir la columna de texto a número.

base <- base %>%
  mutate(
    nota_promedio = as.numeric(nota_promedio)
  )


# 7. Corregir la variable trabaja ---------------------------------------------

unique(base$trabaja)

# Todos los valores deben quedar exactamente como "Sí" o "No".

base <- base %>%
  mutate(
    trabaja = recode(
      trabaja,
      "si "  = "Sí",
      "NO"   = "No",
      "Sí "  = "Sí",
      "sí"   = "Sí",
      "no"   = "No"
      # "Sí" y "No" (filas 1 y 3) ya vienen correctos.
    )
  )


# 8. Convertir el identificador ------------------------------------------------

base <- base %>%
  mutate(
    id = as.integer(id)
  )


# 9. Revisar el resultado ------------------------------------------------------

print(base)
glimpse(base)
summary(base)


# 10. Comprobaciones automáticas ----------------------------------------------

stopifnot(nrow(base) == 7)
stopifnot(length(unique(base$id)) == 7)
stopifnot(inherits(base$fecha_encuesta, "Date"))
stopifnot(is.numeric(base$ingreso_mensual))
stopifnot(is.numeric(base$nota_promedio))
stopifnot(sum(is.na(base$ingreso_mensual)) == 1)
stopifnot(sum(is.na(base$nota_promedio)) == 1)
stopifnot(all(na.omit(base$trabaja) %in% c("Sí", "No")))


# 11. Exportar la base ---------------------------------------------------------

dir.create("resultados", showWarnings = FALSE)

write_csv(
  base,
  ruta_salida,
  na = ""
)

print("La base limpia fue guardada en resultados/base_limpia.csv")