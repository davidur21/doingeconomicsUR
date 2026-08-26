# ================================================================
# TALLER 4a - VISUALIZACION
# Doing Economics - Penetracion de internet en tenderos de Colombia
# ================================================================
# Proyecto : Penetracion de internet y poblacion municipal - 10 ciudades
# Autor(es): [Camilo Ospina, David Pascagaza, Emily Rodriguez]
# Fecha    : 2026-08-23
#
# Input:
#   - Datos/Originales/TenderosFU03_Publica.dta   (encuesta a tenderos)
#   - Datos/Originales/TerriData_Dim2.xlsx        (poblacion DANE, hoja "Hoja02")
#   - Datos/Derivados/base_larga.xlsx             (generada por 03_Analisis.R)
#
# Output:
#   - Outputs/Imagenes/grafico_ciudad.png
#   - Outputs/Imagenes/grafico_actividad.png
#   - Outputs/Imagenes/grafico_poblacion.png
#
# Proposito:
#   Construir las 3 graficas clave del Taller 4a (una por diapositiva),
#   aplicando la teoria de visualizacion vista en clase: una historia por
#   grafico, colores solo para procesamiento preatentivo, orden segun el
#   valor (no alfabetico), labels cuando se necesita precision, y cuidado
#   con el tamano de muestra ("el bigote") antes de resaltar diferencias.
#
# Este script asume que ya corrieron 03_Analisis.R (el que ya tienen) y
# que "base_larga.xlsx" existe en Datos/Derivados. Los dos resumenes que
# 03_Analisis.R no exporto (internet por ciudad e internet por actividad,
# a nivel nacional) se recalculan aqui a partir de los datos originales.
# ================================================================

# ---------------------
# 0. LIMPIEZA Y CONFIGURACION INICIAL
# ---------------------
rm(list = ls())  # limpiar memoria (buena practica -> Manejo de datos, Regla 2)

# Directorio del proyecto. AJUSTAR por cada integrante/equipo antes de correr.
# Se usa file.path() para que la ruta funcione igual en Windows/Mac/Linux
# (evita el problema de "/" vs "\" mencionado en la Regla 2 de Manejo de datos).
directorio      <- "."   # raiz del repositorio del taller
dir_datos_orig  <- file.path(directorio, "Datos", "Originales")
dir_datos_deriv <- file.path(directorio, "Datos", "Derivados")
dir_outputs_img <- file.path(directorio, "Outputs", "Imagenes")
dir.create(dir_outputs_img, recursive = TRUE, showWarnings = FALSE)

# Paquetes necesarios (se instalan solo si faltan)
paquetes <- c("dplyr", "tidyr", "ggplot2", "scales", "forcats",
              "haven", "readxl", "writexl", "ggrepel")
faltantes <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(faltantes) > 0) install.packages(faltantes)
invisible(lapply(paquetes, library, character.only = TRUE))

# ---------------------
# 1. PALETA Y TEMA VISUAL
# ---------------------
# Un color domina (gris azulado neutro), un acento resalta lo positivo,
# y un tercer color resalta lo que llama la atencion por ser bajo/riesgoso.
# Regla vista en clase: "los colores solo para procesamiento preatentivo"
# -> se usan EXCLUSIVAMENTE para marcar el maximo y el minimo de cada
# grafico, nunca como decoracion de las demas barras.
color_neutro <- "#B6C4C7"
color_acento <- "#02C39A"
color_alerta <- "#E4572E"
color_texto  <- "#2B2B2B"

tema_taller <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title          = element_text(face = "bold", size = 15, color = color_texto),
      plot.subtitle        = element_text(size = 11.5, color = "gray35", margin = margin(b = 10)),
      plot.caption         = element_text(size = 8.5, color = "gray55", hjust = 0),
      axis.text            = element_text(color = color_texto, size = 11),
      axis.title           = element_text(color = "gray35"),
      panel.grid.minor     = element_blank(),
      panel.grid.major.y   = element_blank(),
      legend.position       = "none"
    )
}

# ---------------------
# 2. CARGA DE DATOS BASE
# ---------------------
tenderos_raw <- read_dta(file.path(dir_datos_orig, "TenderosFU03_Publica.dta")) %>%
  mutate(uso_internet = zap_labels(uso_internet))

# base_larga.xlsx no se usa directamente en estas 3 graficas, pero queda
# cargada por si alguno de los 3 integrantes quiere una 4a grafica de
# ciudad x actividad (ver nota de la seccion 6).
base_larga <- read_excel(file.path(dir_datos_deriv, "base_larga.xlsx"))

# ---------------------
# 3. GRAFICO 1 - Penetracion de internet por ciudad
# ---------------------
# Mismo calculo de la Tarea 1 de 03_Analisis.R (se repite aqui porque ese
# resumen no quedo exportado a Excel).
internet_por_ciudad <- tenderos_raw %>%
  group_by(Munic_Dept, Municipio) %>%
  summarise(internet = round(mean(uso_internet, na.rm = TRUE) * 100, 1), .groups = "drop") %>%
  rename(divipola = Munic_Dept)

ciudad_top    <- internet_por_ciudad$Municipio[which.max(internet_por_ciudad$internet)]
ciudad_bottom <- internet_por_ciudad$Municipio[which.min(internet_por_ciudad$internet)]

datos_g1 <- internet_por_ciudad %>%
  mutate(
    Municipio = fct_reorder(Municipio, internet),          # orden por valor, no alfabetico
    resalte = case_when(
      Municipio == ciudad_top    ~ "top",
      Municipio == ciudad_bottom ~ "bottom",
      TRUE ~ "resto"
    )
  )

grafico_ciudad <- ggplot(datos_g1, aes(x = internet, y = Municipio, fill = resalte)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = paste0(internet, "%")), hjust = -0.2, size = 4, color = color_texto) +
  scale_fill_manual(values = c(top = color_acento, bottom = color_alerta, resto = color_neutro)) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = paste0(ciudad_top, " lidera; ", ciudad_bottom, " tiene la menor penetracion"),
    subtitle = "% de tenderos que usan internet para su negocio, por ciudad (n = 1.222 tiendas, 10 ciudades)",
    x = NULL, y = NULL,
    caption = "Fuente: Encuesta Tenderos FU03, Universidad del Rosario."
  ) +
  tema_taller() +
  theme(axis.text.x = element_blank())  # el eje x sobra: ya hay labels con %

ggsave(file.path(dir_outputs_img, "grafico_ciudad.png"), grafico_ciudad,
       width = 9, height = 5, dpi = 300, bg = "white")

# ---------------------
# 4. GRAFICO 2 - Penetracion de internet por actividad economica
# ---------------------
# OJO - CORRECCION IMPORTANTE respecto al script 03_Analisis.R:
# alli, "internet_por_actividad" se calculo agrupando por actG SIN
# filtrar total == 1. Como pivot_longer() deja una fila por tienda POR
# CADA una de las 11 actividades (haya marcado esa actividad o no), el
# group_by(actG) termina promediando uso_internet sobre las MISMAS 1.222
# tiendas en los 11 grupos -> da el mismo 64.6% para las 11 actividades
# (no es informativo). La correccion es la misma logica que ya usaron
# bien en la Tarea 3 (ciudad x actividad): agregar filter(total == 1)
# ANTES de agrupar, para quedarnos solo con las tiendas que de verdad
# ejercen esa actividad.
ren_frame <- tenderos_raw %>%
  rename(Tienda.1 = actG1, ComidaPreparada.2 = actG2, Peluqueria.3 = actG3, Ropa.4 = actG4,
         Otras.5 = actG5, Papeleria.6 = actG6, VidaNocturna.7 = actG7, ProductosInventario.8 = actG8,
         Salud.9 = actG9, Servicios.10 = actG10, Ferreteria.11 = actG11)

col_frame_nal <- ren_frame %>%
  pivot_longer(cols = Tienda.1:Ferreteria.11, names_to = "category", values_to = "total") %>%
  separate(category, c("Actividad", "actG"))

internet_por_actividad <- col_frame_nal %>%
  filter(total == 1) %>%                        # <- LA CORRECCION clave
  group_by(actG, Actividad) %>%
  summarise(internet = round(mean(uso_internet, na.rm = TRUE) * 100, 1),
            n_tiendas = n(), .groups = "drop")

act_top    <- internet_por_actividad$Actividad[which.max(internet_por_actividad$internet)]
act_bottom <- internet_por_actividad$Actividad[which.min(internet_por_actividad$internet)]

datos_g2 <- internet_por_actividad %>%
  mutate(
    Actividad = fct_reorder(Actividad, internet),
    resalte = case_when(
      Actividad == act_top    ~ "top",
      Actividad == act_bottom ~ "bottom",
      TRUE ~ "resto"
    )
  )

grafico_actividad <- ggplot(datos_g2, aes(x = internet, y = Actividad, fill = resalte)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = paste0(internet, "% (n=", n_tiendas, ")")),
            hjust = -0.05, size = 3.6, color = color_texto) +
  scale_fill_manual(values = c(top = color_acento, bottom = color_alerta, resto = color_neutro)) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.28))) +
  labs(
    title    = "Papelerias y peluquerias usan mas internet; las tiendas de barrio, menos",
    subtitle = "% de tenderos que usan internet, por actividad economica (nivel nacional)",
    x = NULL, y = NULL,
    caption = "Fuente: Encuesta Tenderos FU03. El label muestra tambien el n de tiendas por actividad."
  ) +
  tema_taller() +
  theme(axis.text.x = element_blank())

ggsave(file.path(dir_outputs_img, "grafico_actividad.png"), grafico_actividad,
       width = 9, height = 5, dpi = 300, bg = "white")

# ---------------------
# 5. GRAFICO 3 - Internet vs. poblacion de la ciudad
# ---------------------
# Requiere el archivo de poblacion DANE/TerriData (Tarea 4 de 03_Analisis.R).
# AJUSTAR la ruta/nombre segun donde guarden TerriData_Dim2.xlsx.
poblacion_raw <- read_excel(
  file.path(dir_datos_orig, "TerriData_Dim2.xlsx"),
  sheet = "Hoja02"
)

poblacion <- poblacion_raw %>%
  mutate(
    dato_limpio = `Dato Numérico` %>%
      gsub("\\.", "", .) %>%     # quitar puntos de miles
      gsub(",00$", "", .) %>%    # quitar ",00" final
      gsub(",", ".", .) %>%      # coma decimal -> punto
      as.numeric(),
    divipola = as.numeric(`Código Entidad`)
  ) %>%
  filter(
    Año == 2024,
    Indicador %in% c("Población total de hombres", "Población total de mujeres")
  ) %>%
  group_by(divipola) %>%
  summarise(poblacion = sum(dato_limpio, na.rm = TRUE), .groups = "drop")

# Se une a nivel CIUDAD (no ciudad x actividad, ver nota de la seccion 6)
base_ciudad <- internet_por_ciudad %>%
  inner_join(poblacion, by = "divipola") %>%
  mutate(pob_millones = round(poblacion / 1e6, 2))

# Correlacion simple para respaldar el mensaje de la diapositiva
correlacion <- round(cor(base_ciudad$pob_millones, base_ciudad$internet, use = "complete.obs"), 2)
cat("Correlacion poblacion-internet:", correlacion, "\n")

grafico_poblacion <- ggplot(base_ciudad, aes(x = pob_millones, y = internet)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray65", linetype = "dashed", linewidth = 0.6) +
  geom_point(color = color_acento, size = 4.2) +
  geom_text_repel(aes(label = Municipio), size = 3.6, color = color_texto,
                   seed = 42, max.overlaps = 15) +
  scale_x_continuous(labels = label_number(suffix = " M")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title    = "La penetracion de internet no depende claramente del tamano de la ciudad",
    subtitle = paste0("% de tenderos con internet vs. poblacion municipal, DANE 2024  (r = ", correlacion, ")"),
    x = "Poblacion (millones de habitantes)",
    y = "% de tenderos con uso de internet",
    caption = "Fuente: Encuesta Tenderos FU03 + TerriData (DANE), Dim2, 2024."
  ) +
  tema_taller()

ggsave(file.path(dir_outputs_img, "grafico_poblacion.png"), grafico_poblacion,
       width = 9, height = 5, dpi = 300, bg = "white")

# ---------------------
# 6. NOTA METODOLOGICA (para el repo / apendice, no para la diapositiva)
# ---------------------
# Al revisar "base_larga" (ciudad x actividad, Tarea 3), 24 de las 108
# celdas tienen menos de 5 tiendas (varias con n=1 o n=2), lo que da
# porcentajes de 0% o 100% poco confiables. Por eso el Grafico 3 se hizo
# a nivel "ciudad" y no "ciudad x actividad": agrupa suficientes tiendas
# para que el mensaje sea robusto (regla de "cuidado con el bigote /
# la incertidumbre" de la diapositiva de bar charts). Si alguno de
# ustedes quiere una 4a grafica cruzando ciudad y actividad, se
# recomienda filtrar antes las celdas con n_tiendas < 5 o usar el
# tamano del punto (n_tiendas) para mostrar que tan confiable es cada
# cifra -- eso mismo se puede leer directo de "base_larga".

# ---------------------
# 7. RESUMEN RAPIDO PARA EL GUION DE LA PRESENTACION
# ---------------------
cat("\n--- RESUMEN PARA LAS DIAPOSITIVAS ---\n")
cat("Ciudad con mayor penetracion :", ciudad_top,
    paste0("(", max(internet_por_ciudad$internet), "%)\n"))
cat("Ciudad con menor penetracion :", ciudad_bottom,
    paste0("(", min(internet_por_ciudad$internet), "%)\n"))
cat("Actividad con mayor uso      :", act_top,
    paste0("(", max(internet_por_actividad$internet), "%)\n"))
cat("Actividad con menor uso      :", act_bottom,
    paste0("(", min(internet_por_actividad$internet), "%)\n"))
cat("Correlacion poblacion-internet:", correlacion, "\n")
cat("\nGraficos guardados en:", dir_outputs_img, "\n")
