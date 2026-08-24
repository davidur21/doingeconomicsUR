# =============================================================
# Analisis penetracion de internet - tenderos + poblacion DANE
# =============================================================
# Ajustado para la estructura de carpetas del repositorio:
#   Datos/Originales/  -> datos crudos (TenderosFU03_Publica.dta, TerriData_Dim2.xlsx)
#   Datos/Derivados/   -> aqui se guardan los 3 Excel que produce este script
# Corran este script desde el .Rproj ubicado en la raiz del repositorio.
# =============================================================
library(dplyr)
library(tidyr)
library(scales)
library(readxl)
library(haven)
library(writexl)
# ---------------------
# CARGA DE DATOS
# ---------------------
tenderos_raw <- read_dta("Datos/Originales/TenderosFU03_Publica.dta")
# Hoja02 (no la default) trae los totales de poblacion H+M
poblacion_raw <- read_excel(
  "Datos/Originales/TerriData_Dim2.xlsx",
  sheet = "Hoja02"
)
# uso_internet viene con etiquetas de Stata; las quitamos para operar como numero
tenderos_raw <- tenderos_raw %>%
  mutate(uso_internet = zap_labels(uso_internet))
# ---------------------
# TAREA 1: % de uso de internet por ciudad
# ---------------------
internet_por_ciudad <- tenderos_raw %>%
  group_by(Munic_Dept, Municipio) %>%
  summarise(internet = round(mean(uso_internet, na.rm = TRUE) * 100, 1)) %>%
  ungroup() %>%
  rename(divipola = Munic_Dept)
# ---------------------
# TAREA 2: uso de internet por actividad economica (nivel nacional)
# ---------------------
# Renombramos actG1..actG11 con nombres descriptivos (num al final = actG)
ren_frame <- tenderos_raw %>%
  rename(Tienda.1 = actG1, ComidaPreparada.2 = actG2, Peluqueria.3 = actG3, Ropa.4 = actG4,
         Otras.5 = actG5, Papeleria.6 = actG6, vidanocturna.7 = actG7, productosinventario.8 = actG8,
         salud.9 = actG9, servicios.10 = actG10, ferreteria.11 = actG11)
# RESHAPE (melt): de 11 columnas dummy a una columna "Actividad" + "total" (0/1)
col_frame <- ren_frame %>%
  pivot_longer(cols = Tienda.1:ferreteria.11, names_to = "category", values_to = "total") %>%
  separate(category, c("Actividad", "actG"))  # separa nombre.numero -> 2 columnas
# COLLAPSE: % de internet por actividad (todas las tiendas, sin filtrar total==1
# porque aqui estamos promediando uso_internet, no dependemos de la dummy actG)
internet_por_actividad <- col_frame %>%
  group_by(actG, Actividad) %>%
  summarise(internet = round(mean(uso_internet, na.rm = TRUE) * 100, 1))
# ---------------------
# TAREA 3: uso de internet por ciudad x actividad (solo donde aplica la actividad)
# ---------------------
internet_por_ciudad_actividad <- col_frame %>%
  filter(total == 1) %>%                     # solo tiendas que SI tienen esa actividad
  group_by(Munic_Dept, Municipio, actG, Actividad) %>%
  summarise(internet = round(mean(uso_internet, na.rm = TRUE) * 100, 0)) %>%  # *100 = %, no dummy 0/1
  rename(divipola = Munic_Dept) %>%
  arrange(Municipio, actG)
# ---------------------
# TAREA 4: poblacion total (hombres + mujeres) por ciudad, DANE 2024
# ---------------------
# Limpieza: "1.949.253,00" (texto DANE) -> 1949253 (numero)
poblacion_limpia <- poblacion_raw %>%
  mutate(
    `Dato Numérico Limpio` = `Dato Numérico` %>%
      gsub("\\.", "", .) %>%     # quitar puntos de miles
      gsub(",00$", "", .) %>%    # quitar ",00" final
      gsub(",", ".", .) %>%      # coma decimal -> punto
      as.numeric(),
    divipola = as.numeric(`Código Entidad`)
  )
# COLLAPSE: sumar hombres + mujeres = poblacion total por divipola
poblacion <- poblacion_limpia %>%
  filter(
    Año == 2024,
    Indicador %in% c("Población total de hombres", "Población total de mujeres")
  ) %>%
  group_by(divipola) %>%
  summarise(poblacion = sum(`Dato Numérico Limpio`, na.rm = TRUE))
# ---------------------
# TAREA 5: MERGE/JOIN de poblacion + internet por ciudad x actividad
# ---------------------
base_final_internet_poblacion <- merge(
  poblacion, internet_por_ciudad_actividad,
  by.x = "divipola", by.y = "divipola"
)
# ---------------------
# TAREA 6: base LARGA y base EXTENSA (para PowerBI / graficos)
# ---------------------
# BASE LARGA: 1 fila por ciudad x actividad (ya es el formato de base_final_internet_poblacion)
base_larga <- base_final_internet_poblacion %>%
  mutate(pob_millones = round(poblacion / 1e6, 1)) %>%
  select(divipola, municipio = Municipio, actG, Actividad, internet, pob_millones) %>%
  arrange(divipola, actG)
# BASE EXTENSA (CAST/pivot_wider): 1 fila por ciudad, 1 columna por actividad
base_extensa <- base_larga %>%
  select(divipola, municipio, pob_millones, actG, internet) %>%
  pivot_wider(names_from = actG, values_from = internet, names_prefix = "internet_") %>%
  arrange(divipola)
# ---------------------
# EXPORTAR
# ---------------------
write_xlsx(internet_por_ciudad_actividad, "Datos/Derivados/internet_por_ciudad_actividad.xlsx")
write_xlsx(base_larga, "Datos/Derivados/base_larga.xlsx")
write_xlsx(base_extensa, "Datos/Derivados/base_extensa.xlsx")
