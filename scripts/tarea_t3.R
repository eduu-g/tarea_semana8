
# T3 — Regresión económica: CASEN y ENE
#
# Autor: Eduardo Guerrero
#
# Fecha: 13-10-2026
#
#
# Pregunta para ENE:
# «Entre las personas ocupadas, ¿ser mujer se asocia con una probabilidad distinta
# de empleo informal y cuánto cambia esa diferencia al controlar por edad,
# educación y región?»
#
# Todas las interpretaciones describen asociaciones; no afirman causalidad.


library(dplyr)
library(broom)
library(modelsummary)

# CASEN


# A1. Leer CASEN y crear experiencia
# Se conserva el número de fila original para identificar observaciones influyentes.
casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(
    fila_casen = row_number(),
    genero = factor(genero, levels = c("F", "M")),
    experiencia = pmax(edad - educ - 6, 0)
  )

# Auditoría inicial: CASEN tiene 60 filas y cinco NA en ingreso.
str(casen)
colSums(is.na(casen))
table(casen$genero, useNA = "ifany")

# m1 estima la asociación simple entre educación e ingreso mensual.
# m2 agrega experiencia y género como controles.
m1 <- lm(ingreso ~ educ, data = casen)
m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)

# A2. Tabla de coeficientes de m2 en un solo pipe, sin intercepto.
# Incluye estimación, IC del 95 %, p-value e indicador de significancia al 5 %.

tabla_m2 <- tidy(m2, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(significativo_5 = p.value < 0.05) |>
  arrange(p.value) |>
  mutate(
    across(c(estimate, conf.low, conf.high), ~ round(.x, 2)),
    p.value = signif(p.value, 3)
  )

print(tabla_m2)

# A3. Tercer modelo de ejemplo: agrega experiencia al cuadrado.
# I() indica que experiencia^2 se trata como una operación matemática.
m3 <- lm(
  ingreso ~ educ + experiencia + I(experiencia^2) + genero,
  data = casen
)

modelos_casen <- list(
  simple = m1,
  controles = m2,
  experiencia_cuadratica = m3
)

comparacion_casen <- lapply(modelos_casen, glance) |>
  bind_rows(.id = "modelo") |>
  select(modelo, nobs, r.squared, adj.r.squared, AIC, BIC)

print(comparacion_casen)

# RESPUESTA A3 — m2 tiene R² ≈ 0,4750 y R² ajustado ≈ 0,4441; m3 tiene R² ≈ 0,4813,
# pero su R² ajustado baja a ≈ 0,4398. El término cuadrático eleva poco el R², mientras
# que el R² ajustado "castiga" la complejidad añadida; en este ejemplo, m3 no mejora claramente.

# A4. Coeficiente de educación en los tres modelos.
coef_educacion <- bind_rows(
  simple = tidy(m1),
  controles = tidy(m2),
  experiencia_cuadratica = tidy(m3),
  .id = "modelo"
) |>
  filter(term == "educ") |>
  select(modelo, estimate, std.error, p.value)

print(coef_educacion)

b_educ_m1 <- tidy(m1) |> filter(term == "educ") |> pull(estimate)
b_educ_m2 <- tidy(m2) |> filter(term == "educ") |> pull(estimate)
b_educ_m3 <- tidy(m3) |> filter(term == "educ") |> pull(estimate)
cambio_educ_m1_m2 <- b_educ_m2 - b_educ_m1
print(cambio_educ_m1_m2)

# RESPUESTA A4 — cambio del coeficiente de educación:
# El coeficiente de educ es aproximadamente $42.079,87 en m1, $42.245,76 en m2
# y $42.690,21 en m3. Como los tres modelos tienen ingreso en pesos como variable
# dependiente, los coeficientes de educación comparten la misma unidad. Al pasar
# de m1 a m2, la estimación aumenta cerca de $165,90 por año adicional de educación.
# Un control puede mover ese coeficiente cuando está relacionado tanto con educación
# como con ingreso.

# A5. Diagnóstico de m2 mediante augment().

casen_muestra_m2 <- casen |>
  filter(!is.na(ingreso))

diagnostico_m2 <- augment(m2, data = casen_muestra_m2)

# A5.1. Las tres observaciones peor predichas, ordenadas por residuo absoluto.

peor_predichas <- diagnostico_m2 |>
  filter(!is.na(.resid)) |>
  arrange(desc(abs(.resid))) |>
  filter(row_number() <= 3) |>
  select(fila_casen, ingreso, educ, edad, genero, .fitted, .resid)

print(peor_predichas)

# RESPUESTA A5.1 — observaciones peor predichas:
# Las tres filas son 47, 51 y 40. En las tres, el ingreso observado queda por debajo
# de lo pronosticado por m2: los residuos son aproximadamente -$298.210, -$296.393
# y -$249.293. Las tres observaciones corresponden a hombres con educación de 13,
# 9 y 17 años, respectivamente. Esos casos muestran que el modelo sobreestima sus
# ingresos individuales; no se puede deducir una causa a partir de esos residuos.

# A5.2. Máxima distancia de Cook y máximo apalancamiento.
resumen_influencia <- diagnostico_m2 |>
  summarise(
    max_cooksd = max(.cooksd, na.rm = TRUE),
    max_hat = max(.hat, na.rm = TRUE)
  )

print(resumen_influencia)

caso_mayor_hat <- diagnostico_m2 |>
  slice_max(order_by = .hat, n = 1, with_ties = FALSE) |>
  select(fila_casen, .hat)
print(caso_mayor_hat)

caso_mas_influyente <- diagnostico_m2 |>
  filter(!is.na(.cooksd)) |>
  slice_max(order_by = .cooksd, n = 1, with_ties = FALSE) |>
  select(fila_casen, .cooksd, .hat)

caso_mas_influyente

# RESPUESTA A5.2 — influencia y apalancamiento:
# La mayor distancia de Cook es aproximadamente 0,0869 y corresponde a la fila 43.
# El máximo apalancamiento es aproximadamente 0,1752 y corresponde a la fila 27.
# Son diagnósticos distintos: Cook combina información del residuo y del leverage;
# .hat indica cuán extremo es el caso en el espacio de variables explicativas.

# A5.3. Re-estimar m2 sin la observación con mayor distancia de Cook.
fila_eliminar <- caso_mas_influyente$fila_casen

casen_sin_influyente <- casen |>
  filter(fila_casen != fila_eliminar)

m2_sin_influyente <- lm(
  ingreso ~ educ + experiencia + genero,
  data = casen_sin_influyente
)

comparacion_influencia <- bind_rows(
  original = tidy(m2),
  sin_caso_influyente = tidy(m2_sin_influyente),
  .id = "modelo"
) |>
  filter(term == "educ") |>
  select(modelo, estimate, std.error, p.value)

print(comparacion_influencia)

coef_educ_original <- tidy(m2) |> filter(term == "educ") |> pull(estimate)
coef_educ_sin_caso <- tidy(m2_sin_influyente) |> filter(term == "educ") |> pull(estimate)
cambio_educ_pesos <- coef_educ_sin_caso - coef_educ_original
cambio_educ_porcentaje <- 100 * cambio_educ_pesos / coef_educ_original
print(cambio_educ_pesos)
print(cambio_educ_porcentaje)

# RESPUESTA A5.3 — sensibilidad a la observación más influyente:
# Al quitar la fila 43, el coeficiente de educación pasa de aproximadamente $42.246
# a $45.218, un aumento de unos $2.972 (cerca de 7,0 %). La estimación aun es
# positiva, pero el tamaño del coeficiente depende parcialmente de una observación.
# Signfica que debemos tener cuidado al interpretar resultados con solo 55 observaciones válidas.

# A6. Estimaciones e intervalos usados en la interpretación de m2.

coef_educ_m2 <- tidy(m2, conf.int = TRUE) |>
  filter(term == "educ")
coef_genero_m2 <- tidy(m2, conf.int = TRUE) |>
  filter(term == "generoM")

print(coef_educ_m2)
print(coef_genero_m2)

# RESPUESTA A6 — interpretación de educación y género:
# En el modelo con controles, cada año adicional de educación se asocia con cerca
# de $42.246 más de ingreso mensual, en promedio, manteniendo experiencia y género
# constantes (IC del 95 %: aproximadamente $29.487 a $55.004). El intervalo no
# incluye cero y el p-value es menor que 0,05: la asociación estimada es estadísticamente
# significativa. El coeficiente de generoM es aproximadamente $55.686: en esta muestra,
# el ingreso mensual estimado de los hombres es mayor que el de las mujeres, manteniendo
# educación y experiencia constantes. Sin embargo, p ≈ 0,204 y su IC del 95 % va de
# aproximadamente -$31.246 a $142.618, incluyendo cero. Por eso no hay evidencia
# estadística suficiente al 5 % para afirmar una diferencia de género en este modelo.
# Aunque $55.686 es una diferencia puntual considerable, con n = 55 hay incertidumbre
# alta; significancia estadística e importancia económica no son lo mismo.

# PARTE B — ENE: PREGUNTA, AUDITORÍA Y MODELOS

# B1. Pregunta: 
# «Entre las personas ocupadas, ¿ser mujer se asocia con una probabilidad distinta
# de empleo informal y cuánto cambia esa diferencia al controlar por edad,
# educación y región?»
# La pregunta es asociativa. No significa que ser mujer cause informalidad.

# B2

ene <- read.csv2("data/raw/ene-2026-06-mjj.csv") |>
  select(activ, ocup_form, sexo, edad, nivel, region, fact_cal)

# Auditoría de las variables que requiere la pregunta.
str(ene)
colSums(is.na(ene))
summary(ene$edad)
summary(ene$fact_cal)

table(ene$activ, useNA = "ifany")
table(ene$ocup_form, useNA = "ifany")
table(ene$sexo, useNA = "ifany")
table(ene$nivel, useNA = "ifany")
table(ene$region, useNA = "ifany")

# RESPUESTA B2 — lectura de códigos y faltantes:
# La base original tiene 97.946 filas y 222 columnas. activ == 1 identifica a las
# personas ocupadas; ocup_form codifica 1 = formal y 2 = informal. En la base total,
# los NA de ocup_form son estructurales para las personas que no están ocupadas,
# porque la formalidad del empleo no se define para ellas. Tras filtrar activ == 1,
# la base tiene 41.850 ocupados. sexo usa 1 = hombre y 2 = mujer. La variable nivel
# incluye 88 = no sabe y 99 = no responde; no son niveles educativos numéricos válidos,
# así que se convierten en NA antes de estimar. fact_cal es el factor de expansión
# calibrado y se utiliza como ponderador. region se recodifica como factor textual,
# nunca como escala numérica.

# Nombres de regiones en el orden de códigos utilizado por el manual/laboratorio.
regiones <- c(
  "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
  "O'Higgins", "Maule", "Biobío", "La Araucanía", "Los Lagos",
  "Aysén", "Magallanes", "Metropolitana", "Los Ríos",
  "Arica y Parinacota", "Ñuble"
)

# Filtrar ocupados y recodificar variables según el manual del INE.
ocupados_pre <- ene |>
  filter(activ == 1) |>
  mutate(
    informal = case_when(
      ocup_form == 1 ~ FALSE,
      ocup_form == 2 ~ TRUE,
      TRUE ~ NA
    ),
    mujer = factor(
      case_when(
        sexo == 1 ~ "Hombre",
        sexo == 2 ~ "Mujer",
        TRUE ~ NA_character_
      ),
      levels = c("Hombre", "Mujer")
    ),
    educacion = case_when(
      is.na(nivel) ~ NA_character_,
      nivel %in% c(88, 99) ~ NA_character_,
      nivel == 0 ~ "Sin estudios",
      nivel %in% c(1, 2) ~ "Inicial",
      nivel == 3 ~ "Básica",
      nivel %in% c(4, 5, 6) ~ "Media",
      nivel %in% c(7, 8, 9) ~ "Superior",
      nivel %in% c(10, 11, 12) ~ "Posgrado",
      nivel %in% c(13, 14) ~ "Especial/normalista",
      TRUE ~ NA_character_
    ),
    educacion = factor(
      educacion,
      levels = c("Media", "Sin estudios", "Inicial", "Básica", "Superior",
                 "Posgrado", "Especial/normalista")
    ),
    region_txt = factor(regiones[region], levels = regiones)
  )

# Confirmar las distribuciones posteriores a la recodificación.
table(ocupados_pre$informal, useNA = "ifany")
table(ocupados_pre$mujer, useNA = "ifany")
table(ocupados_pre$educacion, useNA = "ifany")
table(ocupados_pre$region_txt, useNA = "ifany")
colSums(is.na(ocupados_pre[, c("informal", "mujer", "edad", "educacion", "region_txt", "fact_cal")]))

# Para ambos modelos se utiliza la misma muestra completa. Solo se excluyen casos
# sin la respuesta o variables explicativas requeridas, y pesos no válidos.
ocupados_modelo <- ocupados_pre |>
  filter(
    !is.na(informal), !is.na(mujer), !is.na(edad), !is.na(educacion),
    !is.na(region_txt), !is.na(fact_cal), fact_cal > 0
  )

resumen_muestra_ene <- data.frame(
  etapa = c("Personas ocupadas antes de excluir faltantes educativos",
            "Muestra analítica común a los modelos"),
  filas = c(nrow(ocupados_pre), nrow(ocupados_modelo))
)
print(resumen_muestra_ene)

# RESPUESTA sobre muestra analítica:
# De los 41.850 ocupados, 18 no tienen una categoría educativa válida porque
# registran no sabe/no responde; por eso los dos modelos comparables usan 41.832
# observaciones. Los faltantes de nivel son respuestas no disponibles, no una
# categoría educativa que pueda tratarse como número.

# B3. Modelo base ponderado: asociación entre ser mujer y la probabilidad de empleo informal.
modelo_ene_base <- lm(
  informal ~ mujer,
  data = ocupados_modelo,
  weights = fact_cal
)

# Modelo con controles ponderado: añade edad, edad al cuadrado, educación y región.
# Educación y región son controles adicionales que no estaban en el modelo fijo del laboratorio.
modelo_ene_controles <- lm(
  informal ~ mujer + edad + I(edad^2) + educacion + region_txt,
  data = ocupados_modelo,
  weights = fact_cal
)

print(tidy(modelo_ene_base, conf.int = TRUE))
print(tidy(modelo_ene_controles, conf.int = TRUE))

# B4. Comparar el coeficiente de mujer entre ambos modelos, en puntos porcentuales.
comparacion_mujer_ene <- bind_rows(
  base_ponderado = tidy(modelo_ene_base, conf.int = TRUE),
  controles_ponderado = tidy(modelo_ene_controles, conf.int = TRUE),
  .id = "modelo"
) |>
  filter(term == "mujerMujer") |>
  mutate(across(c(estimate, conf.low, conf.high), ~ 100 * .x)) |>
  mutate(across(c(estimate, conf.low, conf.high), ~ round(.x, 2))) |>
  mutate(p.value = signif(p.value, 3)) |>
  select(modelo, term, estimate, conf.low, conf.high, p.value)

print(comparacion_mujer_ene)

comparacion_modelos_ene <- lapply(
  list(base_ponderado = modelo_ene_base,
       controles_ponderado = modelo_ene_controles),
  glance
) |>
  bind_rows(.id = "modelo") |>
  select(modelo, nobs, r.squared, adj.r.squared, AIC, BIC)

print(comparacion_modelos_ene)

# B4. Alternativa elegida: regresión por región con intervalo de confianza del 95 %.
# Cada modelo regional controla por edad, edad al cuadrado y educación, y usa fact_cal.
brecha_region <- ocupados_modelo |>
  group_by(region_txt) |>
  group_modify(~ {
    modelo_region <- lm(
      informal ~ mujer + edad + I(edad^2) + educacion,
      data = .x,
      weights = fact_cal
    )
    tidy(modelo_region, conf.int = TRUE) |>
      filter(term == "mujerMujer") |>
      mutate(n = nobs(modelo_region))
  }) |>
  ungroup() |>
  mutate(
    estimacion_pp = round(100 * estimate, 2),
    limite_inf_pp = round(100 * conf.low, 2),
    limite_sup_pp = round(100 * conf.high, 2),
    IC_incluye_cero = limite_inf_pp <= 0 & limite_sup_pp >= 0
  ) |>
  select(region_txt, n, estimacion_pp, limite_inf_pp, limite_sup_pp, p.value, IC_incluye_cero) |>
  arrange(desc(estimacion_pp))

print(brecha_region)

regiones_ic_cero <- brecha_region |>
  filter(IC_incluye_cero) |>
  select(region_txt, estimacion_pp, limite_inf_pp, limite_sup_pp)
print(regiones_ic_cero)

# RESPUESTA B4 — heterogeneidad regional:
# La estimación puntual de la diferencia mujer-hombre es mayor en
# Atacama (≈ 13,54 pp; IC 95 %: 9,33 a 17,76) y menor en Los Ríos (≈ 2,47 pp; IC
# 95 %: -1,96 a 6,91). En Los Ríos, el intervalo incluye cero; por lo tanto, la
# estimación regional es menos precisa y no se distingue estadísticamente de cero
# al 5 %. Estas regresiones describen estimaciones por región; no constituyen por
# sí solas una prueba formal de que los coeficientes regionales sean diferentes.

# B5. Sensibilidad: comparar el modelo con controles ponderado y sin ponderar.
# Se mantiene la misma especificación y la misma muestra; cambia solo el peso.
modelo_ene_controles_sin_peso <- lm(
  informal ~ mujer + edad + I(edad^2) + educacion + region_txt,
  data = ocupados_modelo
)

sensibilidad_ponderacion <- bind_rows(
  ponderado = tidy(modelo_ene_controles, conf.int = TRUE),
  sin_ponderar = tidy(modelo_ene_controles_sin_peso, conf.int = TRUE),
  .id = "modelo"
) |>
  filter(term == "mujerMujer") |>
  mutate(
    estimate_pp = 100 * estimate,
    conf_low_pp = 100 * conf.low,
    conf_high_pp = 100 * conf.high
  ) |>
  mutate(across(c(estimate_pp, conf_low_pp, conf_high_pp), ~ round(.x, 2))) |>
  select(modelo, estimate_pp, conf_low_pp, conf_high_pp, p.value)

print(sensibilidad_ponderacion)

coef_ponderado_pp <- sensibilidad_ponderacion |>
  filter(modelo == "ponderado") |>
  pull(estimate_pp)
coef_sin_ponderar_pp <- sensibilidad_ponderacion |>
  filter(modelo == "sin_ponderar") |>
  pull(estimate_pp)
cambio_peso_pp <- coef_sin_ponderar_pp - coef_ponderado_pp
print(cambio_peso_pp)

# RESPUESTA B5 — sensibilidad a la ponderación:
# El coeficiente de mujer es aproximadamente 5,94 pp con fact_cal y 6,47 pp sin
# ponderar. Al retirar el ponderador, la estimación aumenta alrededor de 0,53 pp.
# La conclusión descriptiva mantiene el signo positivo, pero la magnitud cambia;
# por el diseño de la ENE es mejor elegir la estimación ponderada para hablar
# de la población objetivo, reconociendo la limitación de los errores estándar.

# B6. INTERPRETACIÓN COMPLETA
# 1. En el modelo base ponderado, ser mujer se asocia con una probabilidad de informalidad 3,21 pp mayor que la de los hombres (IC 95 %: 2,36 a 4,06; p < 0,001).
# 2. Al controlar por edad, edad al cuadrado, educación y región, la diferencia estimada aumenta a 5,94 pp (IC 95 %: 5,13 a 6,76; p < 0,001).
# 3. La diferencia aumenta 2,73 pp; el segundo modelo compara mujeres y hombres con igual edad, educación y región, por lo que responde una pregunta condicionada.
# 4. El R² pasa de 0,0013 en el modelo base a 0,0999 con controles: se explica más variación, aunque aproximadamente el 90 % sigue sin explicar.
# 5. Por región, la estimación es 13,55 pp en Atacama y 2,52 pp en Los Ríos; en Los Ríos el IC incluye cero y no se probó formalmente la diferencia entre regiones.
# 6. Sin ponderar, la asociación estimada es 6,47 pp frente a 5,94 pp ponderado: retirar fact_cal aumenta el coeficiente aproximadamente 0,53 pp.
# 7. Una diferencia de cerca de 6 pp puede ser descriptivamente relevante y los p-values son pequeños, pero la significancia no demuestra causalidad.
# 8. Limitaciones: lm(weights = fact_cal) no incorpora el diseño muestral complejo en sus errores estándar y pueden existir variables omitidas.


# PARTE C — TABLAS Y EXPORTACIÓN


dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# Tabla comparativa de los modelos ENE: base ponderado, controles ponderado y
# sensibilidad sin ponderar. output con extensión .csv escribe directamente el
# archivo y evita abrir el visor predeterminado de RStudio.
modelos_ene <- list(
  "Base (ponderado)" = modelo_ene_base,
  "Controles (ponderado)" = modelo_ene_controles,
  "Controles (sin ponderar)" = modelo_ene_controles_sin_peso
)

modelsummary(
  modelos_ene,
  stars = TRUE,
  gof_map = c("nobs", "r.squared"),
  output = "data/processed/t3_tabla_ene.csv"
)

# Exportar la tabla completa de coeficientes de m2 de CASEN, con IC del 95 %.
tidy(m2, conf.int = TRUE) |>
  write.csv("data/processed/t3_coeficientes_casen.csv", row.names = FALSE)


