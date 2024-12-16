# Paso 1: Configuracion inicial
# Definir vectores
energia <- c(rep("renovable", 10), rep("no_renovable", 10))
consumo <- c(15.3, 14.8, NA, 16.2, 14.0, NA, 13.7, 15.8, 14.5, 15.0,
             20.3, 19.8, 21.0, NA, 20.5, 19.7, NA, 20.1, 21.2, 20.0)
costo_kwh <- c(rep(0.12, 10), rep(0.15, 10))

# Paso 2: Limpieza de datos
# Reemplazar NA en "consumo" con la mediana del consumo por tipo de energ?a
mediana_renovable <- median(consumo[energia == "renovable"], na.rm = TRUE)
mediana_no_renovable <- median(consumo[energia == "no_renovable"], na.rm = TRUE)

consumo[is.na(consumo) & energia == "renovable"] <- mediana_renovable
consumo[is.na(consumo) & energia == "no_renovable"] <- mediana_no_renovable

# Paso 3: Crear el dataframe
df_consumo <- data.frame(energia, consumo, costo_kwh)

# Paso 4: Calculos
# Agregar columna "costo_total"
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Calcular totales y medias
total_consumo <- aggregate(consumo ~ energia, data = df_consumo, sum)
total_costo <- aggregate(costo_total ~ energia, data = df_consumo, sum)
media_consumo <- aggregate(consumo ~ energia, data = df_consumo, mean)

# Agregar columna "ganancia"
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: Resumen
# Ordenar el dataframe por "costo_total" en orden descendente
df_ordenado <- df_consumo[order(-df_consumo$costo_total), ]

# Extraer las tres filas con mayor "costo_total"
top_3_costos <- head(df_ordenado, 3)

# Crear la lista resumen
resumen_energia <- list(
  "Dataframe Ordenado" = df_ordenado,
  "Total Consumo por Tipo" = total_consumo,
  "Total Costo por Tipo" = total_costo,
  "Top 3 Costos" = top_3_costos
)

# Mostrar el resumen
print(resumen_energia)

