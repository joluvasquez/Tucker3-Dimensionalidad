# Tucker3-Dimensionalidad-simulación

Simulación Monte Carlo en R para comparar **DIFFIT** y **scree plot multiway** en la selección de los rangos de un modelo Tucker3. Ambos criterios se aplican a la misma tabla de modelos candidatos para cada tensor, lo que permite comparar sus aciertos de forma emparejada.

## Comparaciones V1–V2 y análisis controlados (octubre de 2026)

La carpeta [comparisons](comparisons) contiene los resultados, el código y un paquete completo para reproducir las selecciones sobre **5.400 conjuntos de ajustes guardados (199.800 modelos candidatos)**. Incluye comparaciones DIFFIT/scree V1–V2, ocho variantes controladas de DIFFIT, análisis del espaciado del scree, intervalos Monte Carlo pareados y comprobaciones de la aplicación macroeconómica.

**[Guía y resultados](comparisons/README.md)** · **[Paquete reproducible](comparisons/Tucker3_reproducibility.zip)** · **[Excel de comparaciones controladas](comparisons/Comparaciones_controladas_Tucker3.xlsx)**

V1 identifica la implementación inicial de los autores; V2, las reglas alternativas implementadas. Estas etiquetas no implican superioridad ni equivalencia automática con una formulación bibliográfica. Los scripts de reproducción reutilizan los mismos ajustes y no vuelven a ejecutar ALS. Los resultados se limitan al diseño estudiado.

## Contenido

- [Script documentado](simulacion_tucker3-dimensionalidad.R): programa autónomo de simulación, ajuste, análisis y exportación, con comentarios en español.
- `README.md`: guía de ejecución y resultados.

La versión documentada conserva las 101 expresiones de R del archivo original: se verificó su identidad con `parse(..., keep.source = FALSE)`. Esa comprobación no equivale a ejecutar toda la simulación ni valida sus conclusiones estadísticas. Los resultados precalculados y sus verificaciones se incorporan ahora en `comparisons`.

## Requisitos

R y el paquete **openxlsx**; RStudio es opcional. Instala la dependencia una sola vez:

```r
install.packages("openxlsx")
```

Descarga el repositorio con **Code → Download ZIP** o clónalo:

```sh
git clone https://github.com/joluvasquez/Tucker3-Dimensionalidad.git
cd Tucker3-Dimensionalidad
```

En RStudio, establece la carpeta descargada como directorio de trabajo. Puedes comprobarla con `getwd()`.

## Primera ejecución: piloto reducido

Ejecuta estos comandos en R, desde la carpeta del repositorio:

```r
Sys.setenv(
  RUN_MODE = "pilot",
  N_REP = "2",
  N_STARTS_ALS = "1",
  MAX_ITER_ALS = "50",
  TOL_ALS = "1e-7"
)

source(
  "simulacion_tucker3-dimensionalidad.R",
  encoding = "UTF-8"
)
```

Este ejemplo procesa **6 escenarios × 2 réplicas = 12 tensores**, con ajustes reducidos para una prueba técnica. No corresponde al diseño completo ni garantiza convergencia. El programa comienza a simular al ejecutar `source()`; no se limita a definir funciones.

El piloto conserva los tres núcleos, pero utiliza solo el tamaño **50 × 10 × 5**, el error **14%** y los patrones aleatorio y agrupado con **2 grupos**. Seleccionar `RUN_MODE="pilot"` por sí solo no reduce las réplicas ni los inicios.

## Ejecución completa

Las variables definidas con `Sys.setenv()` persisten en la sesión. Para recuperar los valores predeterminados y ejecutar el diseño completo:

```r
Sys.unsetenv(c(
  "RUN_MODE", "N_REP", "N_STARTS_ALS", "MAX_ITER_ALS",
  "TOL_ALS", "MC_SEED", "MC_OUTPUT_DIR", "SAVE_SIMULATED_ARRAYS"
))

source(
  "simulacion_tucker3-dimensionalidad.R",
  encoding = "UTF-8"
)
```

El diseño completo procesa **108 escenarios × 50 réplicas = 5400 tensores**, con **20 inicios por modelo** y hasta **500 iteraciones por inicio**. El cálculo es secuencial y puede requerir un tiempo considerable; no se establece aquí una duración estimada.

## Configuración

Los parámetros se leen desde variables de entorno antes de iniciar el experimento.

| Variable | Valor predeterminado | Función |
| --- | --- | --- |
| `MC_SEED` | `2026` | Semilla del generador aleatorio |
| `N_REP` | `50` | Réplicas por escenario; mínimo 2 |
| `N_STARTS_ALS` | `20` | Inicios por candidato; mínimo 1 |
| `MAX_ITER_ALS` | `500` | Límite de iteraciones por inicio |
| `TOL_ALS` | `1e-7` | Cambio absoluto de FIT, en puntos porcentuales |
| `RUN_MODE` | `full` | `full` o `pilot` |
| `SAVE_SIMULATED_ARRAYS` | `false` | `true` guarda factores, señal, ruido y datos |
| `MC_OUTPUT_DIR` | Carpeta con modo y fecha/hora | Destino de resultados; debe ser nuevo |

Por ejemplo, antes de `source()`:

```r
Sys.setenv(
  MC_OUTPUT_DIR = "resultados_experimento_01",
  SAVE_SIMULATED_ARRAYS = "true"
)
```

La carpeta se crea automáticamente. Si ya existe, el programa se detiene. Guardar todos los tensores aumenta el consumo de memoria y espacio. La semilla es global: cambiar el diseño, su orden o los inicios cambia también las realizaciones posteriores.

## Diseño de la simulación

| Factor | Niveles |
| --- | --- |
| Tamaño `(I,J,K)` | `(50,10,5)`, `(100,10,5)`, `(25,25,25)` |
| Rangos generadores `(P,Q,R)` | `(2,2,2)`, `(3,2,2)`, `(3,3,3)` |
| Error controlado | 14%, 39%, 59% |
| Patrón del modo A | Aleatorio o agrupado |
| Grupos dentro del patrón agrupado | 2, 3, 4 |

Hay 27 escenarios aleatorios y 81 agrupados. El número de grupos está anidado en el patrón agrupado; no se aplica al aleatorio.

La señal se genera como `X_true = G ×₁ A ×₂ B ×₃ C`. El núcleo es disperso y se verifica su rango multilineal. Los factores B y C se ortogonalizan; A conserva su escala y, cuando corresponde, su estructura de grupos. Los datos observados son `X = X_true + E`, sin centrado ni escalado.

El ruido se reescala para fijar:

```text
error_pct_obtenido = 100 · ||E||² / (||X_true||² + ||E||²)
```

La medida `error_pct_en_X` usa `||X||²` como denominador. Puede diferir de la anterior porque la energía de X incluye el término cruzado entre señal y ruido.

## Ajuste y selección

Cada candidato se ajusta mediante actualizaciones alternadas ALS/HOOI. El primer inicio usa HOSVD y los restantes son aleatorios; se conserva el mayor FIT. Los rangos candidatos llegan hasta 4 por modo, sujetos a compatibilidad multilineal.

- **Scree multiway:** obtiene la menor SCE por suma de componentes `S=P+Q+R` y selecciona el punto de máxima curvatura absoluta mediante diferencias divididas, considerando los saltos entre valores de S.
- **DIFFIT:** utiliza incrementos de FIT, máximos secuenciales y cocientes de prominencia (`salience`). Aplica el umbral `100/(Smax-3)`, con Smax calculado a partir de I, J y K. Si ningún candidato supera el umbral, detiene la ejecución.

El ajuste es `FIT = 100*(1-SCE/sum(X^2))`. Expresa energía reconstruida respecto al origen. La suma S cuenta componentes, no parámetros libres.

## Archivos generados

Los resultados se escriben en `MC_OUTPUT_DIR` o en una carpeta `resultados_corregidos_<modo>_<fecha_hora>` del directorio de trabajo.

| Archivo | Contenido |
| --- | --- |
| `modelos_por_replica/SPM_00001.rds`, etc. | Tabla de candidatos por tensor, con ajuste y convergencia |
| `resultados_numericos.rds` | Metadatos, resultados individuales, generación y escenarios |
| `Resultados_Tucker3.xlsx` | Libro de la ejecución completa |
| `Resultados_Tucker3_PILOTO.xlsx` | Libro de la ejecución piloto |
| `datos_simulados.rds` | Opcional: factores generadores, señal, ruido y tensor observado |

El Excel contiene **20 hojas**, entre ellas `Global`, `Replicas`, `Celdas_completas`, `McNemar_global`, `Logistica`, `Parametros` y `Sesion_R`. Se exportan valores calculados en R, no fórmulas. Los ausentes quedan vacíos y los infinitos se conservan como texto. El programa no genera gráficos.

Después de una ejecución terminada, puedes consultar:

```r
tabla_global
tabla_celda_completa
mcnemar_global
head(resultados)

# Recuperar los resultados numéricos desde disco
guardado <- readRDS(file.path(output_dir, "resultados_numericos.rds"))
names(guardado)
```

## Interpretación y limitaciones

- `acierto` exige recuperar los tres rangos exactos; acertar únicamente S no basta. Sobreselección y subselección se definen por la suma S.
- Ambos métodos utilizan el mismo tensor. McNemar compara pares discordantes; el intervalo de la diferencia de tasas es Wald pareado aproximado. No se corrigen comparaciones múltiples.
- La regresión logística es exploratoria y agrupa la covarianza por `id_sim`. En el piloto no se estima por falta de variación de algunos factores.
- Un inicio ganador puede no haber convergido. Conviene revisar `convergio_scree`, `convergio_diffit` y las tablas SPM.
- El FIT de referencia se estima sobre datos ruidosos con los rangos verdaderos. No equivale a comparar directamente con la señal sin ruido.
- Los resúmenes globales ponderan por réplicas: con el diseño completo, el patrón agrupado aporta tres cuartas partes de los tensores.
- No hay reanudación automática. Si la ejecución se interrumpe, pueden quedar archivos SPM parciales sin el Excel final.
- Este repositorio documenta la implementación. La recuperación de rangos, la estabilidad de los resultados y las atribuciones bibliográficas del código requieren evaluación específica antes de usar las salidas como evidencia de un artículo.
