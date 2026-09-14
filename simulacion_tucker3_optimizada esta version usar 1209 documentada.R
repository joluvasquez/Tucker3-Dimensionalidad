# GUÍA DE LA VERSIÓN DOCUMENTADA
# =============================
# Propósito: comparar la recuperación de rangos Tucker3 mediante DIFFIT y
# scree multiway en un experimento Monte Carlo con réplicas emparejadas.
# Cada tensor se evalúa con ambos criterios sobre la MISMA tabla de ajustes.
# Esta copia añade documentación; no modifica expresiones ni parámetros.
# Los comentarios describen la implementación, sin validar por sí mismos
# las atribuciones bibliográficas incluidas en el archivo original.
#
# REQUISITOS Y EJECUCIÓN
# Requiere R y openxlsx. Instalar el paquete una sola vez si falta:
#   install.packages("openxlsx")
# Abrir el archivo y ejecutar Source en RStudio, o usar source() con
# encoding = "UTF-8" y la ruta de esta versión documentada.
# El script ejecuta la simulación al cargarlo, crea carpetas y exporta archivos.
# No es una biblioteca que solo defina funciones. No borra todo el entorno,
# pero crea y puede sobrescribir objetos con los mismos nombres en la sesión.
#
# CONFIGURACIÓN EXTERNA (antes de source; valores de ejemplo para prueba)
#   Sys.setenv(RUN_MODE = "pilot", N_REP = "2", N_STARTS_ALS = "1",
#              MAX_ITER_ALS = "50", TOL_ALS = "1e-7")
# Esos valores reducen el trabajo y no equivalen al diseño completo.
# Sys.setenv persiste en la sesión. Para volver a los valores del archivo:
#   Sys.unsetenv(c("RUN_MODE", "N_REP", "N_STARTS_ALS", "MAX_ITER_ALS",
#                 "TOL_ALS", "MC_SEED", "MC_OUTPUT_DIR",
#                 "SAVE_SIMULATED_ARRAYS"))
#
# VALORES POR DEFECTO
# MC_SEED=2026; N_REP=50; N_STARTS_ALS=20; MAX_ITER_ALS=500;
# TOL_ALS=1e-7; RUN_MODE="full"; SAVE_SIMULATED_ARRAYS="false".
# Diseño completo: 108 escenarios x 50 réplicas = 5400 tensores.
# Piloto: 6 escenarios (primer tamaño, primer ruido, tres núcleos,
# patrón aleatorio y agrupado con 2 grupos); conserva N_REP e inicios.
# El piloto NO recorre los tres tamaños de tensor.
#
# MAPA DEL PROGRAMA
# 1-3: configuración y diseño; 4-6: álgebra y simulación;
# 7-9: ajustes y selección; 10: métricas y resúmenes;
# 11-12: bucle Monte Carlo; 13-14: análisis y exportación.
# I,J,K: tamaños de datos; P_real,Q_real,R_real: rangos generadores;
# p,q,r: rangos candidatos; S=p+q+r: suma de componentes, no parámetros libres.
# FIT está en porcentaje; SCE es suma de cuadrados residual sin normalizar.
#
# SALIDAS
# MC_OUTPUT_DIR permite elegir una carpeta NUEVA. Si no se define, se usa
# getwd()/resultados_corregidos_<modo>_<fecha_hora>.
# - modelos_por_replica/SPM_00001.rds, ...: tabla de candidatos por tensor.
# - resultados_numericos.rds: metadatos, resultados, generación y escenarios.
# - Resultados_Tucker3.xlsx o Resultados_Tucker3_PILOTO.xlsx: 20 hojas.
# - datos_simulados.rds: opcional, con factores, señal, ruido y observaciones.
# No se generan gráficos. Excel recibe valores, no fórmulas recalculables.
#
# ALCANCE DE LA COMPROBACIÓN DE ESTA COPIA
# La documentación puede verificarse comparando parse(..., keep.source=FALSE)
# del original y de esta copia. Eso comprueba identidad del código, no
# ejecución completa, convergencia, validez inferencial o tiempos de cálculo.

############################################################
# MONTE CARLO TUCKER3 PARA ARTICULO
# COMPARACION ENTRE DIFFIT Y MULTIWAY SCREE PLOT
# PROGRAMA AUTONOMO: SIMULACION, ANALISIS Y EXPORTACION
############################################################

# Instalar una sola vez: install.packages("openxlsx")
# Abra este archivo en RStudio y ejecute su contenido completo.
# Todas las funciones estan definidas aqui; no se leen otros scripts.
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop('Falta openxlsx. Ejecute install.packages("openxlsx") y vuelva a intentar.')
}

# Exporta objetos en memoria, sin CSV intermedios.
# Exportación de las tablas finales a un libro .xlsx.
# Entradas: tablas, lista nombrada de data.frames; destino, ruta del archivo.
# Salida: devuelve destino de forma invisible; guarda el libro en disco.
# Expande columnas matriciales de aggregate(), reserva una fila para encabezados
# y comprueba límites de Excel. NA/NaN quedan vacíos; Inf/-Inf se escriben
# como texto. Aplica filtros, encabezados y congelación de la primera fila.
# Espera una hoja llamada Resumen al ajustar los anchos al final.
# overwrite=TRUE sustituye un libro existente en destino; el flujo principal
# evita reutilizar la carpeta de una ejecución anterior.
exportar_excel <- function(tablas, destino) {
  wb <- openxlsx::createWorkbook()
  encabezado <- openxlsx::createStyle(
    fgFill = "#243D59", fontColour = "#FFFFFF",
    textDecoration = "bold", wrapText = TRUE
  )

  for (hoja in names(tablas)) {
    # aggregate() puede devolver columnas matriciales: expandirlas para Excel.
    datos <- do.call(data.frame, c(unclass(tablas[[hoja]]), check.names = FALSE))
    if (nrow(datos) > 1048575L || ncol(datos) > 16384L) {
      stop("La hoja excede los limites de Excel: ", hoja)
    }
    infinitos <- lapply(datos, function(x) {
      if (is.numeric(x)) which(is.infinite(x)) else integer(0)
    })
    valores_inf <- Map(function(x, filas) as.character(x[filas]), datos, infinitos)
    for (j in seq_along(datos)) datos[[j]][infinitos[[j]]] <- NA

    openxlsx::addWorksheet(wb, hoja, gridLines = FALSE)
    openxlsx::writeData(
      wb, hoja, datos, headerStyle = encabezado,
      withFilter = nrow(datos) > 0, keepNA = FALSE
    )
    # Inf/-Inf como texto; los restantes valores conservan su tipo numerico.
    for (j in which(lengths(infinitos) > 0L)) {
      for (k in seq_along(infinitos[[j]])) {
        openxlsx::writeData(
          wb, hoja, valores_inf[[j]][k],
          startRow = infinitos[[j]][k] + 1L, startCol = j, colNames = FALSE
        )
      }
    }
    openxlsx::freezePane(wb, hoja, firstRow = TRUE)
    openxlsx::setRowHeights(wb, hoja, rows = 1, heights = 60)
    openxlsx::setColWidths(
      wb, hoja, cols = seq_along(datos),
      widths = pmin(38, pmax(18, nchar(names(datos)) * 0.8))
    )
  }
  openxlsx::setColWidths(wb, "Resumen", cols = 1:2, widths = c(28, 80))
  openxlsx::saveWorkbook(wb, destino, overwrite = TRUE)
  invisible(destino)
}

############################################################
# 1. CONFIGURACION REPRODUCIBLE
############################################################

# Sys.getenv lee texto del entorno y las conversiones lo vuelven numérico.
# N_REP es el número de réplicas POR escenario, no el total de simulaciones.
# TOL_ALS se mide en puntos porcentuales de FIT, no en unidades de SCE.
# SAVE_SIMULATED_ARRAYS solo se activa con "true" (sin distinguir mayúsculas).
# No se establece una semilla independiente por réplica: cambiar diseño,
# orden o número de inicios cambia el consumo posterior de números aleatorios.
MC_SEED <- as.integer(Sys.getenv("MC_SEED", "2026"))
N_REP <- as.integer(Sys.getenv("N_REP", "50")) ####Aquí es el número de repeticiones############################################################
N_STARTS_ALS <- as.integer(Sys.getenv("N_STARTS_ALS", "20"))
MAX_ITER_ALS <- as.integer(Sys.getenv("MAX_ITER_ALS", "500"))
TOL_ALS <- as.numeric(Sys.getenv("TOL_ALS", "1e-7"))
SAVE_SIMULATED_ARRAYS <- identical(tolower(Sys.getenv("SAVE_SIMULATED_ARRAYS", "false")), "true")

set.seed(MC_SEED)

# Una carpeta nueva por ejecucion: nunca mezclar resultados antiguos y corregidos.
# El modo debe ser exactamente full o pilot. Las validaciones detienen
# la ejecución si repeticiones, inicios, iteraciones o tolerancia no son válidos.
# La ruta relativa MC_OUTPUT_DIR, si se utiliza, se resuelve desde getwd().
# Una interrupción puede dejar la carpeta parcialmente escrita; no hay
# reanudación automática ni recuperación mediante tryCatch en el bucle.
RUN_MODE <- Sys.getenv("RUN_MODE", "full")
stopifnot(RUN_MODE %in% c("full", "pilot"), N_REP >= 2,
          N_STARTS_ALS >= 1, MAX_ITER_ALS >= 1, TOL_ALS > 0)
output_dir <- Sys.getenv("MC_OUTPUT_DIR", file.path(getwd(),
  paste0("resultados_corregidos_", RUN_MODE, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))))
if (dir.exists(output_dir)) stop("La carpeta de salida ya existe; elija una nueva.")
if (!dir.create(output_dir, recursive = TRUE)) {
  stop("No se pudo crear la carpeta de salida: ", output_dir)
}
if (!dir.create(file.path(output_dir, "modelos_por_replica"))) {
  stop("No se pudo crear la carpeta de modelos por replica.")
}

cat("Semilla Monte Carlo:", MC_SEED, "\n")
cat("Replicas por escenario:", N_REP, "\n")
cat("Inicios ALS por modelo:", N_STARTS_ALS, "\n")
cat("Tolerancia ALS:", TOL_ALS, "\n")
cat("Iteraciones maximas ALS:", MAX_ITER_ALS, "\n")

############################################################
# 2. PARAMETROS DEL DISENO
############################################################

# Factores del diseño: tres tamaños, tres rangos del núcleo y tres niveles
# de error. Los grupos solo existen dentro del patrón agrupado (diseño anidado).
# separacion_grupos es la desviación de centroides aleatorios, no una distancia
# fija entre grupos; sd_intragrupo controla la dispersión alrededor de ellos.
tam_list <- list(
  c(50, 10, 5),
  c(100, 10, 5),
  c(25, 25, 25)
)

core_list <- list(
  c(2, 2, 2),
  c(3, 2, 2),
  c(3, 3, 3)
)

error_list <- c(14, 39, 59)
n_grupos_list <- c(2, 3, 4)

separacion_grupos <- 3
sd_intragrupo <- 0.40

############################################################
# 3. DISENO EXPERIMENTAL ANIDADO
############################################################

# Producto de tamaños x núcleos x errores: 27 combinaciones.
# Se añaden 27 escenarios aleatorios y 81 agrupados (2,3,4 grupos): 108.
# En resúmenes globales con igual N_REP, el patrón agrupado aporta 3/4 de
# las réplicas; no se equilibran a posteriori los dos patrones.
base_diseno <- expand.grid(
  id_tam = seq_along(tam_list),
  id_core = seq_along(core_list),
  id_error = seq_along(error_list),
  stringsAsFactors = FALSE
)

diseno_aleatorio <- cbind(
  base_diseno,
  patron = "aleatorio",
  n_grupos = NA_integer_
)

diseno_agrupado <- do.call(
  rbind,
  lapply(n_grupos_list, function(g) {
    cbind(base_diseno, patron = "agrupado", n_grupos = g)
  })
)

escenarios <- rbind(diseno_aleatorio, diseno_agrupado)
escenarios$id_escenario <- seq_len(nrow(escenarios))
# Piloto: las tres dimensiones y ambos patrones; no es evidencia publicable.
# El filtro real usa id_tam==1 e id_error==1: tamaño 50x10x5 y error 14%.
# Mantiene los tres núcleos y dos variantes de patrón: 6 escenarios.
# Los identificadores de escenario originales se conservan, con posibles saltos.
if (RUN_MODE == "pilot") escenarios <- escenarios[
  escenarios$id_tam == 1 & escenarios$id_error == 1 &
  (escenarios$patron == "aleatorio" | escenarios$n_grupos == 2), ]
escenarios <- escenarios[!is.na(escenarios$id_escenario), ]

# Cruza cada escenario con rep=1,...,N_REP, ordena y asigna id_sim.
# id_sim identifica un tensor y enlaza registros, archivos RDS y pares de métodos.
diseno <- merge(
  escenarios,
  data.frame(rep = seq_len(N_REP)),
  by = NULL
)
diseno <- diseno[order(diseno$id_escenario, diseno$rep), ]
rownames(diseno) <- NULL
diseno$id_sim <- seq_len(nrow(diseno))

cat("Escenarios factoriales:", nrow(escenarios), "\n")
cat("Simulaciones totales:", nrow(diseno), "\n")

############################################################
# 4. FUNCIONES AUXILIARES
############################################################

# Entrada: n filas y r columnas. Salida: matriz n x r con entradas N(0,1).
# No impone ortogonalidad ni estructura de grupos.
generar_matriz_aleatoria <- function(n, r) {
  matrix(rnorm(n * r), nrow = n, ncol = r)
}

# Entradas: n individuos, r componentes, número de grupos y sus escalas.
# Asigna tamaños casi equilibrados (difieren a lo sumo en un individuo),
# permutando las etiquetas; sortea centroides y perturbaciones normales.
# Salida: lista con M (n x r), grupos (longitud n), centroides (n_grupos x r).
# Los grupos estructuran las filas del modo A; no son una clasificación estimada.
generar_matriz_agrupada <- function(n, r, n_grupos,
                                    separacion = 3,
                                    sd_intra = 0.4) {
  grupos <- rep(seq_len(n_grupos), length.out = n)
  grupos <- sample(grupos, size = n, replace = FALSE)

  centroides <- matrix(
    rnorm(n_grupos * r, mean = 0, sd = separacion),
    nrow = n_grupos,
    ncol = r
  )

  M <- matrix(NA_real_, nrow = n, ncol = r)
  for (i in seq_len(n)) {
    M[i, ] <- centroides[grupos[i], ] + rnorm(r, mean = 0, sd = sd_intra)
  }

  list(M = M, grupos = grupos, centroides = centroides)
}

# Devuelve las primeras r columnas de Q en una descomposición QR.
# Requiere r compatible con las dimensiones; drop=FALSE evita perder el formato
# matricial cuando r=1. Esta función se aplica a B,C y a inicios aleatorios.
ortogonalizar_columnas <- function(M, r = ncol(M)) {
  qr.Q(qr(M))[, seq_len(r), drop = FALSE]
}

# Crea una lista de bases ortonormales A (Ixp), B (Jxq), C (Kxr).
# Cada llamada consume números aleatorios; los rangos deben caber en cada modo.
crear_inicio_aleatorio <- function(I, J, K, p, q, r) {
  list(
    A = ortogonalizar_columnas(matrix(rnorm(I * p), I, p), p),
    B = ortogonalizar_columnas(matrix(rnorm(J * q), J, q), q),
    C = ortogonalizar_columnas(matrix(rnorm(K * r), K, r), r)
  )
}

############################################################
# 5. MATRICIZACION Y PRODUCTOS MULTILINEALES
############################################################

# Despliegue del tensor X de tamaño I x J x K en una matriz I x (J*K).
# La vectorización de R recorre primero i, luego j y finalmente k;
# el orden de columnas de este despliegue corresponde a kronecker(C,B).
matricizar_modo1 <- function(X) matrix(X, nrow = dim(X)[1])

# Devuelve J x (I*K), permutando antes los ejes a (j,i,k).
# Las columnas corresponden al orden de kronecker(C,A).
matricizar_modo2 <- function(X) {
  matrix(aperm(X, c(2, 1, 3)), nrow = dim(X)[2])
}

# Devuelve K x (I*J), permutando los ejes a (k,i,j).
# Las columnas corresponden al orden de kronecker(B,A).
matricizar_modo3 <- function(X) {
  matrix(aperm(X, c(3, 1, 2)), nrow = dim(X)[3])
}

# Producto tensor-matriz por un eje: X tiene tres dimensiones y M tiene
# tantas columnas como el tamaño del eje mode. Sustituye ese tamaño por
# nrow(M), preservando el orden original de los otros ejes.
# Permutar, matricizar, multiplicar y deshacer la permutación evita escribir
# sumas explícitas. Las comprobaciones verifican modo y compatibilidad.
producto_modo <- function(X, M, mode) {
  stopifnot(length(mode) == 1L, mode %in% 1:3, length(dim(X)) == 3L)
  orden <- c(mode, setdiff(1:3, mode))
  dimensiones <- dim(X)[orden]
  stopifnot(ncol(M) == dimensiones[1])
  matricizado <- matrix(aperm(X, orden), nrow = dimensiones[1])
  producto <- array(M %*% matricizado, dim = c(nrow(M), dimensiones[-1]))
  aperm(producto, order(orden))
}

############################################################
# 6. GENERACION DEL TENSOR Y DEL ERROR
############################################################

# Construye un núcleo disperso de tamaño P x Q x R con entradas activas 1.
# No genera un núcleo denso aleatorio. La celda especial (3,1,2) del rango
# (3,2,2) mantiene diferenciados los componentes; al final verifica sus rangos.
# La construcción está diseñada para los rangos de core_list: no todo triple
# arbitrario satisface la comprobación final.
generar_core_simple <- function(P, Q, R) {
  G <- array(0, dim = c(P, Q, R))
  for (p in seq_len(P)) {
    q <- min(p, Q)
    r <- min(p, R)
    # Para (3,2,2), una celda independiente evita fusionar componentes 2 y 3.
    if (P == 3 && Q == 2 && R == 2 && p == 3) { q <- 1; r <- 2 }
    G[p, q, r] <- 1
  }
  stopifnot(all(rangos_multilineales(G) == c(P, Q, R)))
  G
}

# Devuelve tres rangos numéricos, uno por matricización de X.
# Cuenta valores singulares > 1e-10 veces el máximo de su propio modo.
# Es un criterio de rango numérico relativo, no una selección estadística.
rangos_multilineales <- function(X) {
  vapply(list(matricizar_modo1(X), matricizar_modo2(X), matricizar_modo3(X)),
    function(M) { d <- svd(M, nu=0, nv=0)$d; sum(d > max(d) * 1e-10) }, integer(1))
}

# Reconstruye X = G x1 A x2 B x3 C mediante producto_modo.
# Entradas: A Ixp, B Jxq, C Kxr y G pxqxr. Salida: tensor I x J x K.
# No exige que A sea ortonormal para generar la señal.
construir_tensor_tucker <- function(A, B, C, G) {
  X <- producto_modo(G, A, 1)
  X <- producto_modo(X, B, 2)
  X <- producto_modo(X, C, 3)
  X
}

# Entrada: señal X_true y porcentaje error_pct (se espera 0 <= valor < 100).
# Normaliza la energía de una realización normal a la energía de la señal
# y la multiplica por sqrt(pi_error/(1-pi_error)). Así fija exactamente,
# salvo precisión numérica, ||E||^2/(||X_true||^2+||E||^2)=pi_error.
# El reescalado global acopla las entradas: E no es simplemente ruido iid
# con desviación fija. No se elimina el producto cruzado <X_true,E>.
# Por ello error_pct_en_X usa otro denominador, ||X_true+E||^2, y puede
# diferir de error_pct_obtenido. Devuelve X,E y diagnósticos de energía.
agregar_error_controlado <- function(X_true, error_pct) {
  pi_error <- error_pct / 100
  N <- array(rnorm(length(X_true)), dim = dim(X_true))

  SS_estruct <- sum(X_true^2)
  SS_N <- sum(N^2)
  N <- N * sqrt(SS_estruct / SS_N)

  e <- sqrt(pi_error / (1 - pi_error))
  E <- e * N
  X <- X_true + E

  SS_error <- sum(E^2)
  SS_total_sin_cruce <- SS_estruct + SS_error

  list(
    X = X,
    E = E,
    e = e,
    SS_estruct = SS_estruct,
    SS_error = SS_error,
    SS_total_sin_cruce = SS_total_sin_cruce,
    error_pct_obtenido = 100 * SS_error / SS_total_sin_cruce,
    error_pct_en_X = 100 * SS_error / sum(X^2)
  )
}

############################################################
# 7. AJUSTE TUCKER3 POR ALS/HOOI CON INICIOS MULTIPLES
############################################################

# Proyecta X en las bases A,B,C y reconstruye Xhat a partir del núcleo G.
# Las bases ortonormales del ajuste permiten usar sus transpuestas al proyectar.
# Devuelve G, Xhat, SCE=||X-Xhat||^2, FIT=100*(1-SCE/||X||^2)
# y prop_residual=SCE/||X||^2. X debe tener energía no nula.
# Como no hay centrado, FIT es energía explicada respecto al origen.
calcular_ajuste_tucker <- function(X, A, B, C) {
  G <- producto_modo(X, t(A), 1)
  G <- producto_modo(G, t(B), 2)
  G <- producto_modo(G, t(C), 3)

  Xhat <- producto_modo(G, A, 1)
  Xhat <- producto_modo(Xhat, B, 2)
  Xhat <- producto_modo(Xhat, C, 3)

  SSX <- sum(X^2)
  SCE <- sum((X - Xhat)^2)
  FIT <- 100 * (1 - SCE / SSX)

  list(G = G, Xhat = Xhat, SCE = SCE, FIT = FIT, prop_residual = SCE / SSX)
}

# Inicialización determinista para X y rangos fijos: toma los primeros
# vectores singulares izquierdos de cada matricización (HOSVD truncada).
# Devuelve lista A,B,C; no realiza todavía las actualizaciones alternadas.
inicio_hosvd <- function(X, p, q, r) {
  list(
    A = svd(matricizar_modo1(X), nu = p, nv = 0)$u[, seq_len(p), drop = FALSE],
    B = svd(matricizar_modo2(X), nu = q, nv = 0)$u[, seq_len(q), drop = FALSE],
    C = svd(matricizar_modo3(X), nu = r, nv = 0)$u[, seq_len(r), drop = FALSE]
  )
}

# Ajusta un candidato desde las bases de inicio, por actualizaciones HOOI.
# Cada iteración actualiza A, luego B con A nuevo, y C con A,B nuevos.
# La condición abs(FIT_actual-FIT_anterior)<tol se expresa en puntos
# porcentuales; la primera iteración no puede converger por este criterio.
# Devuelve el ajuste, factores, iteraciones usadas y bandera convergio.
# Al agotar max_iter devuelve el último ajuste con convergio=FALSE.
# El criterio comprueba estabilidad, no certifica el óptimo global.
ajustar_tucker_als_un_inicio <- function(X, p, q, r, inicio,
                                         max_iter = 500,
                                         tol = 1e-7) {
  A <- inicio$A
  B <- inicio$B
  C <- inicio$C
  X1 <- matricizar_modo1(X)
  X2 <- matricizar_modo2(X)
  X3 <- matricizar_modo3(X)
  fit_anterior <- -Inf
  convergio <- FALSE

  for (iter in seq_len(max_iter)) {
    Z1 <- kronecker(C, B)
    A <- svd(X1 %*% Z1, nu = p, nv = 0)$u[, seq_len(p), drop = FALSE]

    Z2 <- kronecker(C, A)
    B <- svd(X2 %*% Z2, nu = q, nv = 0)$u[, seq_len(q), drop = FALSE]

    Z3 <- kronecker(B, A)
    C <- svd(X3 %*% Z3, nu = r, nv = 0)$u[, seq_len(r), drop = FALSE]

    ajuste <- calcular_ajuste_tucker(X, A, B, C)
    mejora <- ajuste$FIT - fit_anterior

    if (is.finite(fit_anterior) && abs(mejora) < tol) {
      convergio <- TRUE
      break
    }

    fit_anterior <- ajuste$FIT
  }

  ajuste$A <- A
  ajuste$B <- B
  ajuste$C <- C
  ajuste$iter <- iter
  ajuste$convergio <- convergio
  ajuste
}

# Ajusta n_starts inicios: el primero HOSVD y los restantes aleatorios.
# Devuelve el de mayor FIT (primero en caso de empate), añadiendo start,
# fit_por_inicio y n_starts. No descarta inicios que no convergieron:
# el mejor ajuste puede conservar convergio=FALSE.
# Durante esta función se guardan todos los ajustes, incluidas reconstrucciones.
ajustar_tucker_als <- function(X, p, q, r,
                               n_starts = 20,
                               max_iter = 500,
                               tol = 1e-7) {
  d <- dim(X)
  inicios <- vector("list", n_starts)
  inicios[[1]] <- inicio_hosvd(X, p, q, r)

  if (n_starts > 1) {
    for (s in 2:n_starts) {
      inicios[[s]] <- crear_inicio_aleatorio(d[1], d[2], d[3], p, q, r)
    }
  }

  ajustes <- vector("list", n_starts)
  for (s in seq_len(n_starts)) {
    ajustes[[s]] <- ajustar_tucker_als_un_inicio(
      X, p, q, r, inicios[[s]],
      max_iter = max_iter,
      tol = tol
    )
    ajustes[[s]]$start <- s
  }

  fits <- vapply(ajustes, function(z) z$FIT, numeric(1))
  mejor <- ajustes[[which.max(fits)]]
  mejor$fit_por_inicio <- fits
  mejor$n_starts <- n_starts
  mejor
}

############################################################
# 8. CONSTRUCCION DE LA TABLA DE MODELOS CANDIDATOS
############################################################

# Filtra triples con r<=p*q, q<=p*r y p<=q*r, condiciones de compatibilidad
# del rango multilineal. Recibe y devuelve una tabla con columnas P,Q,R.
filtrar_modelos_utiles <- function(SPM) {
  SPM[SPM$P * SPM$Q >= SPM$R &
        SPM$P * SPM$R >= SPM$Q &
        SPM$Q * SPM$R >= SPM$P, , drop = FALSE]
}

# Enumera todos los triples hasta p_max,q_max,r_max y aplica el filtro.
# Ajusta cada candidato con múltiples inicios y devuelve una tabla ordenada
# por S, SCE y rangos. S=P+Q+R; convergio, iter y mejor_inicio corresponden
# solo al inicio ganador, no al conjunto de inicios de ese candidato.
# No devuelve ni almacena factores de todos los modelos en SPM.
construir_SPM <- function(X, p_max, q_max, r_max,
                          n_starts = 20,
                          max_iter = 500,
                          tol = 1e-7) {
  combinaciones <- expand.grid(P = 1:p_max, Q = 1:q_max, R = 1:r_max)
  combinaciones <- filtrar_modelos_utiles(combinaciones)

  SPM <- data.frame(
    P = integer(nrow(combinaciones)),
    Q = integer(nrow(combinaciones)),
    R = integer(nrow(combinaciones)),
    S = integer(nrow(combinaciones)),
    SCE = numeric(nrow(combinaciones)),
    FIT = numeric(nrow(combinaciones)),
    prop_residual = numeric(nrow(combinaciones)),
    convergio = logical(nrow(combinaciones)),
    iter = integer(nrow(combinaciones)),
    mejor_inicio = integer(nrow(combinaciones))
  )

  for (i in seq_len(nrow(combinaciones))) {
    p <- combinaciones$P[i]
    q <- combinaciones$Q[i]
    r <- combinaciones$R[i]

    ajuste <- ajustar_tucker_als(
      X, p, q, r,
      n_starts = n_starts,
      max_iter = max_iter,
      tol = tol
    )

    SPM$P[i] <- p
    SPM$Q[i] <- q
    SPM$R[i] <- r
    SPM$S[i] <- p + q + r
    SPM$SCE[i] <- ajuste$SCE
    SPM$FIT[i] <- ajuste$FIT
    SPM$prop_residual[i] <- ajuste$prop_residual
    SPM$convergio[i] <- ajuste$convergio
    SPM$iter[i] <- ajuste$iter
    SPM$mejor_inicio[i] <- ajuste$start
  }

  SPM <- SPM[order(SPM$S, SPM$SCE, SPM$P, SPM$Q, SPM$R), ]
  rownames(SPM) <- NULL
  SPM
}

############################################################
# 9. CRITERIOS DE SELECCION
############################################################

# Resume SPM por S: conserva SCE mínimo y FIT máximo, y ordena por S.
# Ambas medidas son equivalentes para un mismo tensor y denominador SSX.
# Devuelve la curva tabulada; esta función no dibuja un gráfico.
obtener_scree_multiway <- function(SPM) {
  SCEmin <- aggregate(SCE ~ S, data = SPM, FUN = min)
  FITmax <- aggregate(FIT ~ S, data = SPM, FUN = max)
  tabla <- merge(SCEmin, FITmax, by = "S")
  tabla <- tabla[order(tabla$S), ]
  rownames(tabla) <- NULL
  tabla
}

# Devuelve una fila completa por S, priorizando menor SCE y luego mayor FIT.
# Los empates restantes se resuelven por P,Q,R en orden ascendente.
obtener_mejor_modelo_por_S <- function(SPM) {
  S_vals <- sort(unique(SPM$S))
  salida <- vector("list", length(S_vals))

  for (i in seq_along(S_vals)) {
    sub <- SPM[SPM$S == S_vals[i], , drop = FALSE]
    sub <- sub[order(sub$SCE, -sub$FIT, sub$P, sub$Q, sub$R), , drop = FALSE]
    salida[[i]] <- sub[1, , drop = FALSE]
  }

  out <- do.call(rbind, salida)
  rownames(out) <- NULL
  out
}

# Entrada: tabla ordenada por S, con valores S distintos y columna SCE.
# Usa curvatura por diferencias divididas para considerar saltos entre sumas.
# Elige el punto interior de máxima curvatura absoluta, no necesariamente
# un cambio de un signo específico. Devuelve s_sel, idx y valor_criterio.
# Con menos de tres filas elige la menor SCE; en esa rama idx se fija en 1
# aunque la fila ganadora sea otra. El bucle principal utiliza s_sel.
seleccionar_multiway_scree <- function(tabla_scree) {
  if (nrow(tabla_scree) < 3) {
    return(list(s_sel = tabla_scree$S[which.min(tabla_scree$SCE)],
                idx = 1,
                valor_criterio = NA_real_))
  }

  # Diferencias divididas: los valores admisibles de S pueden saltar de 3 a 5.
  h <- diff(tabla_scree$S)
  d1 <- -diff(tabla_scree$SCE) / h
  d2 <- 2 * diff(d1) / (head(h, -1) + tail(h, -1))
  idx <- which.max(abs(d2)) + 1

  list(
    s_sel = tabla_scree$S[idx],
    idx = idx,
    valor_criterio = max(abs(d2), na.rm = TRUE)
  )
}

# Tabla de candidatos ganadores por S para DIFFIT. Prioriza FIT descendente,
# SCE ascendente y, en empates, P,Q,R; devuelve filas ordenadas por S.
mejor_modelo_por_s_fit <- function(SPM) {
  s_vals <- sort(unique(SPM$S))
  out <- vector("list", length(s_vals))

  for (i in seq_along(s_vals)) {
    sub <- SPM[SPM$S == s_vals[i], , drop = FALSE]
    sub <- sub[order(-sub$FIT, sub$SCE, sub$P, sub$Q, sub$R), , drop = FALSE]
    out[[i]] <- sub[1, , drop = FALSE]
  }

  best <- do.call(rbind, out)
  rownames(best) <- NULL
  best
}

# Añade dif: primer FIT respecto a cero, seguido por incrementos de FIT
# entre sumas consecutivas disponibles. No divide por la distancia entre S;
# esta convención difiere de las diferencias divididas usadas por el scree.
calcular_dif <- function(best_by_s) {
  best_by_s <- best_by_s[order(best_by_s$S), , drop = FALSE]
  best_by_s$dif <- c(best_by_s$FIT[1], diff(best_by_s$FIT))
  best_by_s
}

# Retiene incrementos dif mayores o iguales a TODOS los incrementos
# posteriores; incluye siempre el último. No son solo máximos locales.
# salience es el cociente entre incrementos retenidos consecutivos; si
# el denominador tiene magnitud <1e-12 se asigna Inf. La última salience es NA.
# Devuelve las filas retenidas con esa columna adicional.
obtener_maximos_secuenciales <- function(best_by_s) {
  n <- nrow(best_by_s)
  es_max_seq <- rep(FALSE, n)

  for (i in seq_len(n)) {
    if (i == n) {
      es_max_seq[i] <- TRUE
    } else {
      es_max_seq[i] <- best_by_s$dif[i] >= max(best_by_s$dif[(i + 1):n], na.rm = TRUE)
    }
  }

  dift <- best_by_s[es_max_seq, , drop = FALSE]
  rownames(dift) <- NULL
  dift$salience <- NA_real_

  if (nrow(dift) >= 2) {
    for (i in seq_len(nrow(dift) - 1)) {
      dift$salience[i] <- ifelse(abs(dift$dif[i + 1]) < 1e-12,
                                 Inf,
                                 dift$dif[i] / dift$dif[i + 1])
    }
  }

  dift
}

# DIFFIT: Timmerman y Kiers (2000); formulacion explicita de Smax
# en Ceulemans y Kiers (2006). I,J,K son dimensiones del tensor,
# no los limites de busqueda p_max,q_max,r_max.
# Implementación del criterio DIFFIT usado aquí. Recibe la tabla SPM y
# dimensiones de DATOS I,J,K, no los rangos máximos de búsqueda.
# Calcula Smax con las dimensiones, el umbral 100/(Smax-3), y conserva
# filas con salience definida y dif estrictamente superior al umbral.
# Elige mayor salience, luego mayor dif y menor S. Devuelve modelo_final
# y los valores del criterio. Si no hay candidatos, stop() interrumpe
# toda la ejecución; no se registra simplemente una selección ausente.
# El umbral usa Smax teórico aunque la búsqueda esté truncada a rango 4.
seleccionar_diffit <- function(SPM, I, J, K) {
  stopifnot(length(I) == 1L, length(J) == 1L, length(K) == 1L,
            all(is.finite(c(I, J, K))), all(c(I, J, K) >= 1))
  best_by_s <- mejor_modelo_por_s_fit(SPM)
  best_by_s <- calcular_dif(best_by_s)
  dift <- obtener_maximos_secuenciales(best_by_s)

  smax <- min(I, J * K) + min(J, I * K) + min(K, I * J)
  if (smax <= 3) stop("DIFFIT: Smax debe ser mayor que 3 para definir el umbral.")
  umbral_diffit_pct <- 100 / (smax - 3)
  candidatos <- dift[!is.na(dift$salience) & dift$dif > umbral_diffit_pct, , drop = FALSE]

  # No relajar silenciosamente el umbral ni forzar una seleccion.
  if (nrow(candidatos) == 0) {
    stop("DIFFIT: ningun candidato cumple el umbral bibliografico.")
  }
  candidatos <- candidatos[order(-candidatos$salience, -candidatos$dif, candidatos$S), , drop = FALSE]
  elegido <- candidatos[1, , drop = FALSE]
  modelo_final <- best_by_s[best_by_s$S == elegido$S, , drop = FALSE]

  list(
    s_c = elegido$S,
    modelo_final = modelo_final,
    valor_diffit = elegido$salience,
    dif_seleccionado = elegido$dif,
    umbral_diffit_pct = umbral_diffit_pct
  )
}

############################################################
# 10. METRICAS DE DESEMPENO
############################################################

# Compara rangos seleccionados con generadores. Devuelve una fila con
# aciertos exactos y por modo, errores L1/cuadrático y sesgo de la suma S.
# Sobre/subselección dependen solo de S: igualar la suma no implica acertar
# el triple. delta_fit_vs_verdadero resta el FIT estimado con rangos reales
# sobre X ruidoso; no compara directamente la reconstrucción con X_true.
calcular_metricas <- function(P_sel, Q_sel, R_sel, fit_sel, SCE_sel,
                              P_real, Q_real, R_real, fit_true) {
  S_sel <- P_sel + Q_sel + R_sel
  S_real <- P_real + Q_real + R_real

  data.frame(
    P_sel = P_sel,
    Q_sel = Q_sel,
    R_sel = R_sel,
    S_sel = S_sel,
    fit = fit_sel,
    SCE = SCE_sel,
    prop_residual = 1 - fit_sel / 100,
    acierto = as.integer(P_sel == P_real & Q_sel == Q_real & R_sel == R_real),
    acierto_P = as.integer(P_sel == P_real),
    acierto_Q = as.integer(Q_sel == Q_real),
    acierto_R = as.integer(R_sel == R_real),
    error_abs_total = abs(P_sel - P_real) + abs(Q_sel - Q_real) + abs(R_sel - R_real),
    error_cuadratico = (P_sel - P_real)^2 + (Q_sel - Q_real)^2 + (R_sel - R_real)^2,
    sesgo_S = S_sel - S_real,
    error_abs_S = abs(S_sel - S_real),
    sobreseleccion = as.integer(S_sel > S_real),
    subseleccion = as.integer(S_sel < S_real),
    seleccion_exacta_S = as.integer(S_sel == S_real),
    delta_fit_vs_verdadero = fit_sel - fit_true
  )
}

# Entrada: vector de aciertos 0/1 sin ausentes. Salida: tamaño, tasa,
# error Monte Carlo sqrt(p*(1-p)/n) e intervalo Wilson con z=1.96.
# Tasas, error e intervalos se expresan en porcentaje. No elimina NA.
# Si p=0 o p=1 el error plug-in es cero, pero el intervalo Wilson no
# colapsa a un único punto. Se usa como resumen binomial por grupo.
mc_resumen_binario <- function(x) {
  n <- length(x)
  p <- mean(x)
  se <- sqrt(p * (1 - p) / n)
  data.frame(
    n = n,
    tasa_pct = 100 * p,
    error_mc_pct = 100 * se,
    ic95_inf_pct = 100 * ((p+1.96^2/(2*n))-1.96*sqrt(p*(1-p)/n+1.96^2/(4*n^2)))/(1+1.96^2/n),
    ic95_sup_pct = 100 * ((p+1.96^2/(2*n))+1.96*sqrt(p*(1-p)/n+1.96^2/(4*n^2)))/(1+1.96^2/n)
  )
}

# Agrupa resultados por vars_grupo; character(0) produce un resumen global.
# Sustituye NA por "no_aplica" solo al construir claves, evitando perder
# las filas del patrón aleatorio. Las columnas originales mantienen sus NA.
# Devuelve tasas, intervalos, ajustes medios, errores y coincidencia.
# Las medias se calculan sobre filas presentes, sin ponderar escenarios
# ni ajustar por heterogeneidad de probabilidades entre celdas agrupadas.
resumir_por_grupo <- function(data, vars_grupo) {
  if (length(vars_grupo) == 0) {
    data$.grupo_global <- "global"
    vars_grupo <- ".grupo_global"
  }

  split_data <- split(data, lapply(data[vars_grupo], function(x) {
    x <- as.character(x); x[is.na(x)] <- "no_aplica"; x
  }), drop = TRUE)

  out <- lapply(names(split_data), function(nm) {
    z <- split_data[[nm]]
    base <- z[1, vars_grupo, drop = FALSE]
    scree <- mc_resumen_binario(z$acierto_scree)
    diffit <- mc_resumen_binario(z$acierto_diffit)

    data.frame(
      base,
      n = nrow(z),
      acierto_scree_pct = 100 * mean(z$acierto_scree),
      error_mc_acierto_scree = 100 * sqrt(mean(z$acierto_scree) * (1 - mean(z$acierto_scree)) / nrow(z)),
      acierto_diffit_pct = 100 * mean(z$acierto_diffit),
      error_mc_acierto_diffit = 100 * sqrt(mean(z$acierto_diffit) * (1 - mean(z$acierto_diffit)) / nrow(z)),
      ic95_inf_scree_pct = scree$ic95_inf_pct,
      ic95_sup_scree_pct = scree$ic95_sup_pct,
      ic95_inf_diffit_pct = diffit$ic95_inf_pct,
      ic95_sup_diffit_pct = diffit$ic95_sup_pct,
      aciertoP_scree_pct = 100*mean(z$aciertoP_scree),
      aciertoQ_scree_pct = 100*mean(z$aciertoQ_scree),
      aciertoR_scree_pct = 100*mean(z$aciertoR_scree),
      aciertoP_diffit_pct = 100*mean(z$aciertoP_diffit),
      aciertoQ_diffit_pct = 100*mean(z$aciertoQ_diffit),
      aciertoR_diffit_pct = 100*mean(z$aciertoR_diffit),
      fit_scree_prom = mean(z$fit_scree),
      fit_diffit_prom = mean(z$fit_diffit),
      SCE_scree_prom = mean(z$SCE_scree),
      SCE_diffit_prom = mean(z$SCE_diffit),
      sobreseleccion_scree_pct = 100 * mean(z$sobreseleccion_scree),
      sobreseleccion_diffit_pct = 100 * mean(z$sobreseleccion_diffit),
      subseleccion_scree_pct = 100 * mean(z$subseleccion_scree),
      subseleccion_diffit_pct = 100 * mean(z$subseleccion_diffit),
      error_abs_scree_prom = mean(z$error_abs_scree),
      error_abs_diffit_prom = mean(z$error_abs_diffit),
      sesgo_scree_prom = mean(z$sesgo_scree),
      sesgo_diffit_prom = mean(z$sesgo_diffit),
      coincidencia_pct = 100 * mean(z$coincide_criterios)
    )
  })

  salida <- do.call(rbind, out)
  rownames(salida) <- NULL
  salida
}

# Comparación emparejada: cada id_sim aporta el acierto de ambos métodos.
# b cuenta aciertos exclusivos de scree y c exclusivos de DIFFIT.
# El p-valor bilateral se obtiene con binom.test(b,b+c,p=0.5); sin
# discordancias vale 1. La diferencia es scree menos DIFFIT en puntos
# porcentuales. Su IC es Wald pareado aproximado, recortado a [-100,100],
# distinto del cálculo exacto del p-valor. Puede degenerar con varianza cero
# y ser NA con un solo par. No aplica corrección por múltiples comparaciones.
mcnemar_por_grupo <- function(data, vars_grupo) {
  if (length(vars_grupo) == 0) {
    data$.grupo_global <- "global"
    vars_grupo <- ".grupo_global"
  }

  split_data <- split(data, lapply(data[vars_grupo], function(x) {
    x <- as.character(x); x[is.na(x)] <- "no_aplica"; x
  }), drop = TRUE)

  out <- lapply(names(split_data), function(nm) {
    z <- split_data[[nm]]
    base <- z[1, vars_grupo, drop = FALSE]

    b <- sum(z$acierto_scree == 1 & z$acierto_diffit == 0)
    c <- sum(z$acierto_scree == 0 & z$acierto_diffit == 1)
    n <- nrow(z)
    d <- z$acierto_scree - z$acierto_diffit
    dif <- mean(d)
    se <- sd(d) / sqrt(n)

    # McNemar exacto, adecuado tambien con pocos pares discordantes.
    p_val <- if ((b + c) == 0) 1 else binom.test(b, b + c, p = 0.5)$p.value

    data.frame(
      base,
      n = n,
      scree_acierta_diffit_no = b,
      diffit_acierta_scree_no = c,
      diferencia_tasas_pct = 100 * dif,
      ic95_inf_dif_pct = 100 * max(-1, dif - 1.96 * se),
      ic95_sup_dif_pct = 100 * min(1, dif + 1.96 * se),
      tipo_ic = "Wald pareado aproximado; degenerado si varianza cero",
      p_mcnemar = p_val
    )
  })

  salida <- do.call(rbind, out)
  rownames(salida) <- NULL
  salida
}

############################################################
# 11. OBJETOS DE SALIDA
############################################################

# Preasigna una posición por tensor. Las tablas se ensamblan al final.
# Guardar tensores opcionales aumenta memoria: todos se conservan en una
# lista hasta exportarlos, no se escriben individualmente en cada iteración.
filas_generacion <- vector("list", nrow(diseno))
filas_resultados <- vector("list", nrow(diseno))
datos_simulados <- if (SAVE_SIMULATED_ARRAYS) vector("list", nrow(diseno)) else NULL

############################################################
# 12. GENERACION Y EVALUACION MONTE CARLO
############################################################

# Bucle secuencial: genera UN tensor, ajusta candidatos y aplica ambos criterios.
# No hay paralelización. Con los valores por defecto, el trabajo completo
# incluye miles de tensores y múltiples inicios para cada candidato.
for (i in seq_len(nrow(diseno))) {
  cat("Simulacion", i, "de", nrow(diseno), "\n")

  dims <- tam_list[[diseno$id_tam[i]]]
  core <- core_list[[diseno$id_core[i]]]
  error_pct <- error_list[diseno$id_error[i]]
  patron <- diseno$patron[i]
  n_grupos <- diseno$n_grupos[i]

  I <- dims[1]; J <- dims[2]; K <- dims[3]
  P_real <- core[1]; Q_real <- core[2]; R_real <- core[3]
  S_real <- P_real + Q_real + R_real

  if (patron == "aleatorio") {
    A <- generar_matriz_aleatoria(I, P_real)
    grupos_A <- rep(NA_integer_, I)
  } else {
    genA <- generar_matriz_agrupada(
      n = I,
      r = P_real,
      n_grupos = n_grupos,
      separacion = separacion_grupos,
      sd_intra = sd_intragrupo
    )
    A <- genA$M
    grupos_A <- genA$grupos
  }

  # En la generación A conserva su escala/estructura y NO se ortogonaliza;
  # B y C sí. Antes de añadir ruido se verifica el rango efectivo de la señal.
  # Esto distingue los factores generadores de las bases ortonormales estimadas.
  B <- ortogonalizar_columnas(generar_matriz_aleatoria(J, Q_real), Q_real)
  C <- ortogonalizar_columnas(generar_matriz_aleatoria(K, R_real), R_real)
  G <- generar_core_simple(P_real, Q_real, R_real)
  X_true <- construir_tensor_tucker(A, B, C, G)
  rango_efectivo <- rangos_multilineales(X_true)
  if (!all(rango_efectivo == core)) stop("Rango efectivo distinto del declarado")
  ruido <- agregar_error_controlado(X_true, error_pct)
  X <- ruido$X

  p_max <- min(4, I)
  q_max <- min(4, J)
  r_max <- min(4, K)

  # Ajuste de referencia a X ruidoso usando rangos generadores conocidos.
  # Se vuelve a ajustar ese mismo triple dentro de SPM con nuevos inicios
  # aleatorios; los dos FIT pueden diferir por óptimos locales o convergencia.
  ajuste_verdadero <- ajustar_tucker_als(
    X, P_real, Q_real, R_real,
    n_starts = N_STARTS_ALS,
    max_iter = MAX_ITER_ALS,
    tol = TOL_ALS
  )
  fit_modelo_verdadero <- ajuste_verdadero$FIT

  SPM <- construir_SPM(
    X, p_max, q_max, r_max,
    n_starts = N_STARTS_ALS,
    max_iter = MAX_ITER_ALS,
    tol = TOL_ALS
  )

  # Guarda SPM antes de la selección DIFFIT. Si esta falla pueden quedar
  # tablas SPM parciales, pero todavía no el libro final ni el resumen RDS.
  # Los archivos SPM permiten revisar errores y convergencia de los candidatos.
  tabla_scree <- obtener_scree_multiway(SPM)
  saveRDS(SPM, file.path(output_dir, "modelos_por_replica",
                         sprintf("SPM_%05d.rds", diseno$id_sim[i])))
  mejor_por_S <- obtener_mejor_modelo_por_S(SPM)
  sel_scree <- seleccionar_multiway_scree(tabla_scree)
  modelo_scree <- mejor_por_S[mejor_por_S$S == sel_scree$s_sel, , drop = FALSE]

  res_diffit <- seleccionar_diffit(SPM, I, J, K)
  modelo_diffit <- res_diffit$modelo_final

  met_scree <- calcular_metricas(
    modelo_scree$P, modelo_scree$Q, modelo_scree$R,
    modelo_scree$FIT, modelo_scree$SCE,
    P_real, Q_real, R_real, fit_modelo_verdadero
  )

  met_diffit <- calcular_metricas(
    modelo_diffit$P, modelo_diffit$Q, modelo_diffit$R,
    modelo_diffit$FIT, modelo_diffit$SCE,
    P_real, Q_real, R_real, fit_modelo_verdadero
  )

  # Registro de control de la simulación: rangos efectivos, energías,
  # porcentajes de error y ajuste de referencia, identificado por id_sim.
  filas_generacion[[i]] <- data.frame(
    id_sim = diseno$id_sim[i],
    id_escenario = diseno$id_escenario[i],
    rep = diseno$rep[i],
    patron = patron,
    n_grupos = ifelse(is.na(n_grupos), NA_integer_, n_grupos),
    I = I, J = J, K = K,
    P_real = P_real, Q_real = Q_real, R_real = R_real,
    S_real = S_real,
    rango_P = rango_efectivo[1], rango_Q = rango_efectivo[2], rango_R = rango_efectivo[3],
    error_pct_deseado = error_pct,
    error_pct_obtenido = ruido$error_pct_obtenido,
    error_pct_en_X = ruido$error_pct_en_X,
    e_error = ruido$e,
    SS_estruct = ruido$SS_estruct,
    SS_error = ruido$SS_error,
    SS_total_sin_cruce = ruido$SS_total_sin_cruce,
    fit_modelo_verdadero = fit_modelo_verdadero,
    convergio_modelo_verdadero = ajuste_verdadero$convergio,
    iter_modelo_verdadero = ajuste_verdadero$iter
  )

  # Una fila emparejada por tensor: columnas *_scree y *_diffit describen
  # las dos selecciones. acierto exige el triple exacto y coincide_criterios
  # compara triples, no solo sumas. convergencia_spm_pct es el porcentaje
  # de candidatos cuyo INICIO GANADOR convergió; no mide todos los reinicios.
  filas_resultados[[i]] <- data.frame(
    id_sim = diseno$id_sim[i],
    id_escenario = diseno$id_escenario[i],
    rep = diseno$rep[i],
    patron = patron,
    n_grupos = ifelse(is.na(n_grupos), NA_integer_, n_grupos),
    I = I, J = J, K = K,
    tamano = paste(I, J, K, sep = "x"),
    P_real = P_real, Q_real = Q_real, R_real = R_real,
    core_real = paste0("P", P_real, "_Q", Q_real, "_R", R_real),
    S_real = S_real,
    error_pct = error_pct,
    P_scree = met_scree$P_sel,
    Q_scree = met_scree$Q_sel,
    R_scree = met_scree$R_sel,
    S_scree = met_scree$S_sel,
    criterio_scree = sel_scree$valor_criterio,
    fit_scree = met_scree$fit,
    SCE_scree = met_scree$SCE,
    prop_residual_scree = met_scree$prop_residual,
    acierto_scree = met_scree$acierto,
    aciertoP_scree = met_scree$acierto_P,
    aciertoQ_scree = met_scree$acierto_Q,
    aciertoR_scree = met_scree$acierto_R,
    error_abs_scree = met_scree$error_abs_total,
    error_cuad_scree = met_scree$error_cuadratico,
    sesgo_scree = met_scree$sesgo_S,
    error_abs_S_scree = met_scree$error_abs_S,
    sobreseleccion_scree = met_scree$sobreseleccion,
    subseleccion_scree = met_scree$subseleccion,
    seleccion_exacta_S_scree = met_scree$seleccion_exacta_S,
    delta_fit_verdadero_scree = met_scree$delta_fit_vs_verdadero,
    P_diffit = met_diffit$P_sel,
    Q_diffit = met_diffit$Q_sel,
    R_diffit = met_diffit$R_sel,
    S_diffit = met_diffit$S_sel,
    valor_diffit = res_diffit$valor_diffit,
    dif_seleccionado = res_diffit$dif_seleccionado,
    umbral_diffit_pct = res_diffit$umbral_diffit_pct,
    fit_diffit = met_diffit$fit,
    SCE_diffit = met_diffit$SCE,
    prop_residual_diffit = met_diffit$prop_residual,
    acierto_diffit = met_diffit$acierto,
    aciertoP_diffit = met_diffit$acierto_P,
    aciertoQ_diffit = met_diffit$acierto_Q,
    aciertoR_diffit = met_diffit$acierto_R,
    error_abs_diffit = met_diffit$error_abs_total,
    error_cuad_diffit = met_diffit$error_cuadratico,
    sesgo_diffit = met_diffit$sesgo_S,
    error_abs_S_diffit = met_diffit$error_abs_S,
    sobreseleccion_diffit = met_diffit$sobreseleccion,
    subseleccion_diffit = met_diffit$subseleccion,
    seleccion_exacta_S_diffit = met_diffit$seleccion_exacta_S,
    delta_fit_verdadero_diffit = met_diffit$delta_fit_vs_verdadero,
    fit_modelo_verdadero = fit_modelo_verdadero,
    coincide_criterios = as.integer(
      met_scree$P_sel == met_diffit$P_sel &
        met_scree$Q_sel == met_diffit$Q_sel &
        met_scree$R_sel == met_diffit$R_sel
    ),
    convergencia_spm_pct = 100 * mean(SPM$convergio),
    convergio_scree = modelo_scree$convergio,
    convergio_diffit = modelo_diffit$convergio,
    iter_spm_prom = mean(SPM$iter)
  )

  if (SAVE_SIMULATED_ARRAYS) {
    datos_simulados[[i]] <- list(
      id_sim = diseno$id_sim[i],
      grupos_A = grupos_A,
      A = A, B = B, C = C, G = G,
      X_true = X_true, E = ruido$E, X = X
    )
  }
}

############################################################
# 13. TABLAS PARA MANUSCRITO Y MATERIAL SUPLEMENTARIO
############################################################

# Consolida listas y libera sus contenedores originales. Los objetos
# locales al bucle en el entorno principal (p.ej. X,SPM) describen solo
# la última réplica; resultados y resumen_generacion contienen todas.
resumen_generacion <- do.call(rbind, filas_generacion)
resultados <- do.call(rbind, filas_resultados)
rm(filas_generacion, filas_resultados)

tabla_global <- resumir_por_grupo(resultados, character(0))
tabla_patron <- resumir_por_grupo(resultados, "patron")
tabla_error_patron <- resumir_por_grupo(resultados, c("patron", "error_pct"))
tabla_tamano <- resumir_por_grupo(resultados, "tamano")
tabla_core <- resumir_por_grupo(resultados, "core_real")
tabla_grupos <- resumir_por_grupo(resultados[resultados$patron == "agrupado", ], "n_grupos")

tabla_celda_completa <- resumir_por_grupo(
  resultados,
  c("patron", "n_grupos", "tamano", "core_real", "error_pct")
)

mcnemar_global <- mcnemar_por_grupo(resultados, character(0))
mcnemar_patron <- mcnemar_por_grupo(resultados, "patron")
mcnemar_error_patron <- mcnemar_por_grupo(resultados, c("patron", "error_pct"))
mcnemar_celda_completa <- mcnemar_por_grupo(
  resultados,
  c("patron", "n_grupos", "tamano", "core_real", "error_pct")
)

tabla_error_generado <- aggregate(
  cbind(error_pct_obtenido, error_pct_en_X) ~ patron + error_pct_deseado,
  data = resumen_generacion,
  FUN = function(x) c(media = mean(x), sd = sd(x), min = min(x), max = max(x))
)

# Transforma cada fila emparejada en dos filas, una por método.
# id_sim identifica la dependencia que luego se usa en la covarianza robusta.
# No son dos tensores independientes.
resultados_largo <- rbind(
  data.frame(
    id_sim = resultados$id_sim,
    patron = resultados$patron,
    n_grupos = resultados$n_grupos,
    tamano = resultados$tamano,
    core_real = resultados$core_real,
    error_pct = resultados$error_pct,
    metodo = "Scree",
    acierto = resultados$acierto_scree,
    sobreseleccion = resultados$sobreseleccion_scree,
    subseleccion = resultados$subseleccion_scree,
    fit = resultados$fit_scree,
    S_sel = resultados$S_scree
  ),
  data.frame(
    id_sim = resultados$id_sim,
    patron = resultados$patron,
    n_grupos = resultados$n_grupos,
    tamano = resultados$tamano,
    core_real = resultados$core_real,
    error_pct = resultados$error_pct,
    metodo = "DIFFIT",
    acierto = resultados$acierto_diffit,
    sobreseleccion = resultados$sobreseleccion_diffit,
    subseleccion = resultados$subseleccion_diffit,
    fit = resultados$fit_diffit,
    S_sel = resultados$S_diffit
  )
)

# Inferencia exploratoria: sandwich agrupado por tensor (id_sim), HC1.
# No reportar inferencia convencional si hay separacion o falta de convergencia.
# Regresión logística exploratoria del acierto: metodo*patron más ruido,
# tamaño y núcleo. n_grupos no entra como predictor. Devuelve coeficientes,
# odds ratios, IC y p-valores Wald con covarianza agrupada por id_sim.
# Las comprobaciones devuelven una tabla de estado si faltan niveles,
# convergencia, rango o información invertible; |beta|>20 es una señal
# heurística de posible separación, no una prueba completa.
# El piloto fija tamaño y ruido, por lo que esta función no estima el modelo.
# Los resúmenes agrupados anteriores y esta regresión tienen objetivos
# distintos; la covarianza agrupada reconoce el par de métodos por tensor.
logistica_pareada <- function(data) {
  if (any(vapply(data[c("metodo","patron","error_pct","tamano","core_real")],
                 function(x) length(unique(x)) < 2, logical(1))))
    return(data.frame(estado="No estimable: piloto o factores sin variacion"))
  fit <- glm(acierto ~ metodo * patron + factor(error_pct) + tamano + core_real,
             data = data, family = binomial())
  if (!fit$converged || anyNA(coef(fit)) || any(abs(coef(fit)) > 20)) {
    return(data.frame(estado="No estimable: convergencia, rango o posible separacion"))
  }
  X <- model.matrix(fit)
  bread <- tryCatch(solve(crossprod(X, X * fit$weights)), error=function(e) NULL)
  if (is.null(bread)) return(data.frame(estado="No estimable: informacion singular"))
  U <- rowsum(X * (data$acierto - fitted(fit)), data$id_sim, reorder=FALSE)
  n <- nrow(X); k <- ncol(X); m <- nrow(U)
  if (m <= 1 || n <= k) return(data.frame(estado="Muestra insuficiente"))
  V <- (m/(m-1))*((n-1)/(n-k)) * bread %*% crossprod(U) %*% bread
  se <- sqrt(pmax(diag(V), 0)); beta <- coef(fit)
  data.frame(termino=names(beta), estimacion=beta, error_estandar_cluster=se,
    odds_ratio=exp(beta), ic95_inf_or=exp(beta-1.96*se),
    ic95_sup_or=exp(beta+1.96*se), p_valor=2*pnorm(-abs(beta/se)),
    estado="Exploratorio; sandwich HC1 agrupado por id_sim", row.names=NULL)
}
tabla_logistica_acierto <- logistica_pareada(resultados_largo)

############################################################
# 14. METADATOS Y EXPORTACION DIRECTA A EXCEL
############################################################

# Metadatos de configuración y versión declarada por el propio script.
# version_codigo es una etiqueta fija, no un hash del archivo ni una
# comprobación automática de correspondencia con una publicación.
parametros <- list(
  MC_SEED = MC_SEED, N_REP = N_REP, N_STARTS_ALS = N_STARTS_ALS,
  MAX_ITER_ALS = MAX_ITER_ALS, TOL_ALS = TOL_ALS,
  escenarios_factoriales = nrow(escenarios), simulaciones_totales = nrow(diseno),
  separacion_grupos = separacion_grupos, sd_intragrupo = sd_intragrupo,
  software = "R", version_R = as.character(getRversion()),
  version_codigo = "optimizado_2026_09_14_diffit_bibliografico", modo = RUN_MODE,
  core = "sparse_full_multilinear_rank",
  scree = "curvatura_diferencias_divididas",
  preprocesamiento = "sin_centrado_ni_escalado", logistica = "cluster_id_sim_HC1"
)
metadata <- data.frame(
  parametro = names(parametros),
  valor = vapply(parametros, as.character, character(1)), row.names = NULL
)

resumen <- data.frame(
  Campo = c("Ejecucion", "Tensores", "Escenarios", "Replicas por escenario",
            "Inicios ALS", "Porcentajes", "Datos ausentes", "Infinitos", "Origen"),
  Valor = c(
    if (RUN_MODE == "pilot") "PILOTO: prueba tecnica, no resultados definitivos"
    else "Simulacion completa",
    nrow(diseno), nrow(escenarios), N_REP, N_STARTS_ALS,
    "Columnas *_pct en escala 0-100", "NA/NaN: celdas vacias",
    "Inf y -Inf se conservan como texto",
    "Resultados calculados en R; no se recalculan al editar Excel"
  )
)

# Comprueba que el resumen por celdas conserva todos los escenarios.
# No sustituye la revisión de convergencia o del número de réplicas por celda.
stopifnot(nrow(tabla_celda_completa) == nrow(escenarios))
# Nombres de esta lista = nombres de las 20 hojas Excel. Incluye tablas
# agregadas, resultados individuales, diseño, metadatos y sessionInfo().
# Por_dimension resume rangos del núcleo; Por_tamano resume tamaños del tensor.
tablas <- list(
  Resumen = resumen,
  Global = tabla_global,
  Por_patron = tabla_patron,
  Ruido_y_patron = tabla_error_patron,
  Por_tamano = tabla_tamano,
  Por_dimension = tabla_core,
  Por_grupos = tabla_grupos,
  Celdas_completas = tabla_celda_completa,
  McNemar_global = mcnemar_global,
  McNemar_patron = mcnemar_patron,
  McNemar_ruido = mcnemar_error_patron,
  McNemar_celdas = mcnemar_celda_completa,
  Logistica = tabla_logistica_acierto,
  Parametros = metadata,
  Escenarios = escenarios,
  Generacion = resumen_generacion,
  Replicas = resultados,
  Error_generado = tabla_error_generado,
  Formato_largo = resultados_largo,
  Sesion_R = data.frame(Informacion = capture.output(sessionInfo()))
)

# RDS conserva los objetos y su precision para reutilizarlos en R.
saveRDS(
  list(metadata = metadata, resultados = resultados,
       generacion = resumen_generacion, escenarios = escenarios),
  file.path(output_dir, "resultados_numericos.rds")
)
if (SAVE_SIMULATED_ARRAYS) {
  saveRDS(datos_simulados, file.path(output_dir, "datos_simulados.rds"))
}

# Exportación final después de completar simulación y análisis. El RDS
# numérico ya se ha escrito antes del Excel; una falla de exportación no
# implica necesariamente perder ese RDS. Excel no guarda los factores
# de todos los ajustes ni crea gráficos automáticamente.
nombre_excel <- if (RUN_MODE == "pilot") "Resultados_Tucker3_PILOTO.xlsx" else "Resultados_Tucker3.xlsx"
archivo_excel <- exportar_excel(tablas, file.path(output_dir, nombre_excel))
cat("\nExcel guardado:", archivo_excel, "\n")
cat("Hojas principales: Global, Replicas, Celdas_completas, McNemar_global, Logistica.\n")
