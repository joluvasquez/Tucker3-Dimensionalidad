# Comparaciones controladas de reglas de seleccion Tucker3.
# Reutiliza los ajustes SPM. NO ejecuta ALS ni modifica los programas originales.
# Exportacion nativa con openxlsx. Ejecute source() en RStudio o Rscript.
RAIZ <- normalizePath(".", mustWork=TRUE)
ENTRADA <- file.path(RAIZ, "data/simulation")
CODIGO_V2 <- file.path(RAIZ, "code/simulation_V2.R")
CODIGO_V1 <- file.path(RAIZ, "code/simulation_V1.R")
SALIDA <- file.path(RAIZ, "controlled_outputs")
if (!requireNamespace("openxlsx", quietly=TRUE)) stop('Instale openxlsx con install.packages("openxlsx").')
dir.create(SALIDA, recursive=TRUE, showWarnings=FALSE)

cargar_funciones <- function(path) {
  env <- new.env(parent=globalenv())
  for (e in parse(path, encoding="UTF-8")) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && length(e)==3L &&
        is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e, env)
  }
  env
}

# g[1]=FIT[1] es el incremento respecto de ajuste cero.
# Con inicial=FALSE solo se excluye ese candidato; los otros incrementos no cambian.
# Todas las variantes controladas comparten tolerancia, desempate y manejo de vacios.
puntajes_diffit <- function(g, tipo) {
  n <- length(g); score <- rep(NA_real_, n)
  indices <- if(tipo=="todos") seq_len(n) else which(vapply(seq_len(n), function(i)
    i==n || g[i]>=max(g[(i+1L):n]), logical(1)))
  if(length(indices)>1L) for(k in seq_len(length(indices)-1L)) {
    i <- indices[k]
    den <- if(tipo=="todos") max(g[(i+1L):n]) else g[indices[k+1L]]
    score[i] <- if(abs(den)<1e-12) Inf else g[i]/den
  }
  score
}
elegir_diffit <- function(g, s, score, inicial, umbral, tau) {
  ok <- !is.na(score)
  if(!inicial) ok[1] <- FALSE
  if(umbral) ok <- ok & g>tau
  ids <- which(ok)
  if(!length(ids)) return(NA_integer_)
  ids[order(-score[ids], -g[ids], s[ids])][1]
}
puntajes_scree <- function(fit, s, espaciado) {
  if(length(s)<3) stop("Scree requiere al menos tres puntos.")
  h <- diff(s)
  x <- if(espaciado) 2*diff(diff(fit)/h)/(head(h,-1)+tail(h,-1)) else diff(diff(fit))
  c(NA_real_, abs(x), NA_real_)
}
# Pruebas de condiciones que alteran la interpretacion del contraste.
stopifnot(is.na(elegir_diffit(c(1,1,1),1:3,c(1,1,NA),TRUE,TRUE,2)))
stopifnot(elegir_diffit(c(20,4,1),1:3,c(5,4,NA),FALSE,FALSE,0)==2L)
stopifnot(identical(puntajes_scree(c(0,10,12,13),1:4,TRUE),
                    puntajes_scree(c(0,10,12,13),1:4,FALSE)))

v1 <- cargar_funciones(CODIGO_V1); v2 <- cargar_funciones(CODIGO_V2)
obj <- readRDS(file.path(ENTRADA,"resultados_numericos.rds")); meta <- obj$resultados
stopifnot(!anyDuplicated(meta$id_sim))
archivos <- file.path(ENTRADA,"modelos_por_replica",sprintf("SPM_%05d.rds",meta$id_sim))
stopifnot(all(file.exists(archivos)))
diseno <- expand.grid(inicial=c(FALSE,TRUE), umbral=c(FALSE,TRUE),
                      cociente=c("todos","maximos"), stringsAsFactors=FALSE)
diseno$variante <- paste0("D_I",as.integer(diseno$inicial),"_U",as.integer(diseno$umbral),"_",diseno$cociente)
registros <- vector("list",nrow(meta)); curvas <- vector("list",nrow(meta))
validacion <- matrix(0L,nrow(meta),4,dimnames=list(NULL,c("D_V2_guardado","S_V2_guardado","D_V1_control","S_V1_control")))
referencia_v1 <- file.path(RAIZ,"data/V1_comparison.rds")
stopifnot(file.exists(referencia_v1))
prev <- readRDS(referencia_v1)$Por_replica

for(i in seq_len(nrow(meta))) {
  m <- meta[i,]; z <- readRDS(archivos[i])
  stopifnot(all(is.finite(z$FIT)),all(is.finite(z$SCE)),all(z$S==z$P+z$Q+z$R))
  b <- v2$mejor_modelo_por_s_fit(z); s <- b$S; g <- c(b$FIT[1],diff(b$FIT))
  tau <- 100/(min(m$I,m$J*m$K)+min(m$J,m$I*m$K)+min(m$K,m$I*m$J)-3)
  qt <- puntajes_diffit(g,"todos"); qm <- puntajes_diffit(g,"maximos")
  sc1 <- puntajes_scree(b$FIT,s,FALSE); sc2 <- puntajes_scree(b$FIT,s,TRUE)
  indices <- vapply(seq_len(nrow(diseno)),function(j) elegir_diffit(g,s,
    if(diseno$cociente[j]=="todos") qt else qm,diseno$inicial[j],diseno$umbral[j],tau),integer(1))
  indices <- c(indices,which.max(sc1),which.max(sc2))
  nombres <- c(diseno$variante,"S_sin_espaciado","S_con_espaciado")
  # Referencias exactas: se usan las funciones del archivo suministrado, sin source().
  for(version in c("V1","V2")) {
    env <- if(version=="V1") v1 else v2
    d <- env$seleccionar_diffit(z,m$I,m$J,m$K)$modelo_final
    ss <- env$seleccionar_multiway_scree(env$obtener_scree_multiway(z))$s_sel
    indices <- c(indices,match(d$S,s),match(ss,s))
    nombres <- c(nombres,paste0("Referencia_D_",version),paste0("Referencia_S_",version))
  }
  r <- b[indices,c("P","Q","R","S","FIT"),drop=FALSE];rownames(r)<-NULL
  names(r)[5] <- "Fit_pct"
  r$variante <- nombres; r$id_sim <- m$id_sim; r$id_escenario <- m$id_escenario
  for(k in c("patron","tamano","core_real","n_grupos","error_pct","P_real","Q_real","R_real","S_real")) r[[k]] <- m[[k]]
  r$definido <- !is.na(indices)
  r$acierto <- as.integer(r$definido & r$P==m$P_real & r$Q==m$Q_real & r$R==m$R_real)
  r$error_L1 <- abs(r$P-m$P_real)+abs(r$Q-m$Q_real)+abs(r$R-m$R_real)
  r$sub_S <- r$S<m$S_real; r$sobre_S <- r$S>m$S_real
  r$seleccion_111 <- r$P==1 & r$Q==1 & r$R==1
  registros[[i]] <- r
  curvas[[i]] <- data.frame(id_sim=m$id_sim,b[,c("P","Q","R","S","FIT","SCE")],
    incremento_pct=g,umbral_pct=tau,cociente_todos=qt,cociente_maximos=qm,
    scree_sin_espaciado=sc1,scree_con_espaciado=sc2)
  getr <- function(nombre) r[r$variante==nombre,c("P","Q","R"),drop=FALSE]
  eq <- function(a,b) isTRUE(all.equal(unname(as.numeric(a)),unname(as.numeric(b)),tolerance=0))
  validacion[i,1] <- !eq(getr("Referencia_D_V2"),m[,c("P_diffit","Q_diffit","R_diffit")])
  validacion[i,2] <- !eq(getr("Referencia_S_V2"),m[,c("P_scree","Q_scree","R_scree")])
  validacion[i,3] <- !eq(getr("Referencia_D_V1"),getr("D_I0_U0_todos"))
  validacion[i,4] <- !eq(getr("Referencia_S_V1"),getr("S_sin_espaciado"))
  if(i%%500L==0L) message("Replicas procesadas: ",i,"/",nrow(meta))
}
detalle <- do.call(rbind,registros); curvas <- do.call(rbind,curvas)
stopifnot(sum(validacion[,1:2])==0)
for(par in list(c("Referencia_D_V2","D_I1_U1_maximos"),c("Referencia_S_V2","S_con_espaciado"))) {
  a <- detalle[detalle$variante==par[1],]; b <- detalle[detalle$variante==par[2],]
  stopifnot(identical(a$id_sim,b$id_sim),identical(a$P,b$P),identical(a$Q,b$Q),identical(a$R,b$R))
}
if(!is.null(prev)) {
  for(crit in c("diffit","scree")) {
    a <- prev[prev$Version=="Original_aplicacion" & prev$Criterio==crit,]
    if(nrow(a)==0) stop("No se encontro el criterio en referencia V1: ",crit)
    b <- detalle[detalle$variante==paste0("Referencia_",if(crit=="diffit") "D" else "S","_V1"),]
    a <- a[match(b$id_sim,a$id_sim),]
    stopifnot(nrow(a)==nrow(b),all(a$P==b$P),all(a$Q==b$Q),all(a$R==b$R))
  }
}
resumir <- function(x) data.frame(variante=x$variante[1],N=nrow(x),
  sin_seleccion=sum(!x$definido),acierto_pct=100*mean(x$acierto),
  media_S=mean(x$S,na.rm=TRUE),Fit_pct=mean(x$Fit_pct,na.rm=TRUE),
  error_L1=mean(x$error_L1,na.rm=TRUE),sub_S_pct=100*mean(x$sub_S,na.rm=TRUE),
  sobre_S_pct=100*mean(x$sobre_S,na.rm=TRUE),seleccion_111=sum(x$seleccion_111,na.rm=TRUE))
global <- do.call(rbind,lapply(split(detalle,detalle$variante),resumir));rownames(global)<-NULL
factores <- do.call(rbind,lapply(c("patron","tamano","core_real","error_pct","n_grupos"),function(f)
  do.call(rbind,lapply(split(detalle,list(detalle$variante,detalle[[f]]),drop=TRUE),function(x)
    cbind(factor=f,nivel=as.character(x[[f]][1]),resumir(x))))))
contrastar <- function(a,b,efecto) {
  x <- detalle[detalle$variante==a,]; y <- detalle[detalle$variante==b,]
  y <- y[match(x$id_sim,y$id_sim),];stopifnot(identical(x$id_sim,y$id_sim))
  d <- y$acierto-x$acierto
  se <- sqrt(sum(vapply(split(d,x$id_escenario),function(z)length(z)*var(z),numeric(1))))/length(d)
  cambio <- (x$definido!=y$definido) | (x$definido & y$definido & (x$P!=y$P|x$Q!=y$Q|x$R!=y$R))
  data.frame(efecto=efecto,desde=a,hacia=b,N=length(d),cambios=sum(cambio),
    gana_acierto=sum(d==1),pierde_acierto=sum(d==-1),diferencia_pp=100*mean(d),
    MCSE_pp=100*se,IC95_inf_pp=100*(mean(d)-1.96*se),IC95_sup_pp=100*(mean(d)+1.96*se),
    cambio_media_S=mean(y$S-x$S,na.rm=TRUE),cambio_Fit_pp=mean(y$Fit_pct-x$Fit_pct,na.rm=TRUE))
}
pares <- list()
for(f in c("inicial","umbral","cociente")) for(j in seq_len(nrow(diseno))) {
  target <- diseno[j,]; base <- target[[f]]
  if((f=="cociente" && base!="todos") || (f!="cociente" && base)) next
  target[[f]] <- if(f=="cociente") "maximos" else TRUE
  k <- which(diseno$inicial==target$inicial & diseno$umbral==target$umbral & diseno$cociente==target$cociente)
  pares[[length(pares)+1L]] <- contrastar(diseno$variante[j],diseno$variante[k],f)
}
pares[[length(pares)+1L]] <- contrastar("S_sin_espaciado","S_con_espaciado","espaciado")
pares[[length(pares)+1L]] <- contrastar("Referencia_D_V1","Referencia_D_V2","V2 menos V1 DIFFIT")
pares[[length(pares)+1L]] <- contrastar("Referencia_S_V1","Referencia_S_V2","V2 menos V1 scree")
contrastes <- do.call(rbind,pares)
frecuencias <- aggregate(list(N=rep(1L,nrow(detalle))),detalle[,c("variante","P","Q","R")],sum)
auditoria <- data.frame(comprobacion=colnames(validacion),discrepancias=colSums(validacion))
metodo <- data.frame(Concepto=c("Objetivo","Datos","Inicial","Umbral","Cocientes","Scree","Desempate DIFFIT",
 "Sin seleccion","Incertidumbre","Lectura de diferencias","Promedios","Limite","Referencias","Excel"),
 Detalle=c("Cambiar un componente por contraste manteniendo los mismos ajustes por replica.",
 paste(nrow(meta),"replicas;",length(unique(meta$id_escenario)),"escenarios. No se reestima Tucker3."),
 "I1 permite g1=FIT(Smin), respecto de cero; I0 excluye solo ese candidato.",
 "U1 exige g > 100/(smax-3), con smax=min(I,JK)+min(J,IK)+min(K,IJ). U0 no filtra.",
 "todos: g_i/max(g_j,j>i). maximos: registros maximos hacia la derecha y cociente entre registros consecutivos. Denominador abs<1e-12 da Inf.",
 "Sin espaciado: abs(diff(diff(FIT))). Con espaciado: abs(2*diff(diff(FIT)/diff(S))/(h_anterior+h_siguiente)). Ambos excluyen extremos.",
 "Mayor cociente, mayor incremento, menor S. Para tripletas: mayor FIT, menor SCE, luego P,Q,R crecientes. Scree: menor S entre empates.",
 "NA explicito si no hay candidato. Acierto se cuenta como 0; las demas medias excluyen NA. Referencias conservan reglas originales.",
 "D_r=acierto_hacia-acierto_desde en la misma replica. MCSE=sqrt(sum_h(n_h*var(D_h)))/N. IC normal puntual: diferencia +/-1.96 MCSE; no simultaneo.",
 "Todas las diferencias son hacia menos desde; tasas y ajustes en escala 0-100; pp significa puntos porcentuales.",
 "media_S: promedio de P+Q+R; error_L1: suma de errores absolutos por modo. Sub/sobre comparan S con S_real. Promedios globales ponderados por replicas.",
 "Diagnostico del diseno existente, no validacion independiente ni prueba de superioridad universal. No seleccionar reglas por mayor acierto en estos mismos datos.",
 "Referencia_V1/V2 ejecutan exclusivamente las funciones de sus archivos. La comprobacion control registra diferencias debidas a convenciones numericas o desempates.",
 "Resultados calculados en R, no se recalculan al editar Excel. Inf se conserva como texto y NA como celda vacia."))
salidas <- list(Resumen=global,Contrastes=contrastes,Por_factores=factores,Dimensiones=frecuencias,
               Por_replica=detalle,Curvas=curvas,Variantes=diseno,Auditoria=auditoria,Metodo=metodo)
saveRDS(salidas,file.path(SALIDA,"diagnostico_completo.rds"))
write.csv(global,file.path(SALIDA,"resumen.csv"),row.names=FALSE)
write.csv(contrastes,file.path(SALIDA,"contrastes.csv"),row.names=FALSE)
wb <- openxlsx::createWorkbook(); openxlsx::modifyBaseFont(wb,fontName="Arial",fontSize=10)
header <- openxlsx::createStyle(fgFill="#243D59",fontColour="#FFFFFF",textDecoration="bold",wrapText=TRUE,valign="center")
numero <- openxlsx::createStyle(numFmt="0.00")
for(nombre in names(salidas)) {
  x <- salidas[[nombre]]
  infinitos <- list()
  for(j in seq_along(x)) if(is.numeric(x[[j]]) && any(is.infinite(x[[j]]))) {
    filas <- which(is.infinite(x[[j]]))
    infinitos[[as.character(j)]] <- list(filas=filas,texto=as.character(x[[j]][filas]))
    x[[j]][filas] <- NA_real_
  }
  openxlsx::addWorksheet(wb,nombre,gridLines=FALSE)
  openxlsx::writeData(wb,nombre,x,headerStyle=header,withFilter=TRUE)
  for(j in names(infinitos)) for(k in seq_along(infinitos[[j]]$filas))
    openxlsx::writeData(wb,nombre,infinitos[[j]]$texto[k],startRow=infinitos[[j]]$filas[k]+1L,
                        startCol=as.integer(j),colNames=FALSE)
  openxlsx::setRowHeights(wb,nombre,1,42)
  openxlsx::setColWidths(wb,nombre,seq_along(x),pmax(15,pmin(30,nchar(names(x))*1.1)))
  openxlsx::setColWidths(wb,nombre,1,30)
  if("variante" %in% names(x)) openxlsx::setColWidths(wb,nombre,match("variante",names(x)),30)
  openxlsx::freezePane(wb,nombre,firstRow=TRUE)
  for(j in which(vapply(x,is.numeric,logical(1)))) if(any(x[[j]]%%1!=0,na.rm=TRUE))
    openxlsx::addStyle(wb,nombre,numero,rows=2:(nrow(x)+1),cols=j,gridExpand=TRUE)
  if(nombre=="Contrastes") openxlsx::setColWidths(wb,nombre,2:3,29)
  if(nombre=="Metodo") {
    openxlsx::setColWidths(wb,nombre,2,110)
    openxlsx::addStyle(wb,nombre,openxlsx::createStyle(wrapText=TRUE,valign="top"),rows=2:(nrow(x)+1),cols=2,gridExpand=TRUE)
    openxlsx::setRowHeights(wb,nombre,2:(nrow(x)+1),48)
  }
}
destino <- file.path(SALIDA,"Comparaciones_controladas_Tucker3.xlsx")
# Esta version de openxlsx conserva relaciones vacias de dibujos aunque no se
# crean dibujos ni comentarios. Quitarlas evita referencias a partes inexistentes.
wb$worksheets_rels <- lapply(wb$worksheets_rels,function(x)
  x[!grepl("/drawing\"|/vmlDrawing\"",x)])
openxlsx::saveWorkbook(wb,destino,overwrite=TRUE)
releer <- openxlsx::read.xlsx(destino,sheet="Resumen")
stopifnot(isTRUE(all.equal(releer$acierto_pct,global$acierto_pct,tolerance=1e-12)))
fuentes <- c(CODIGO_V1,CODIGO_V2,file.path(ENTRADA,"resultados_numericos.rds"))
write.csv(data.frame(archivo=c("code/simulation_V1.R","code/simulation_V2.R","data/simulation/resultados_numericos.rds"),md5=unname(tools::md5sum(fuentes))),file.path(SALIDA,"fuentes_md5.csv"),row.names=FALSE)
capture.output(sessionInfo(),file=file.path(SALIDA,"sessionInfo.txt"))
print(global); print(contrastes); print(auditoria)
message("Excel guardado: ",destino)
