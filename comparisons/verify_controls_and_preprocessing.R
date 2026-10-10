root<-normalizePath('.',mustWork=TRUE)
out<-file.path(root,'verification')
sink(file.path(out,'audit_extra.txt'),split=TRUE)
o<-readRDS(file.path(root,'data/controlled_results.rds'))
curves<-split(o$Curvas,o$Curvas$id_sim); failures<-0L
for(z in curves){
 n<-nrow(z);g<-c(z$FIT[1],diff(z$FIT));s<-z$S
 for(initial in c(FALSE,TRUE))for(threshold in c(FALSE,TRUE))for(kind in c('todos','maximos')){
  idx<-if(kind=='todos')seq_len(n) else which(vapply(seq_len(n),function(i)i==n||g[i]>=max(g[(i+1):n]),logical(1)))
  scores<-rep(NA_real_,n)
  for(i in head(idx,-1)){den<-max(g[(i+1):n]);scores[i]<-if(abs(den)<1e-12)Inf else g[i]/den}
  ok<-which(!is.na(scores)&(initial|seq_len(n)>1)&(!threshold|g>z$umbral_pct))
  sel<-if(length(ok))ok[order(-scores[ok],-g[ok],s[ok])][1] else NA_integer_
  name<-paste0('D_I',as.integer(initial),'_U',as.integer(threshold),'_',kind)
  saved<-o$Por_replica[o$Por_replica$id_sim==z$id_sim[1]&o$Por_replica$variante==name,]
  failures<-failures+if(is.na(sel)) saved$definido else !all(z[sel,c('P','Q','R')]==saved[,c('P','Q','R')])
 }
}
cat('Controlled comparison mismatches:',failures,'\n');stopifnot(failures==0)
original<-file.path(root,'code/empirical_original.R')
txt<-readLines(original,encoding='UTF-8',warn=FALSE)
excel_sheets<-openxlsx::getSheetNames
read_excel<-function(path,sheet,col_names=TRUE)openxlsx::read.xlsx(path,sheet=sheet,colNames=col_names,check.names=FALSE)
ruta_archivo<-file.path(root,'data/observed_data.xlsx')
eval(parse(text=txt[grep('^fila_nombres <-',txt):(grep('^# 6. MODELOS CANDIDATOS',txt)-2)]))
app<-readRDS(file.path(root,'data/empirical_reference.rds'))
X<-array(X1,dim=c(I,J,K));cat('Preprocessing max error',max(abs(X-app$X)),'dimensions',dim(X),'\n');stopifnot(max(abs(X-app$X))<1e-15)
old<-read.csv(file.path(root,'data/empirical_V1_candidates.csv'));old$S<-old$P+old$Q+old$R;old$FIT<-old$Ajuste_pct
write.csv(old,file.path(out,'empirical_V1_candidates.csv'),row.names=FALSE)
mc<-readRDS(file.path(root,'data/simulation/resultados_numericos.rds'))
cat('Noise maximum discrepancy',max(abs(mc$generacion$error_pct_obtenido-mc$generacion$error_pct_deseado)),'\n')
stopifnot(all(mc$generacion$rango_P==mc$generacion$P_real),all(mc$generacion$rango_Q==mc$generacion$Q_real),all(mc$generacion$rango_R==mc$generacion$R_real))
cat('All generated multilinear ranks agree\n')
r<-read.csv(file.path(out,'verified_replicates.csv'))
freq<-aggregate(list(N=rep(1,nrow(r))),r[c('variant','core_real','P','Q','R')],sum)
modal<-do.call(rbind,lapply(split(freq,list(freq$variant,freq$core_real),drop=TRUE),function(x)x[which.max(x$N),]));print(modal);write.csv(modal,file.path(out,'modal_triplets.csv'),row.names=FALSE)
cat('Smallest model V2 by true core\n');print(table(r$core_real[r$variant=='D2'&r$S==3]))
sink()
