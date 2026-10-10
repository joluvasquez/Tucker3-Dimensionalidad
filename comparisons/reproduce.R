# Read-only verification of archived fits. Does not run ALS or change source outputs.
args<-commandArgs(trailingOnly=TRUE)
root<-normalizePath(if(length(args))args[1] else '.',mustWork=TRUE)
out<-file.path(root,'verification');dir.create(out,showWarnings=FALSE)
sink(file.path(out,'audit_results.txt'),split=TRUE)
mcdir<-file.path(root,'data/simulation')
o<-readRDS(file.path(mcdir,'resultados_numericos.rds'));d<-o$resultados
old<-readRDS(file.path(root,'data/V1_comparison.rds'))$Por_replica
load_functions<-function(p){e<-new.env(); for(x in parse(p,encoding='UTF-8')) if(is.call(x)&&identical(x[[1]],as.name('<-'))&&length(x)==3&&is.call(x[[3]])&&identical(x[[3]][[1]],as.name('function')))eval(x,e);e}
v1<-load_functions(file.path(root,'code/simulation_V1.R'))
v2<-load_functions(file.path(root,'code/simulation_V2.R'))
independent<-function(z,I,J,K){
 z<-z[order(z$S,z$SCE,-z$FIT,z$P,z$Q,z$R),]; b<-z[!duplicated(z$S),]; m<-nrow(b)
 a<-c(NA,head(b$SCE,-1)-tail(b$SCE,-1));rat<-rep(NA_real_,m)
 for(i in 2:(m-1)){den<-max(a[(i+1):m]);if(is.finite(den)&&den>0)rat[i]<-a[i]/den}
 i1<-if(all(is.na(rat)))which.min(b$SCE) else which.max(rat)
 g<-c(b$FIT[1],diff(b$FIT)); ids<-which(vapply(1:m,function(i)i==m||g[i]>=max(g[(i+1):m]),logical(1)))
 q<-g[head(ids,-1)]/g[tail(ids,-1)];q[abs(g[tail(ids,-1)])<1e-12]<-Inf
 el<-head(ids,-1);tau<-100/(min(I,J*K)+min(J,I*K)+min(K,I*J)-3)
 ok<-g[el]>tau;el<-el[ok];q<-q[ok];i2<-if(length(el))el[order(-q,-g[el],b$S[el])][1] else NA_integer_
 h<-diff(b$S);s1<-which.max(abs(diff(diff(b$FIT))))+1
 s2<-which.max(abs(2*diff(-diff(b$SCE)/h)/(head(h,-1)+tail(h,-1))))+1
 ans<-b[c(i1,i2,s1,s2),c('P','Q','R','S','FIT')];rownames(ans)<-c('D1','D2','S1','S2');ans
}
stopifnot(nrow(d)==5400,length(unique(d$id_escenario))==108,all(table(d$id_escenario)==50))
records<-vector('list',nrow(d)); mismatches<-c(savedV1=0,savedV2=0,code=0); nfit<-0;nonconv<-0
for(i in seq_len(nrow(d))){
 m<-d[i,];z<-readRDS(file.path(mcdir,'modelos_por_replica',sprintf('SPM_%05d.rds',m$id_sim)))
 a<-independent(z,m$I,m$J,m$K);nfit<-nfit+nrow(z);nonconv<-nonconv+sum(!z$convergio)
 for(crit in c('diffit','scree')){
  short<-if(crit=='diffit')'D' else 'S'
  saved<-as.numeric(m[paste0(c('P','Q','R'),'_',crit)])
  mismatches['savedV2']<-mismatches['savedV2']+!all(as.numeric(a[paste0(short,2),1:3])==saved)
  prev<-old[old$id_sim==m$id_sim & old$Version=='Original_aplicacion' & old$Criterio==crit,]
  mismatches['savedV1']<-mismatches['savedV1']+!all(as.numeric(a[paste0(short,1),1:3])==as.numeric(prev[c('P','Q','R')]))
  for(v in 1:2){env<-if(v==1)v1 else v2; b<-if(crit=='diffit')env$seleccionar_diffit(z,m$I,m$J,m$K)$modelo_final else {s<-env$seleccionar_multiway_scree(env$obtener_scree_multiway(z))$s_sel;tmp<-env$obtener_mejor_modelo_por_S(z);tmp[tmp$S==s,]};mismatches['code']<-mismatches['code']+!all(as.numeric(a[paste0(short,v),1:3])==as.numeric(b[c('P','Q','R')]))}
 }
 truth<-as.numeric(m[c('P_real','Q_real','R_real')]);errors<-sweep(as.matrix(a[,1:3]),2,truth)
 records[[i]]<-data.frame(id_sim=m$id_sim,id_escenario=m$id_escenario,variant=rownames(a),a,patron=m$patron,core_real=m$core_real,tamano=m$tamano,n_grupos=m$n_grupos,error_pct=m$error_pct,correct=rowSums(abs(errors))==0,L1=rowSums(abs(errors)),L2=rowSums(errors^2),bias=a$S-sum(truth),mode1=errors[,1]==0,mode2=errors[,2]==0,mode3=errors[,3]==0,row.names=NULL)
 if(i%%1000==0)cat('Checked',i,'arrays\n')
}
r<-do.call(rbind,records);write.csv(r,file.path(out,'verified_replicates.csv'),row.names=FALSE)
cat('MISMATCHES\n');print(mismatches);stopifnot(all(mismatches==0))
cat('Candidate fits:',nfit,'Nonconvergent retained candidates:',nonconv,'\n')
summary<-function(x)data.frame(N=nrow(x),recovery=100*mean(x$correct),S=mean(x$S),FIT=mean(x$FIT),L1=mean(x$L1),L2=mean(x$L2),bias=mean(x$bias),under=100*mean(x$bias<0),over=100*mean(x$bias>0),mode1=100*mean(x$mode1),mode2=100*mean(x$mode2),mode3=100*mean(x$mode3),MCSE=100*sqrt(sum(vapply(split(x$correct,x$id_escenario),function(v)length(v)*var(v),numeric(1))))/nrow(x))
for(f in c('global','patron','core_real','tamano','n_grupos','error_pct')){
 gr<-if(f=='global')split(r,r$variant) else split(r,list(r$variant,r[[f]]),drop=TRUE)
 tab<-do.call(rbind,lapply(gr,summary));write.csv(tab,file.path(out,paste0('summary_',f,'.csv')));cat('\n',f,'\n');print(tab)
}
pair<-function(a,b,pattern=NULL){x<-r[r$variant==a,];y<-r[r$variant==b,];if(!is.null(pattern)){x<-x[x$patron==pattern,];y<-y[y$patron==pattern,]};stopifnot(identical(x$id_sim,y$id_sim));v<-as.integer(x$correct)-as.integer(y$correct);se<-sqrt(sum(vapply(split(v,x$id_escenario),function(t)length(t)*var(t),numeric(1))))/length(v);data.frame(contrast=paste(a,'minus',b),pattern=if(is.null(pattern))'all' else pattern,delta=100*mean(v),MCSE=100*se,lower=100*(mean(v)-1.96*se),upper=100*(mean(v)+1.96*se),changed=sum(x$P!=y$P|x$Q!=y$Q|x$R!=y$R),gain=sum(v==1),loss=sum(v== -1))}
contrasts<-rbind(pair('D1','S1'),pair('D2','S2'),pair('D2','D1'),pair('S2','S1'),pair('D2','S2','agrupado'),pair('D2','S2','aleatorio'));cat('\nPAIRED\n');print(contrasts);write.csv(contrasts,file.path(out,'paired.csv'),row.names=FALSE)
macro<-readRDS(file.path(root,'data/empirical_reference.rds'));cat('\nEMPIRICAL V2 CANDIDATES\n');print(independent(macro$SPM,24,12,18));cat('Years',range(macro$years),'\n')
initial<-read.csv(file.path(root,'data/empirical_V1_candidates.csv'));initial$S<-initial$P+initial$Q+initial$R;initial$FIT<-initial$Ajuste_pct
cat('\nEMPIRICAL V1 CANDIDATES\n');print(independent(initial,24,12,18));keys<-function(x)paste(x$P,x$Q,x$R);ix<-match(keys(initial),keys(macro$SPM));cat('Common fit maximum absolute difference',max(abs(initial$FIT-macro$SPM$FIT[ix])),'\n')
write.csv(independent(macro$SPM,24,12,18),file.path(out,'empirical_selections.csv'))
cat('\nArchive hashes\n');hash_paths <- c('code/simulation_V2.R','code/simulation_V1.R','data/simulation/resultados_numericos.rds'); hashes <- tools::md5sum(file.path(root,hash_paths)); names(hashes) <- hash_paths; print(hashes)
capture.output(sessionInfo(),file=file.path(out,'audit_sessionInfo.txt'));sink()
