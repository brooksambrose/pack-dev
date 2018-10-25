#' Imported JSTOR PDF to Clean
#'
#' @param imp
#' @param lev
#' @param test.size
#' @param hold.prop
#' @param jav.mem
#' @param cvf
#' @param seed
#' @param out
#' @param hand
#' @param learners
#' @param benefit
#'
#' @return
#' @export
#' @import data.table
#' @import SuperLearner
#' @importFrom stringr str_count
#' @examples
imp2cln.f<-function(
  imp,lev='par',test.size=1000,hold.prop=.2,jav.mem="-Xmx5g",cvf=10,benefit=1
  ,seed,out,hand
  ,learners=ec("SL.mean,SL.glmnet,SL.randomForest,SL.nnet,SL.xgboost,SL.svm,SL.bartMachine")){

  k<-c('doc',lev)
  nm<-c('doc','pag','par')
  setkeyv(imp,k)
  aud<-list()
  for(i in nm) aud[[i]]<-data.table(
    level=i
    ,N=imp[,.N,by=eval(nm[c(1,which(nm==i))] %>% unique)][,.N]
  )
  aud<-rbindlist(aud)

  if(missing(hand)){
    if(missing(out)) {mo<-T;out<-paste('d','d',paste0('cln',Sys.time() %>% as.integer,'.txt'),sep=.Platform$file.sep);file.create(out)}

    cln<-imp[,.(txt=txt %>% paste0(collapse='')),by=k]
    vw<-'[aeiou]'
    cn<-setdiff(letters,ec(vw,'')) %>% paste0(.,collapse='') %>% paste0('[',.,']',collapse='')
    cln[,`:=`(
      vwl=str_count(txt,vw)
      ,cns=str_count(txt,cn)
      ,A2Z=str_count(txt,'[A-Z]')
      ,dig=str_count(txt,'[0-9]')
      ,wsp=str_count(txt,'[[:space:]]')
      ,pun=str_count(txt,'[!,;.\"\']')
      ,rem=str_count(txt,'[^a-zA-Z0-9[:space:]!,;.\"\']')
      ,txt=substr(gsub(' +',' ',txt),0,500)
      ,truth=NA_real_
    )]
    cln[,parp:=par/max(par),by=doc]
    cln[,tot:=vwl+cns+A2Z+dig+wsp+pun+rem]
    cln[,`:=`(vwlp=vwl/tot,cnsp=cns/tot,A2Zp=A2Z/tot,digp=dig/tot,wspp=wsp/tot,punp=pun/tot,remp=rem/tot)]

    if(missing(seed)) {rnorm(1);seed<-.Random.seed %>% sample(1)}
    set.seed(seed)

    w<-cln[,sample(1:.N,as.integer(test.size/(1-hold.prop)))]

    fwrite(cln[w,.SD,.SDcols=c('truth','txt',k)],file = out,sep = '\t',quote = ifelse(cln[,any(grepl('\t',txt))],'auto',F))

    setattr(cln,'seed',seed)
    setattr(cln,'hold.prop',hold.prop)
    setattr(cln,'test.size',test.size)
    setattr(cln,'lev',lev)
    setattr(cln,'out',out)
    setattr(cln,'w',w)
    clm<-ec('doc,par,parp,vwl,vwlp,cns,cnsp,A2Z,A2Zp,dig,digp,wsp,wspp,pun,punp,rem,remp,tot,truth,txt')
    setcolorder(cln,clm)

    save(cln,file=sub('txt','RData',out))

    cat('Training database for manual coding at:',out,sep='\n')
    return(out)

  } else {
    load(sub('txt','RData',hand))
    cln[,`:=`(txt=NULL,truth=NULL)]
    trn<-fread(hand,quote='')
    trn<-merge(cln,trn)
    set.seed(attr(cln,'seed'))
    w<-trn[,sample(1:.N,attr(cln,'test.size'))]
    hld<-trn[!w]
    trn<-trn[w][,txt:=NULL]

    # Fit Models ------------------------------------------------------------
    options(java.parameters = jav.mem)

    #https://cran.r-project.org/web/packages/SuperLearner/vignettes/Guide-to-SuperLearner.html#fit-ensemble-with-external-cross-validation
    set.seed(attr(cln,'seed'))
    t0<-Sys.time()
    cv_sl <- CV.SuperLearner(
      Y = trn[,truth]
      ,X = trn[,!c(k,'truth'),with=F] %>% as.data.frame
      ,family = binomial()
      ,V = cvf
      ,verbose=T
      ,SL.library = learners
    )
    t1<-Sys.time()
    t1-t0

    cvmod<-cv_sl %>% plot %>% myth
    #cvmod

    review_weights <- function(cv_sl) {
      meta_weights = coef(cv_sl)
      means = colMeans(meta_weights)
      sds = apply(meta_weights, MARGIN = 2,  FUN = sd)
      mins = apply(meta_weights, MARGIN = 2, FUN = min)
      maxs = apply(meta_weights, MARGIN = 2, FUN = max)
      # Combine the stats into a single matrix.
      sl_stats = cbind("mean(weight)" = means, "sd" = sds, "min" = mins, "max" = maxs)
      # Sort by decreasing mean weight.
      sl_stats[order(sl_stats[, 1], decreasing = T), ]
    }

    rw<-review_weights(cv_sl)
    print(rw, digits = 3)

    sl<-SuperLearner(
      Y = trn[,truth]
      ,X = trn[,!c(k,'truth'),with=F] %>% as.data.frame
      ,family = binomial()
      ,verbose=T
      ,SL.library = learners
    )

    pred <- predict(sl, hld[,!c(k,'truth','txt'),with=F] %>% as.data.frame, onlySL = T)
    #qplot(pred$pred[, 1],binwidth=1/10) %>% myth
    pbx <- myth(ggplot(data.frame(truth=ec('drop,keep')[hld$truth+1] %>% factor,guess=pred$pred),aes(truth,guess)) + geom_boxplot(outlier.shape=3))

    th<-ggplot_build(pbx)$data[[1]][c('ymin','ymax')] %>% unlist %>% `[`(3:2)
    #pbx<-pbx #+
      # geom_hline(yintercept=mean(th),color='darkgray') + annotate(geom='label',x=1.5,y=mean(th)*1.0,label=mean(th) %>% round(3),color='blue') +
      # geom_hline(yintercept=th[1],color='darkgray') + annotate(geom='label',x=1.5,y=th[1]*1.0,label=th[1] %>% round(3),color='blue') +
      # geom_hline(yintercept=th[2],color='darkgray') + annotate(geom='label',x=1.5,y=th[2]*1.0,label=th[2] %>% round(3),color='blue')

    #pbx

    if(diff(th)<0) warning('Bad separation between drop and keep. Predicted probability boxplots overlap in holdout set.')

    pred_rocr <- ROCR::prediction(pred$pred, hld[,truth])
    tfp <- ROCR::performance(pred_rocr,"tpr","fpr")
    ptfp <- myth(qplot(x=tfp@x.values[[1]],y=tfp@y.values[[1]],geom='step')) + xlab(tfp@x.name) + ylab(tfp@y.name)
    #ptfp

    auc <- ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
    #auc

    vp<-visreg::visreg(randomForest(factor(truth)~.,data=trn[,!(c(k)),with=F]),plot = F)
    #visreg:::plot.visreg(vp)

    prd <- predict(sl, cln[,!c(k),with=F] %>% as.data.frame, onlySL = T)

    ret<-cln[,.(doc,par,prd=prd$pred)] %>% setkeyv(k)

    rhld<-hld[,.(doc,par,truth=truth,prd=pred$pred)][,`:=`(lth=prd>th[1],sth=prd>mean(th),hth=prd>th[2])]
    shld<-rhld[,.(plotID=paste(doc,par),Observed=truth,Predicted=prd)]
    ths<-pbapply::pbreplicate(
      PresenceAbsence::optimal.thresholds(shld[sample(1:.N,replace = T)],opt.methods = 'Cost',FPC = 1,FNC = benefit)[,2]
      ,n=1000
    #  ,cl = ceiling(parallel::detectCores()/2)
    )
    pbx<- pbx + geom_boxplot(data=data.frame(truth='MaxPCC',guess=ths),notch=T) + annotate(geom='point',x=3,y=mean(ths),shape=18,color='blue',size=5)

    cfm<-rhld[,prop.table(table(truth,ec('–,+')[lth+1],deparse.level = 0))] %>% data.table(T)

    aud<-list()
    for(i in nm) aud[[i]]<-data.table(
      level=i
      ,N=imp[,.N,by=eval(nm[c(1,which(nm==i))] %>% unique)][,.N]
    )
    aud<-rbindlist(aud)

    thr<-c(mean=mean(thr),quantile(thr,p=c(.025,.25,.5,.75,.975)))
    ret[,mth:=prd>=thr['2.5%']]
    return(list(prd=ret,thr=thr,thr.boot.mpcc=ths,aud=aud,sl=sl,auc=auc,slcvwd=rw,p=list(box=pbx,tfp=ptfp,cv=cvmod,vr=vp)))
  }
}
