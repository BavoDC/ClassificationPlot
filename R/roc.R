########################
## Function: ROC.R    ##
########################
## I: resp  (px1) --- 1 or 0
##    score (px1) --- large is positive
##======================
## O: distinct score (kx1)
##  : omr (kx1)
##  : sensitivity (kx1)
##  : specificity (kx1)
##  : ppv (kx1)
##  : npv (kx1)
##  : ROC-AUC (1x1)
########################
roc <- function(resp, score, data, weight=NULL, ConfLevel = 0.95, digits=3){

  Argz  = as.list(match.call())[-1]
  resp  = eval(Argz$resp, data)
  score = eval(Argz$score, data)

  alpha = 1 - ConfLevel
  conf  = ConfLevel

  nn    = length(resp)
  if(is.null(weight)) weight = rep(1,nn)
  rn    = rep(1,nn)

  wk    = c(score, seq(0, 1, by=0.005))
  cc    = unique(sort(wk))
  s     = length(cc)

  ##--- ROC --##
  omr = sens = spec = ppv = npv = TP = FP = FN = TN = nTP = nFP = nFN = nTN = nTPFP = nTNFN = rep(0,s)
  plr = nlr = NULL
  sens.ci = spec.ci = ppv.ci = npv.ci = rep("",s)


  ##--- ROC --##
  for (i in 1:s) {
    pred = as.numeric(score >= cc[i])
    TP[i]   = (   resp  *    pred  * weight) %*% rn
    FP[i]   = ((1-resp) *    pred  * weight) %*% rn
    FN[i]   = (   resp  * (1-pred) * weight) %*% rn
    TN[i]   = ((1-resp) * (1-pred) * weight) %*% rn

    omr[i]  = (abs(resp-pred) * weight) %*% rn/nn
    sens[i] = TP[i]/(TP[i]+FN[i])
    spec[i] = TN[i]/(TN[i]+FP[i])
    ppv[i]  = TP[i]/(TP[i]+FP[i])
    npv[i]  = TN[i]/(TN[i]+FN[i])

    nTP[i]  = TP[i]
    nFP[i]  = FP[i]
    nFN[i]  = FN[i]
    nTN[i]  = TN[i]

    nTPFP[i] = TP[i]+FP[i]
    nTNFN[i] = TN[i]+FN[i]


    ##--- Confidence Interval --##
    zk = qnorm(1-alpha/2)
    sens.ci[i] = binci.desmon(TP[i], TP[i]+FN[i], digits=digits, conf=conf)$ci
    spec.ci[i] = binci.desmon(TN[i], TN[i]+FP[i], digits=digits, conf=conf)$ci
    ppv.ci[i]  = binci.desmon(TP[i], TP[i]+FP[i], digits=digits, conf=conf)$ci
    npv.ci[i]  = binci.desmon(TN[i], TN[i]+FN[i], digits=digits, conf=conf)$ci

  }



  n.pos = (resp * weight) %*% rn
  n.neg = ((1-resp)* weight) %*% rn

  #===================================
  # summary measures
  #===================================
  AUC   = rocauc(sens, spec)  #---ROC-AUC
  AUCSE = roc.se(AUC, n.pos, n.neg)  #---ROC-AUC (SE)
  ACC = 1-min(omr)          #---max accuracy
  YDN = max(sens+spec-1)    #---max yoden index (SE+SP-1)
  CO1 = min(cc[which(omr==min(omr))])
  #---cutoff value -- max(accuracy)
  CO2 = min(cc[which(sens+spec-1==YDN)])
  #---cutoff value -- max(accuracy)
  SE1 = sens[cc==CO1]  #---given max(acc)
  SP1 = spec[cc==CO1]  #---given max(acc)
  PP1 = ppv[cc==CO1]   #---given max(acc)
  NP1 = npv[cc==CO1]   #---given max(acc)
  SE2 = sens[cc==CO2]  #---given max(yoden)
  SP2 = spec[cc==CO2]  #---given max(yoden)
  PP2 = ppv[cc==CO2]   #---given max(yoden)
  NP2 = npv[cc==CO2]   #---given max(yoden)

  SE3 = 0             #---given SP
  PP3 = 0             #---given SP
  NP3 = 0             #---given SP

  SP4 = 0             #---given SE
  PP4 = 0             #---given SE
  NP4 = 0             #---given SE

  pred = as.numeric(score >= CO1)
  TP1  = (resp * pred * weight) %*% rn
  FP1  = ((1-resp) *    pred  * weight) %*% rn
  FN1  = (resp  * (1-pred) * weight) %*% rn
  TN1  = ((1-resp) * (1-pred) * weight) %*% rn

  pred = as.numeric(score >= CO2)
  TP2  = (resp * pred  * weight) %*% rn
  FP2  = ((1-resp) * pred  * weight) %*% rn
  FN2  = (resp * (1-pred) * weight) %*% rn
  TN2  = ((1-resp) * (1-pred) * weight) %*% rn


  ACC1 = (TP1 + TN1)/sum(rn)
  ACC2 = (TP2 + TN2)/sum(rn)
  YDN1 = SE1 + SP1 - 1
  YDN2 = SE2 + SP2 - 1


  out = as.matrix(t(c(AUC, AUCSE, CO1, ACC, SE1, SP1, PP1, NP1, TP1, FP1, FN1, TN1, CO2, YDN, SE2,
                    SP2, PP2, NP2, TP2, FP2, FN2, TN2, ACC1, YDN1, ACC2, YDN2)))
  colnames(out)=c("AUC","AUC(SE)","Cutoff","Accuracy","Sens","Spec","PPV","NPV","TP","FP","FN","TN",
                  "Cutoff","Yoden","Sens","Spec","PPV","NPV", "TP","FP","FN","TN","Acc1","Youden1","Acc2","Youden2")

  out.auc=as.matrix(t(c(AUC, AUCSE, AUC-1.96*AUCSE,AUC+1.96*AUCSE)))
  colnames(out.auc)=c("AUC","SE","Low95","Upp95")

  Z=list()
  Z$out=out
  Z$SE=sens
  Z$SP=spec
  Z$PPV=ppv
  Z$NPV=npv
  Z$dscore=cc
  Z$OMR=omr
  Z$AUC=out.auc



  Z$nTP=nTP
  Z$nFP=nFP
  Z$nFN=nFN
  Z$nTN=nTN

  Z$nTPFP=nTPFP
  Z$nTNFN=nTNFN

  Z$SE.ci=sens.ci
  Z$SP.ci=spec.ci
  Z$PPV.ci=ppv.ci
  Z$NPV.ci=npv.ci

  Z$n = nn

  return(Z)

}
