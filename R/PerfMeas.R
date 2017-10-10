PerfMeas <- function(ResultsRoc, cutoffs){
  ft = ResultsRoc
  n  = ft$n
  Sens = ft$out[1,"Sens"]
  Spec = ft$out[1,"Spec"]
  PPV  = ft$out[1,"PPV"]
  Prev = unname((PPV*(1-Spec))/(Sens*(1-PPV) + PPV*(1 - Spec))) # Calculates prevalence for Std NB
  TP = TN = FP = FN = SE = SP = lambda = NB_model  = NB_nomodel= NNT = plr = nlr = ppv = npv = NULL
  for (i in 1:length(cutoffs)) {
    idx    = sum(ft$dscore<=cutoffs[i])
    TP[i]  = ft$nTP[idx]
    TN[i]  = ft$nTN[idx]
    FP[i]  = ft$nFP[idx]
    FN[i]  = ft$nFN[idx]
    SE[i]  = ft$SE[idx]
    SP[i]  = ft$SP[idx]
    lambda[i]     = cutoffs[i]/(1-cutoffs[i])
    NB_model[i]   = TP[i]/n - FP[i]/n*lambda[i]
    NB_nomodel[i] = max(TP[1]/n - FP[1]/n*lambda[i],0)
    NNT[i] = (TP[i]+FP[i])/TP[i]
    plr[i] = SE[i]/(1-SP[i])
    nlr[i] = (1 - SE[i])/SP[i]
    ppv[i]  = TP[i]/(TP[i]+FP[i])
    npv[i]  = TN[i]/(TN[i]+FN[i])
  }

  NNT[0.5] = NB_nomodel[0.5] = NB_model[0.5] = NA
  NB_diff  = NB_model - NB_nomodel
  StdNB  = NB_model/Prev

  netTP_model   = TP - FP*lambda
  netTP_nomodel = TP[1] - FP[1]*lambda
  NNT_model     = 1/NB_model
  NNT_nomodel   = 1/NB_nomodel
  NNT_extra     = 1/(NB_model - NB_nomodel)
  Results       = cbind.data.frame(Cutoff = cutoffs, TP, TN, FP, FN, SE, SP, NB_model, StdNB, NB_diff,
                                   netTP = netTP_model, NNT = NNT_model, NNT_extra)
  return(list(Summary = Results,
              AddInfo = cbind.data.frame(netTP_model, netTP_nomodel, NNT_model, NNT_nomodel, NNT_extra,
                                         plr, nlr, ppv, npv)))
}
