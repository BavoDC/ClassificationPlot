#' Classification plot
#'
#' This function can be used to visualize the performance measure of a model or two competing models. An object is also
#' returned containing the performance measures of the model(s).
#'
#' @param model1 Variable with the risks of the (baseline) model.
#' @param model2 Variable with the risks of the competing model.
#' @param outcome Response variable.
#' @param data Dataframe in which the predicted risks are to be found
#' @param cutoffs A vector containing the cutoffs for which the performance measures need to be calculated.
#' @param col1 Color for the lines of the TPR model(s).
#' @param col2 Color for the lines of the FPR of the model(s).
#' @param lwd Linewidth of the lines in the plot.
#' @param TreatAll Logical, indicates if treat all has to be shown on the plot. Default is \code{TRUE}.
#' @param colTreatAll Color of treat all.
#' @param TreatNone Logical, indicates if treat none has to be shown on the plot. Default is \code{TRUE}.
#' @param colTreatNone Color of treat none.
#' @param SNBpl Logical, indicates if SNB has to be shown on the plot. Default is \code{TRUE}.
#' @param colSNB Color of SNB.
#' @param axes Axes command of \code{\link{plot}}, default is \code{TRUE}. If set to \code{FALSE}, only the y- and x-axis
#' will be shown on the plot.
#' @param ShowAUC Logical, indicates if the AUC has to be shown on the plot. Default is \code{TRUE}.
#' @param AUCcoord Vector of length two with the coordinates of the AUCs on the plot. \code{\link{locator}} can also be used.
#' @param LegCoord Vector of length two with the coordinates of the legend on the plot. \code{\link{locator}} can also
#'  be used.
#' @param y.intersp Character interspecing factor for vertical distance.
#' @param RiskSet Indicates if performance measures have to be shown (see UtilityMeasures for further specification) below
#' the plot. Specify \code{'model1'} for the performance measures of model 1, \code{'model2'}
#' for those of model 2 and \code{'both'} if the performance measures for both models have to be shown..
#' \code{'none'} suppresses the printing. Default is \code{'model1'}.
#' @param cex.leg Size of the legend.
#' @param cex.auc Size of the AUCs shown on the plot.
#' @param GraphSettings List with graphical settings. See \code{\link{par}}.
#' @param PrintMessages Logical, indicates whether messages have to be printed while the function is calculating the
#' performance measures and preparing the plot. Default is \code{TRUE}.
#' @param UtilityMeasures Indicates which performance measures have to be shown for each of the cutoffs. Specify
#' \code{'default'} for the true and false positive rate (TPR and FPR, respectively) and
#' \code{'accuracy'} to additionally show the positive likelihood ratio (PLR), negative likelihood ratio (NLR),
#'  positive predictive value (PPV) and negative predictive value (NPV).
#' \code{'Utility'} shows the TPR, FPR, standardized net benefit (SNB), net TP per 100 and NNT for 1 TP if the
#' predicted risks of one model are given. In case of 2 models, the TPR and FPR for both models are given
#' as well as the difference in SNB and the difference in net TP per 100. Only possible for the default cutoffs.
#' Default is \code{'proportions'}.
#' @param loc Specifies the x-coordinates for the titles of the RiskSet.
#' @param LabelsModels The labels for the model(s) when \code{Riskset!='none'}.
#' @param ylab Label for the y-axis.
#' @param ... Arguments to be passed to \code{\link{plot}}, see \code{\link{par}}.
#'
#' @return The classification plot is plotted and an object containing the performance measures of the model(s) is returned.
#'
#' @references Vickers AJ, Elkin EB. Decision Curve Analysis: A Novel Method for Evaluating Prediction Models.
#' \emph{Medical Decision Making} 2006, 26(6): 565-574
#' @references Van Calster B, Vickers A, Pencina M, Baker S, Timmerman D, Steyerberg EW.
#' Evaluation of Markers and Risk Prediction Models: Overview of Relationships between NRI and Decision-Analytic
#'  easures. \emph{Medical Decision Making} 2013, 33(4): 490-501.
#' @references Vickers AJ, Van Calster B, Steyerberg EW. Net benefit approaches to the evaluation
#' of prediction models, molecular markers and diagnostic tests. \emph{British Medical Journal} 2016, 352
#'
#' @examples
#' #---------#
#' # 1 model #
#' #---------#
#'
#' # simulated data
#' X      = replicate(4, rnorm(5e2))
#' p0true = binomial()$linkinv(cbind(1, X)%*%c(0.1, 0.5, 1.2, -0.75, 0.8))
#' y      = rbinom(5e2, 1, p0true)
#' Df     = data.frame(y,X)
#'
#' # fit logistic model
#' FitLog = glm(y~., Df, family=binomial)
#' Pred   = binomial()$linkinv(cbind(1, X)%*%coef(FitLog))
#' Df2    = cbind.data.frame(Pred = Pred, Outcome = y)
#'
#' # Classification plot
#' ClassificationPlot(Pred, outcome = Outcome, data=Df2)
#'
#' #----------#
#' # 2 models #
#' #----------#
#'
#' # Fit second model
#' FitLog2 = glm(y~., Df[,1:3], family=binomial)
#' Pred2   = binomial()$linkinv(cbind(1, X[,1:2])%*%coef(FitLog2))
#' Df3    = cbind.data.frame(Model1 = Pred, Model2 = Pred2, Outcome = y)
#'
#' # Classification plot
#' ClassificationPlot(Model1, Model2, Outcome, Df3)
#' ClassificationPlot(Model1, Model2, Outcome, Df3, RiskSet = "both")
#' ClassificationPlot(Model1, Model2, Outcome, Df3, RiskSet = "both", UtilityMeasures = "utility")

ClassificationPlot <- function(model1, model2, outcome, data, cutoffs = seq(0, 0.9, by = 0.1),
                               col1 = "darkgreen", col2 = "red",
                               lwd = 3, TreatAll = T, colTreatAll = "grey", TreatNone = T,
                               colTreatNone = "black",
                               SNBpl = T, colSNB = "blue",
                               axes = T, ShowAUC = T, AUCcoord = c(0.625, 0.95),
                               LegCoord = c(0.75, 1.05),
                               y.intersp = 0.75, RiskSet = c("model1", "model2", "both","none"),
                               cex.leg=0.75, cex.auc = 0.85,
                               GraphSettings = NULL, PrintMessages = T,
                               UtilityMeasures = c("default", "accuracy", "utility"),
                               loc = -0.2,
                               LabelsModels = c("Model 1", "Model 2"),
                               ylab = "Proportion",
                               ...){
  Argz = as.list(match.call())[-1]
  RiskSet = match.arg(RiskSet)
  UtilityMeasures = match.arg(UtilityMeasures)
  if(!is.data.frame(data)) stop("Has to be of type dataframe.")
  if(missing(model1) & !missing(model2)) stop("Please specify model 1.")
  if(min(cutoffs)<0 | max(cutoffs)>1) stop("Cutoffs < 0 or > 1 are not possible.")
  if(!is.character(LabelsModels)) stop("Only character strings are allowed.")
  if(length(LabelsModels)>2) stop("Only one or two character strings are permitted.")
  if(RiskSet=="both" & UtilityMeasures=="accuracy") stop("The accuracy options is not possible for both models.")
  if(!"character" %in% class(ylab)) stop("Has to be of type character.")

  n  = nrow(data)
  Df = data
  Df$m1    = eval(Argz$model1, data)
  if(any(Df$m1<=0) | any(Df$m1>=1)) stop("Predicted probabilities have to be given for model1 (0 < risk < 1).")
  if(!missing(model2)) Df$m2    = eval(Argz$model2, data)
  if(!missing(model2))
    if(any(Df$m2<=0) | any(Df$m2>=1)) stop("Predicted probabilities have to be given for model1 (0 < risk < 1).")
  if(missing(model2) & RiskSet=="model2") stop("RiskSet cannot be calculated when model2 is not given.")
  Df$Outc  = eval(Argz$outcome, data)
  if(!all(Df$outcome%in%0:1)) stop("The response variable can only contain 0 or 1.")
  Prev     = sum(Df$Outc)/nrow(Df)
  op = par("mfrow", "oma", "mai")
  if(RiskSet!="none"){
    if(is.null(GraphSettings)){
      par(mfrow=c(1,1), oma=c(if(RiskSet!="both") 5 else 10,5,0,0),
          mgp = c(2.5, 1, 0),
          mai=c(0.5,0.7,0.2,0.2))
    }else{
      if(!is.list(GraphSettings)) stop("Has to be of type list.")
      NamesPar = names(par())
      if(!all(names(GraphSettings)%in%NamesPar)){
        NoGraphSett = names(GraphSettings)[!names(GraphSettings)%in%NamesPar]
        cat(paste("\n\nThe following are not recognized as graphical parameters:\n",
                  paste(" - ",NoGraphSett, collapse="\n", sep=""), sep=""))
        stop()
        op = par()
        par(GraphSettings)
      }
    }
  }

  if(PrintMessages) cat("\n\nComputing performance measures for the model(s).\n\n")
  ft  = roc(Outc, m1, Df)
  if(!missing(model2)) ft1 = roc(Outc, m2, Df)

  NNT = TP = TN = FP = FN = SE = SP = rep(0,length(cutoffs))
  lambda = NB_model = NB_nomodel = rep(0,length(cutoffs))

  if(PrintMessages) cat("\n\nComputing performance measures for each of the cutoffs.\n\n")
  Results = list()
  Results$Model1 = PerfMeas(ft, cutoffs)
  Results$Model1$AUC     = ft$AUC
  if(RiskSet%in%c("model1", "both")) list2env(as.list(cbind(Results$Model1$Summary[,,drop=F],
                                               Results$Model1$AddInfo[,,drop=F])),
                                 envir = environment())
  if(!missing(model2)){
    Results$Model2 = PerfMeas(ft1, cutoffs)
    Results$Model2$AUC     = ft1$AUC
    if(RiskSet=="model2") list2env(as.list(cbind(Results$Model1$Summary[,,drop=F],
                                                 Results$Model1$AddInfo[,,drop=F])
                                           ), envir = environment())
  }

  netSE = SNB = ft$SE-ft$nFP/(ft$nTP+ft$nFN)*(ft$dscore/(1-ft$dscore))
  # netSE  = ifelse(SNB<0,NA,SNB)

  if(!missing(model2)){
    netSE1 = SNB1 = ft1$SE-ft1$nFP/(ft1$nTP+ft1$nFN)*(ft1$dscore/(1-ft1$dscore))
    # netSE1 = ifelse(SNB1<0,NA,SNB1)
  }

  lambda2 = ft$dscore/(1-ft$dscore)
  P = (ft$nTP+ft$nFN)/(ft$nTP+ft$nFN+ft$nFP+ft$nTN)

  SNBTA = SNBtreat_all = 1-(1/P)*lambda2*(1-P)
  # SNBTA = ifelse(SNBtreat_all<0,NA,SNBtreat_all)

  NNT_extra[0] = NNT_nomodel[0.5] = NNT_model[0.5] = netTP_nomodel[0.5] = netTP_model[0.5] = NA
  # netTP_nomodel[netTP_nomodel<0] = 0

  idx = ft$dscore<=1

  if(PrintMessages) cat("\n\nPreparing the plot\n\n")
  plot(ft$dscore, ft$SE, col=col1, type="l",xlab="", ylab = ylab, lab=c(10, 10, 7), axes=axes, las=1, lwd=lwd, ...)
  lines(ft$dscore, 1-ft$SP, col=col2, lwd=lwd)
  if(SNBpl) lines(ft$dscore, netSE, col=colSNB, lty=1, lwd=lwd)
  if(TreatAll) lines(ft$dscore, SNBTA, col=colTreatAll, lty=4, lwd=lwd)
  if(TreatNone) abline(h=0, lty=2, col=colTreatNone)

  if(!axes){
    axis(side=1, pos=0, at=seq(0,1, 0.1), labels=seq(0,1,0.1))
    axis(side=2, pos=0, at=seq(0, 1, 0.2), las=1)
  }
  if(!missing(model2)){
    lines(ft1$dscore, ft1$SE, col=col1, lty=2, lwd=lwd)
    lines(ft1$dscore, 1-ft1$SP, col=col2, lty=2, lwd=lwd)
    if(SNBpl) lines(ft1$dscore, netSE1, col=colSNB, lty=2, lwd=lwd)
    if(ShowAUC)
      text(AUCcoord[1], AUCcoord[2], labels=paste("AUC ", LabelsModels[1]," = ",round(ft$AUC[1],digits = 3)," \n",
                                            paste("AUC ", LabelsModels[2], " = ",round(ft1$AUC[1],digits = 3)), sep=""), pos=4,
                     cex = cex.auc)
    LegText = c(paste("TPR", LabelsModels),paste("FPR", LabelsModels))
    LegCol  = c(rep(col1, 2), rep(col2, 2))
    LegLty  = c(rep(c(1,2), 2))


  }else{
    LegText = c("TPR", "FPR")
    LegCol  = c(col1, col2)
    LegLty  = c(rep(1,2))
    if(ShowAUC) text(AUCcoord[1], AUCcoord[2], labels = paste("AUC ", LabelsModels[1]," = ",
                                                            round(ft$AUC[1],digits=2)), cex = cex.auc)
  }
  if(SNBpl){
    LegText =
      if(missing(model2)) c(LegText, "SNB")
        else c(LegText, paste("SNB", LabelsModels))
    LegCol  = c(LegCol, rep(colSNB, if(missing(model2)) 1 else 2))
    LegLty  =
      if(missing(model2)) c(LegLty, 1)
        else c(LegLty, 1:2)
  }
  if(TreatAll){
    LegText = c(LegText, "SNB treat all")
    LegCol  = c(LegCol, colTreatAll)
    LegLty  = c(LegLty, 2)
  }
  if(TreatNone){
    LegText = c(LegText, "SNB treat none")
    LegCol  = c(LegCol, colTreatNone)
    LegLty  = c(LegLty, 2)
  }

  mtext(side = 1, at = 0.5,  line = 2, text = "Risk Threshold", col = "black", cex=1)

  legend(LegCoord[1], LegCoord[2],LegText,
         lwd=2, lty=LegLty,col=LegCol,
         y.intersp=y.intersp, box.col="transparent", cex=cex.leg, bg="transparent")

  #--- risk set --
  if(RiskSet!="none"){
    CEX=0.7 ; ATT=seq(0,0.9,0.1)
    TmpTxt = if(RiskSet%in%c("model1", "both")) LabelsModels[1] else if(RiskSet=="model2") LabelsModels[2]

    mtext(side = 1, at = loc, line = 3, text = TmpTxt,  col = "black", cex=CEX, font = 2)
    mtext(side = 1, at = loc, line = 4, text = "True positive rate",  col = "black", cex=CEX)
    mtext(side = 1, at = loc, line = 5, text = "False positive rate",  col = "black", cex=CEX)
    mtext(side = 1, at = ATT, line = 4, text = round(SE,digits=2), col = "black", cex=CEX)
    mtext(side = 1, at = ATT, line = 5, text = round(1-SP,digits=2), col = "black", cex=CEX)
    # Utility
    if(UtilityMeasures=="utility" & RiskSet!="both"){
      mtext(side = 1, at = loc, line = 6, text = "Std Net benefit",  col = "black", cex=CEX)
      mtext(side = 1, at = loc, line = 7, text = "net TP per 100",  col = "black", cex=CEX)
      mtext(side = 1, at = loc, line = 8, text = "NNT for 1 TP",  col = "black", cex=CEX)

      mtext(side = 1, at = ATT, line = 6, text = round(StdNB,digits=2), col = "black", cex=CEX)
      mtext(side = 1, at = ATT, line = 7, text = round(NB_model * 100,digits=1), col = "black", cex=CEX)
      mtext(side = 1, at = ATT, line = 8, text = round(NNT_extra,digits=0), col = "black", cex=CEX)
    }
    if(RiskSet=="both"){
      # Default model2
      mtext(side = 1, at = loc, line = 6, text = LabelsModels[2],  col = "black", cex=CEX, font = 2)
      mtext(side = 1, at = loc, line = 7, text = "True positive rate",  col = "black", cex=CEX)
      mtext(side = 1, at = loc, line = 8, text = "False positive rate",  col = "black", cex=CEX)
      SE2 = Results$Model2$Summary$SE
      SP2 = Results$Model2$Summary$SP
      mtext(side = 1, at = ATT, line = 7, text = round(SE2,digits=2), col = "black", cex=CEX)
      mtext(side = 1, at = ATT, line = 8, text = round(1-SP2,digits=2), col = "black", cex=CEX)
    if(UtilityMeasures=="utility"){
      mtext(side = 1, at = loc, line = 9, text = "Comparison models",  col = "black", cex=CEX, font = 2)
      mtext(side = 1, at = loc, line = 10, text = expression(paste(Delta, " Std NB")), col = "black", cex=CEX)
      mtext(side = 1, at = loc, line = 11, text = expression(paste(Delta, " net TP per 100")),
            col = "black", cex=CEX)
      DiffSNB   = Results$Model2$Summary$StdNB - StdNB
      DiffNetTP = (Results$Model2$Summary$NB_model - NB_model)*100
      mtext(side = 1, at = ATT, line = 10, text = round(DiffSNB,digits=2), col = "black", cex=CEX)
      mtext(side = 1, at = ATT, line = 11, text = round(DiffNetTP,digits=0), col = "black", cex=CEX)
    }
    }
    if(UtilityMeasures=="accuracy"){
      mtext(side = 1, at = loc, line = 6, text = "Positive LR",  col = "black", cex=CEX)
      mtext(side = 1, at = loc, line = 7, text = "Negative LR",  col = "black", cex=CEX)
      mtext(side = 1, at = loc, line = 8, text = "PPV",  col = "black", cex=CEX)
      mtext(side = 1, at = loc, line = 9, text = "NPV",  col = "black", cex=CEX)
      mtext(side = 1, at = ATT, line = 6, text = round(plr,digits=2), col = "black", cex=CEX)
      mtext(side = 1, at = ATT, line = 7, text = round(nlr,digits=2), col = "black", cex=CEX)
      mtext(side = 1, at = ATT, line = 8, text = round(ppv,digits=2), col = "black", cex=CEX)
      mtext(side = 1, at = ATT, line = 9, text = round(npv,digits=2), col = "black", cex=CEX)
    }
  }
  par(op)
  Results = structure(Results, class = "ClassificationPlot")
  return(Results)
}
