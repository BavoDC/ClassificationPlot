print.ClassificationPlot <- function(x, ...){
  cat("\n#~~~~~~~~~~~~~~~~~#\n")
  cat("# Results model 1 #\n")
  cat("#~~~~~~~~~~~~~~~~~#\n\n")
  AUC = round(x$Model1$AUC, 3)
  AUC = paste(AUC[1], " (",AUC[3],"-",AUC[4],")",sep="")
  cat(paste(" - AUC (95% CI)=", AUC,"\n\n"))
  print(x$Model1$Summary)
  if(any(names(x)=="Model2")){
    cat("\n#~~~~~~~~~~~~~~~~~#\n")
    cat("# Results model 2 #\n")
    cat("#~~~~~~~~~~~~~~~~~#\n\n")
    AUC = round(x$Model2$AUC, 3)
    AUC = paste(AUC[1], " (",AUC[3],"-",AUC[4],")",sep="")
    cat(paste(" - AUC (95% CI)=", AUC,"\n\n"))
    print(x$Model2$Summary)
  }
}
