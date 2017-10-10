roc.se=function(area.U, n.pos, n.neg){
  out <- sqrt((area.U*(1 - area.U)+(n.pos - 1)*(area.U/(2 - area.U)
                                                - area.U^2) + (n.neg - 1)*((2*area.U^2)/(1 + area.U)
                                                                           - area.U^2))/(n.pos*n.neg))
  return(out)
}
