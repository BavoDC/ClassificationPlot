rocauc<-function(se, sp){
  wse<-sort(c(se, 0, 1))
  wsp<-sort(c(sp, 0, 1), decreasing=T)
  cbind(wse, wsp)

  x<-1-wsp
  y<-wse

  m<-length(wse)
  area.int<-rep(0, m-1)
  for (i in 1:(m-1)){
    area.int[i]<- (y[i+1] + y[i]) * (x[i+1] - x[i]) /2
  }
  area<-sum(area.int)

  return(area)
}
