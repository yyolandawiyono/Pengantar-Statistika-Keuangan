#estimasi volatilitas historis

x = scan()

vola<- function(x,n,tau){
  m<-length(x)
  y<-array(NA,dim=c(m,2))
  
  for(i in 2:m){
    y[i, 1]=log(x[i]/x[i-1])}
    
  Rt.bar=mean(y[,1], na.rm = TRUE)
    y[,2]= (y[,1]- Rt.bar)^2 
    
  jumlah=sum(y[,2], na.rm = TRUE)
  sigma=sqrt(1/(n-1)*jumlah)
  volatilitas=sigma*sqrt(tau)
  volatilitas
}
vola(x,251,252)
