setwd("D:/PSK Sem4/PSK R")
x=scan()

an.tek<- function(num,x,n){
  m<-length(x)
  y<-numeric(m)
  
  switch(num,
      satu = {
        for(t in n:m){
          y[t]=mean(x[(t-n+1):t])}
          return(data.frame(cbind(x,SMA=y)))
        },
        dua = {
          SMA <-numeric(m)
          for(t in n:n){
            SMA[t]<-mean(x[(t-n+1):t])}
            
        a<-2/(n+1)
        y[n-1]=SMA[n]
        for(t in n:m){
          y[t]=a*(x[t]-y[t-1])+y[t-1]}
          y[n-1]=0
          return(data.frame(cbind(x,EMA=y)))
        },
          tiga = {
            for(t in n:m){
              y[t]=2*sum(n:1)*x[t:(t-n+1)]/(n*(n+1))}
            return(data.frame(cbind(x,WMA=y)))
            }
          )
}
      