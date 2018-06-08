setwd("D:\\PSK Sem4\\PSK R")
x=scan() 

moving.avrg<-function(num, x, n){ 
  nx<-length(x) 
  y<-numeric(nx) 
  
  switch(num, 
         satu= { 
           for(i in n:nx){ 
             y[i]=mean(x[(i - n+1):i])} 
           return(data.frame(cbind(x,SMA=y))) 
         }, 
         dua={ 
           for(i in n:nx){ 
             y[i]=2*sum((n:1)*x[i:(i-n+1)])/(n*(n+1))}   
           return(data.frame(cbind(x, WMA=y))) 
         }, 
        tiga= { 
        SMA<-numeric(nx) 
        for(i in n:n){ 
        SMA[i]=mean(x[i-n+1:i])} 
        a=2/(n+1) 

        y[n-1]=SMA[n] 
        for(i in n:nx){ 
        y[i]=a*(x[i]-y[i-1])+y[i-1]} 
        y[n-1]=0 
        return(data.frame(cbind(x, EMA=y))) 
         
         } 
  ) 
}