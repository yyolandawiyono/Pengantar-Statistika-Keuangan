setwd("D:\\PSK Sem4\\PSK R")
amortisasi<-function(PV=NA,i,m,t,k=FALSE){
  j=i/m;
  v=1/(1+j);
  n=t*m;
  
  if(nargs()==5 & is.na(PV)==TRUE){
    PV=k*(1-v^n)/j
  }
  
  k=PV/((1-v^n)/j);
  b=PV;
  
  output=array(dim=c(n,5))
  
  output[1,1]=1
  output[1,2]=k
  output[1,3]=j*b
  output[1,4]=k-output[1,3]
  output[1,5]=b-output[1,4]
  
  for(i in 2:n){
    output[i,1]=i
    output[i,2]=round(k,digits = 3)
    output[i,3]=round(j*output[i-1,5],digits = 3)
    output[i,4]=round(output[i,2]-output[i,3],digits = 3)
    output[i,5]=round(output[i-1,5]-output[i,4],digits = 3)
  }
  
  period=c(output[,1])
  payment=c(output[,2])
  interest=c(output[,3])
  principal=c(output[,4])
  balance=c(output[,5])
  SCHEDULE=data.frame(period,payment,interest,principal,balance)
  SCHEDULE
}

yolan=amortisasi(20000,0.189,1,12)
yolan
jumlah_bunga=sum(yolan$interest)
jumlah_bunga
saldo=yolan$balance
saldo
plot(saldo,type="l",col="red",lwd=2, xlab="period",ylab="balance")
