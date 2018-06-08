setwd("D:\\PSK Sem4\\PSK R")
obligasi<-function(f,r,i,t,m){
n = t*m;
rb =  r/m;
ib = i/m;
vb = 1/(1+ib);
anb = (1 - (vb^n))/ib;
{
P = (f*rb*anb)+(f*(vb^n))
cat("Harga Obligasi = ",P)}
}

##Harga diskon##
#at par : ketika r = i
#at premium : ketika r > i
#at discount : ketika r < i 