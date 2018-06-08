setwd("D:\\PSK Sem4\\PSK R")
valueobligasi<-function(f,r,i,ttm,m){
  t = ceiling((ttm*m)+1)
  b = ttm*m - (floor(ttm*m))
  k = 1-b
  rb =  r/m
  ib = i/m
  vb = 1/(1+ib)
  anb = (1 - (vb^t))/ib
  Bt = (f*rb*anb)+(f*(vb^t))
  #f = nilai akumulasi
  #t = tingkat kupon 
  #i = tingkat yield
  #ttm = time to maturity
  #m = banyak pemberian bunga dalam 1 tahun 
  #t = tahun
  #Bm = harga obligasi pemilik baru
  #frk = harga obligasi pemilik lama
  
  
  #theoritical method
  Bf1 = Bt*(1+ib)^k
  frk1 = f*rb*((1+ib)^k-1)/ib
  Bm1 = Bf1 - frk1
  cat("Harga Obligasi Theoritical Method = ",Bm1,"\n")
  
  #practical method  Bf2 = Bt*(1+k*ib)
  frk2 = k*f*rb
  Bm2 = Bf2 - frk2
  cat("Harga Obligasi Practical Method = ",Bm2,"\n")
  
  #semi theoritical method
  Bf3 = Bt*(1+ib)^k
  frk3 = k*f*rb
  Bm3 = Bf3 - frk3 
  cat("Harga Obligasi Semi Theoritical Method = ",Bm3,"\n")
}
