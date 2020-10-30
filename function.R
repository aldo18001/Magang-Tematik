annuity <- function(usia, k, discount_rate = 0.055, inflation =  0.025){
  usia <- usia-20
  i <- (discount_rate-inflation)
  JK <- readline(prompt="Masukkan jenis kelamin L/P: ")
  if(JK=="L"){
    ph <- x$p
  } else{
    ph <- y$p
  }
  l <- array()
  for(j in c(0:(k*12))){
    l[j] <- (1+i)^((-j/12)-1)
  }
  diskonto <- l
  #diskonto <- (1+i) ^ - (0:(k*12)-1)
  peluang = c(1,cumprod(ph[(usia*12+2):((usia*12)+k*12)]))
  sum(diskonto*peluang)
}

xpns <- function(usia, k, discount_rate = 0.055, inflation =  0.025){
  usia <- usia-20
  i <- (discount_rate-inflation)
  JK <- readline(prompt="Masukkan jenis kelamin L/P: ")
  if(JK=="L"){
    ph <- x$p
  } else{
    ph <- y$p
  }
  l <- array()
  for(j in c(0:(k*12))){
    l[j] <- (1+i)^(-j/12)
  }
  diskonto <- l
  #diskonto <- (1+i) ^ - (0:(k*12)-1)
  peluang = c(1,cumprod(ph[(usia*12+2):((usia*12)+k*12)]))
  ex <- e[0:k*12]
  result = sum(peluang * ex * diskonto)
  result
}



insurance <- function(usia, k, discount_rate = 0.055, inflation =  0.025, benefit = 2000000000){
  usia <- usia-20
  i <- (discount_rate-inflation)
  JK <- readline(prompt="Masukkan jenis kelamin L/P: ")
  if(JK=="L"){
    ph <- x$p
    q <- x$q
  } else{
    ph <- y$p
    q <- y$q
  }
  l <- array()
  for(j in c(0:(k*12))){
    l[j] <- (1+i)^(-j/12)
  }
  diskonto <- l
  
  #q bersyarat
  peluangh <- c(cumprod(ph[(usia*12+1):(usia*12+k*12)]))
  peluangd <- q[(usia*12+1):(usia*12+k*12)]
  
  a <- sum(diskonto*peluangh*peluangd)
  result <- a*benefit
  result
}

premi <- function(usia,k,discount_rate = 0.055, inflation =  0.025, benefit = 2000000000 ){
  result = insurance(usia,k)/annuity(usia,k)
  result
}

#premi kotor
premik <- function(usia,k,discount_rate = 0.055, inflation =  0.025, benefit = 2000000000, adm = 420000){
  P = (insurance(usia,k)+(adm*annuity(usia,k)))/(annuity(usia,k)-xpns(usia,k)) 
  P
}
  
#Reserve but no expense

reserve <- function(usia,k,bulan_ke,discount_rate = 0.055, inflation = 0.025, benefit = 2000000000, adm = 420000){
  usia1 <- usia-20
  i <- (discount_rate-inflation)
  k1 <- (k*12)-bulan_ke
  usia2 <- (usia1*12)+bulan_ke
  
  JK <- readline(prompt="Masukkan jenis kelamin L/P: ")
  if(JK=="L"){
    ph <- x$p
    q <- x$q
  } else{
    ph <- y$p
    q <- y$q
  }
  
  #diskonto <- (1+i) ^ - (0:(k*12)-1) (j+1)
  l <- array()
  for(z in c(0:(k1))){
    l[z] <- (1+i)^((-z/12)-1)
  }
  diskonto <- l
  
  #peluang hidup
  peluang = c(1,cumprod(ph[(usia2+2):((usia2)+k1)]))
  
  #peluang mati
  peluangd <- q[(usia2+1):(usia2+k1)]
  
  Outflow = sum(l*peluang*peluangd)*benefit
  
  #diskonto <- (1+i) ^ - (0:(k*12)-1) (j)
  l2 <- array()
  for(z in c(0:k1)){
    l2[z] <- (1+i)^(-z/12)
  }
  diskonto <- l2
  
  premium <- premik(usia,k)
  
  Inflow <- sum(l2*peluang)*premium
  
  Result <- Outflow - Inflow
  Result
  
}

#Reserve /w expense

creserve <- function(usia,k,bulan_ke,discount_rate = 0.055, inflation = 0.025, benefit = 2000000000, adm = 420000){
  usia1 <- usia-20
  i <- (discount_rate-inflation)
  k1 <- (k*12)-bulan_ke
  usia2 <- (usia1*12)+bulan_ke
  
  JK <- readline(prompt="Masukkan jenis kelamin L/P: ")
  if(JK=="L"){
    ph <- x$p
    q <- x$q
  } else{
    ph <- y$p
    q <- y$q
  }
  
  #diskonto <- (1+i) ^ - (0:(k*12)-1) (j+1)
  l <- array()
  for(z in c(0:(k1))){
    l[z] <- (1+i)^((-z/12)-1)
  }
  diskonto <- l
  
  #peluang hidup
  peluang = c(1,cumprod(ph[(usia2+2):((usia2)+k1)]))
  
  #peluang mati
  peluangd <- q[(usia2+1):(usia2+k1)]
  
  
  
  
  
  #diskonto <- (1+i) ^ - (0:(k*12)-1) (j)
  l2 <- array()
  for(z in c(0:k1)){
    l2[z] <- (1+i)^(-z/12)
  }
  diskonto <- l2
  
  
  e1 <- (e[(bulan_ke+1):(k*12)]*premik(usia,k)) + adm
  exp <- sum(e1*l2*peluang)
  
  premium <- premik(usia,k)
  
  Outflow = insurance(usia,k) + exp
   
  Inflow <- sum(l2*peluang)*premium
  
  Result <- Outflow - Inflow
  Result
  
}