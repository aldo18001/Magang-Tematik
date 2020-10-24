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
    l[j] <- (1+i)^(-j/12)
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
  ex <- e[0:60]
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
    l[j] <- (1+i)^(-i/12)
  }
  diskonto <- l
  peluangh <- c(1,cumprod(ph[(usia*12+2):(usia*12+k*12)]))
  peluangd <- q[(usia*12+1):(usia*12+k*12)]
  a <- sum(diskonto*peluangh*peluangd)
  result <- a*benefit
  result
}

premi <- function(usia,k,discount_rate = 0.055, inflation =  0.025, benefit = 2000000000 ){
  result = insurance(usia,k)/annuity(usia,k)
  result
}

premik <- function(usia,k,discount_rate = 0.055, inflation =  0.025, benefit = 2000000000, adm = 420000){
  P = (insurance(usia,k)+(adm*annuity(usia,k)))/(annuity(usia,k)-xpns(usia,k)) 
  P
}
  

