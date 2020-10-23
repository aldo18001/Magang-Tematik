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
    l[j] <- (1+i)^(-i/12)
  }
  diskonto <- l
  #diskonto <- (1+i) ^ - (0:(k*12)-1)
  peluang = c(1,cumprod(ph[(usia*12+2):((usia*12)+k*12)]))
  rm(JK)
  rm(ph)
  sum(diskonto*peluang)
}

annuity(30,5)
insurance <- function(i,k,usia,b){
  JK <- readline(prompt="Masukkan jenis kelamin L/P: ")
  if(JK=="L"){
    ph <- x$p
  } else{
    ph <- y$p
  }
  diskonto <- (1+i) ^ - (1:k)
  peluangh <- c(1,cumprod(ph[(usia+2):(usia+k)]))
  qx = 1 - ph
  peluangd <- qx[(usia+1):(usia+k)]
  a <- sum(diskonto*peluangh*peluangd)
  a*b
}

premi <- function(i,k,usia,b){
  result = as/an
  result
}

premik <- function(i,k,usia,b,adm){
  
  P = (insurance(i,k,usia,b)+(adm*annuity(i,k,usia)))/(annuity(i,k,usia)-sum(e[1:k])) 

  P = (insurance(i,k,usia,b)+(420000*annuity(i,k,usia)))/(annuity(i,k,usia)-sum(e[1:k]))

  P
}

