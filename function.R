annuity <- function(i,k,usia){
  JK <- readline(prompt="Masukkan jenis kelamin L/P: ")
  if(JK=="L"){
    ph <- x$p
  } else{
    ph <- y$p
  }
  diskonto <- (1+i) ^ - (0:(k-1))
  peluang = c(1,cumprod(ph[(usia+2):(usia+k)]))
  rm(JK)
  rm(ph)
  sum(diskonto*peluang)
}

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

premik <- function(i,k,usia,b){
  
  P = (insurance(i,k,usia,b)+(420000*annuity(i,k,usia)))/(annuity(i,k,usia)-sum(e[1:k]))
  P
}