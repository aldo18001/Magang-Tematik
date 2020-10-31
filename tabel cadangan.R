tabel <- function(usia,k,JK){
  
  r <- data.frame(cadangan_bulan_ke = character(),
                  jumlah = numeric())

  for(i in c(0:((k*12)-1))){
    r[(i+1),1] <- i
  }  
  
  for(j in c(0:((k*12)-1))){
    r[(j+1),2] <- creserve(usia,k,j,JK)
  }
  
  r
}
