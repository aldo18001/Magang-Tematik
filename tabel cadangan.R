tabel <- function(usia,k,JK){
  
  r <- data.frame(cadangan_bulan_ke = character(),
                  premi_bersih = numeric(),
                  premi_kotor = numeric(),
                  jumlah_cadangan = numeric())                  

  for(i in c(0:((k*12)-1))){
    r[(i+1),1] <- i
  }  
  
  for(j in c(0:((k*12)-1))){
    r[(j+1),4] <- creserve(usia,k,j,JK)
  }
  
  for(j in c(0:((k*12)-1))){
    r[(j+1),2] <- premi(usia,k,JK)
  }
  
  for(j in c(0:((k*12)-1))){
    r[(j+1),3] <- premik(usia,k,JK)
  }
  r
}
